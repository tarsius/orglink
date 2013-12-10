;;; orglink.el --- use Org Mode links in other modes

;; Copyright (C) 2004-2013  Free Software Foundation, Inc.
;; Copyright (C) 2013  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20130501
;; Package-Requires: ((dash "1.3.2") (org "8.0"))
;; Homepage: http://github.com/tarsius/orglink
;; Keywords: hypertext

;; This file is not part of Org Mode.
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements support for some Org Mode link types in
;; other major modes.  Links can be opened and edited like in Org
;; Mode.

;; To use enable `global-orglink-mode' and customize
;; `orglink-activate-in-modes'.  Or use the buffer local
;; `orglink-mode'.  Do the latter now to linkify these examples:

;;   [[Code]]
;;   [[Code][start of code]]
;;   [[define-derived-mode orglink-mode][orglink-mode]]
;;   <mailto:jonas@bernoul.li>
;;   man:info
;;   <info:man>
;;   http://github.com/tarsius/orglink
;;   TODO support Emacs Lisp xref links like (in) `help-mode'.
;;   TODO support footnote.el links (only)

;; If you like this you might also like `orgstruct-mode', [[https://github.com/tj64/outshine][outshine]],
;; [[https://github.com/tarsius/hl-todo][hl-todo]], and [[https://github.com/tarsius/org-elisp-help][org-elisp-help]].

;;; Code:

(require 'dash)

;; Defer loading `org' to speedup Emacs startup when
;; `global-orglink-mode' is called in the init file.
(defvar org-descriptive-links)
(declare-function org-load-modules-maybe 'org)
(declare-function org-remove-from-invisibility-spec 'org-compat)
(declare-function org-set-local 'org-macs)
(declare-function org-unfontify-region 'org)

(defvar hl-todo-keyword-faces)
(defvar outline-minor-mode)

(defgroup orglink nil
  "Use Org Mode links in other modes."
  :prefix "orglink-"
  :group 'convenience)

(defcustom orglink-activate-links '(bracket angle plain)
  "Types of links that should be activated by `orglink-mode'.
This is a list of symbols, each leading to the activation of a
certain link type.

bracket  The [[link][description]] and [[link]] links.
angle    Links in angular brackets like <info:org>.
plain    Plain links in normal text like http://orgmode.org.

Changes to this variable only become effective after restarting
`orglink-mode', which has to be done separately in each buffer."
  :group 'orglink
  :safe (lambda (v)
          (and (listp v) (-all? 'symbolp v)))
  :type '(set :greedy t
              (const :tag "Double bracket links" bracket)
              (const :tag "Angular bracket links" angle)
              (const :tag "Plain text links" plain)))

(defcustom orglink-activate-in-modes '(emacs-lisp-mode)
  "Major modes in which `orglink-mode' should be activated.
This is used by `global-orglink-mode'.  Note that `orglink-mode'
is never activated in the *scratch* buffer, to avoid having to
load `org' at startup (because that would take a long time)."
  :group 'orglink
  :type '(repeat function))

(defcustom orglink-mode-lighter " OrgLink"
  "Mode lighter for Orglink Mode."
  :group 'orglink
  :type '(choice (const :tag "none" nil)
                 string))

(defvar orglink-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2]     'org-open-at-point-global)
    (define-key map [return]      'org-open-at-point-global)
    (define-key map [tab]         'org-next-link)
    (define-key map [backtab]     'org-previous-link)
    map)
  "Keymap used for `orglink-mode' link buttons.
The keymap stored in this variable is actually used by setting
the buffer local value of variable `org-mouse-map' to it's
value when `orglink-mode' is turned on.")

;;;###autoload
(define-minor-mode orglink-mode
  "Toggle display Org-mode links in other major modes.

On the links the following commands are available:

\\{orglink-mouse-map}"
  :lighter orglink-mode-lighter
  (when (derived-mode-p 'org-mode)
    (error "Orglink Mode doesn't make sense in Org Mode"))
  (cond (orglink-mode
         (require 'org)
         (org-load-modules-maybe)
         (add-hook 'org-open-link-functions
                   'orglink-heading-link-search nil t)
         (font-lock-add-keywords nil (orglink-font-lock-keywords) t)
         (org-set-local 'org-descriptive-links org-descriptive-links)
         (when org-descriptive-links
           (add-to-invisibility-spec '(org-link)))
         (org-set-local 'font-lock-unfontify-region-function
                        'orglink-unfontify-region)
         (org-set-local 'org-mouse-map orglink-mouse-map))
        (t
         (remove-hook 'org-open-link-functions
                      'orglink-heading-link-search t)
         (font-lock-remove-keywords nil (orglink-font-lock-keywords t))
         (remove-from-invisibility-spec '(org-link))
         (kill-local-variable 'org-descriptive-links)
         (kill-local-variable 'font-lock-unfontify-region-function)
         (kill-local-variable 'org-mouse-map)))
  (font-lock-fontify-buffer))

;;;###autoload
(define-globalized-minor-mode global-orglink-mode
  orglink-mode turn-on-orglink-mode-if-desired)

(defun turn-on-orglink-mode-if-desired ()
  (when (and (not (equal (buffer-name) "*scratch*"))
             (apply 'derived-mode-p orglink-activate-in-modes))
    (orglink-mode 1)))

(defun orglink-unfontify-region (beg end)
  (org-unfontify-region beg end)
  ;; The above does not remove the following properties.
  ;; TODO Should it? Should we? Ask on mailing-list.
  (remove-text-properties beg end '(help-echo t rear-nonsticky t)))

(defun orglink-font-lock-keywords (&optional all)
  `(,@(and (or all (memq 'bracket orglink-activate-links))
           '((org-activate-bracket-links (0 'org-link t))))
    ,@(and (or all (memq 'angle orglink-activate-links))
           '((org-activate-angle-links (0 'org-link t))))
    ,@(and (or all (memq 'plain orglink-activate-links))
           '((org-activate-plain-links (0 'org-link t))))))

(defun orglink-heading-link-search (s)
  (let (case-fold-search pos)
    (save-excursion
      (goto-char (point-min))
      (and (or outline-minor-mode
               (derived-mode-p 'emacs-lisp-mode))
           (re-search-forward
            (concat "^"
                    (if outline-minor-mode
                        outline-regexp
                      ";;;+ ")
                    (if (bound-and-true-p hl-todo-mode)
                        (regexp-opt (mapcar 'car hl-todo-keyword-faces))
                      "\\(?:\\sw+\\)")
                    "?" s)
            nil t)
           (setq pos (match-beginning 0))
           (goto-char pos)))))

(provide 'orglink)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; orglink.el ends here
