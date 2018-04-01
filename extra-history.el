;;; extra-history.el --- Conditional history lists for read-from-minibuffer, read-string and read-regexp.

;; Filename: extra-history.el
;; Description: Conditional history lists for read-from-minibuffer, read-string and read-regexp.
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2017, Joe Bloggs, all rites reversed.
;; Created: 2017-12-01 22:52:43
;; Version: 0.1
;; Last-Updated: 2017-12-01 22:52:43
;;           By: Joe Bloggs
;;     Update #: 1
;; URL: https://github.com/vapniks/extra-history
;; Keywords: convenience
;; Compatibility: GNU Emacs 25.2.1
;; Package-Requires: ((cl-lib "1.0"))
;;
;; Features that might be required by this library:
;;
;; cl-lib session
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1ArFina3Mi8UDghjarGqATeBgXRDWrsmzo
;;
;; Conditional history lists for read-from-minibuffer, read-string and read-regexp.
;; This is useful when you have some commands that use default history lists (`minibuffer-history' & `regexp-history'),
;; but you would prefer they use specialized history lists (e.g. for `nnmairix-search' search terms).
;; See the documentation of `extra-history-lists' for more details.
;;
;; Note: you can use `session-save-session' in session.el to save history lists between sessions.
;;;;;;;;

;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `extra-history-lists'
;;    Extra history lists for `read-string', `read-from-minibuffer', `read-regexp',
;;    `completing-read' and `completing-read-multiple'.
;;    default = nil

;;
;; All of the above can be customized by:
;;      M-x customize-group RET extra-history RET
;;

;;; Installation:
;;
;; Put extra-history.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'extra-history)

;;; History:

;;; Require
(require 'cl-lib)

;;; Code:

(defvar extra-history-templst nil
  "Variable used for temporary history lists defined in `extra-history-lists'.")

(defcustom extra-history-lists nil
  "Extra history lists for `read-string', `read-from-minibuffer' and `read-regexp'.
The aforementioned functions are advised to make use of this option if their history
argument is nil.
Each element of the alist is a pair (COND . HIST) where COND is an sexp, and HIST is 
a history list to use if COND evaluates to non-nil and no other history list was passed 
as an argument to the read function. COND may make use of `prompt' which is bound to
the value of the prompt argument to the read- function.
HIST can be either a symbol of a history list variable, or a list of strings, or a function
which returns a list of strings when called with no args. In the last 2 cases the list of
strings will be temporarily bound to `extra-history-templst'.

Note: you need to make sure that the history list symbols referred to by this option
have been defined (e.g. using `defvar' or `defcustom')."
  :type '(alist :key-type (sexp :tag "Condition")
		:value-type (choice (variable :tag "List variable")
				    (function :tag "Function returning a list")
				    (repeat (string :tag "String element")))))

(defun extra-history-get (histlist prompt &optional appendlst)
  "If HISTLIST is nil return a symbol to use as an argument for `read-from-minibuffer' or `read-string'.
Uses `extra-history-lists' option to choose which symbol to return.
PROMPT is the prompt used in the read- function, and APPENDLST is a list of extra items to append 
to `extra-history-templst', or the list from `extra-history-lists' if that is empty."
  (or histlist
      (let ((val (cdr (cl-find t extra-history-lists
			       :test (lambda (x y) (eval (car y)))))))
	(if (symbolp val)
	    (if (or (not val) (symbol-value val))
		val
	      (set val appendlst)
	      val)
	  (setq extra-history-templst (if (functionp val)
					  (or (funcall val) appendlst)
					(append val appendlst)))
	  'extra-history-templst))))

(defun extra-history-read-from-minibuffer-advice (args)
  "If the HISTORY arg is nil, obtain a value for it from the `extra-history-lists' option.
This function advises `read-from-minibuffer'."
  (list (nth 0 args) (nth 1 args) (nth 2 args) (nth 3 args)
	(extra-history-get (nth 4 args) (nth 0 args) minibuffer-history)
	(nth 5 args) (nth 6 args)))

(defun extra-history-read-string-advice (args)
  "If the HISTORY arg is nil, obtain a value for it from the `extra-history-lists' option.
This function advises `read-string'."
  (list (nth 0 args) (nth 1 args) 
	(extra-history-get (nth 2 args) (nth 0 args) minibuffer-history)
	(nth 3 args) (nth 4 args)))

(defun extra-history-completing-read-advice (args)
  "If the HISTORY arg is nil, obtain a value for it from the `extra-history-lists' option.
This function advises `completing-read'."
  (list (nth 0 args) (nth 1 args) (nth 2 args) (nth 3 args) (nth 4 args)
	(extra-history-get (nth 5 args) (nth 0 args) minibuffer-history)
	(nth 6 args) (nth 7 args)))

(defun extra-history-completing-read-multiple-advice (args)
  "Append hint about `crm-separator' to prompt.
This function advises `completing-read-multiple'"
  (list (concat (nth 0 args) (format " (separator=%s): " crm-separator))
	(nth 1 args) (nth 2 args) (nth 3 args) (nth 4 args)
	(nth 5 args) (nth 6 args) (nth 7 args)))

(defun extra-history-read-regexp-advice (args)
  "If the HISTORY arg is nil, obtain a value for it from the `extra-history-lists' option.
This function advises `read-regexp'."
  (list (nth 0 args) (nth 1 args)
	(extra-history-get (nth 2 args) (nth 0 args) regexp-history)))

(advice-add 'read-from-minibuffer :filter-args #'extra-history-read-from-minibuffer-advice)
(advice-add 'read-string :filter-args #'extra-history-read-string-advice)
(advice-add 'completing-read :filter-args #'extra-history-completing-read-advice)
(unless (not (featurep 'crm))
  (advice-add 'completing-read-multiple :filter-args #'extra-history-completing-read-advice)
  (advice-add 'completing-read-multiple :filter-args #'extra-history-completing-read-multiple-advice))
(advice-add 'read-regexp :filter-args #'extra-history-read-regexp-advice)
;;(advice-remove 'read-from-minibuffer #'extra-history-read-from-minibuffer-advice)
;;(advice-remove 'read-string #'extra-history-read-string-advice)
;;(advice-remove 'completing-read #'extra-history-completing-read-advice)
;;(advice-remove 'completing-read-multiple #'extra-history-completing-read-advice)
;;(advice-remove 'completing-read-multiple #'extra-history-completing-read-multiple-advice)
;;(advice-remove 'read-regexp #'extra-history-read-regexp-advice)

(provide 'extra-history)

;; (org-readme-sync)
;; (magit-push)

;;; extra-history.el ends here
