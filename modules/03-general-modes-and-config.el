;; -*- emacs-lisp -*-

;; Enable recursive editing in the minibuffer
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Recursive-Edit.html
;; (setq enable-recursive-minibuffers t)

;; magit
(setq magit-push-always-verify nil)

;; Syntax checking
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'textmate)
(textmate-mode)

;; Makes autocomplete easier to read
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

(require 'crux)

(require 'alert)
(setq alert-default-style 'notifier)

