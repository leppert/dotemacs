;; -*- emacs-lisp -*-

;; Enable recursive editing in the minibuffer
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Recursive-Edit.html
;; (setq enable-recursive-minibuffers t)

;; magit
(use-package magit
  :custom (magit-push-always-verify nil))

;; Syntax checking
(use-package flycheck
  :config (global-flycheck-mode))

(use-package textmate
  :config (textmate-mode))
;; HACK - Textmate mode checks for the 'ns version of emacs
;; but not the railwaycat 'mac version
;;(let ((imitate-ns (and (featurep 'mac)
;;                       (not (featurep 'ns)))))
;;  (if imitate-ns (provide 'ns))
;;  (require 'textmate)
;;  (textmate-mode)
;;  ;; Clean up our HACK
;;  (if imitate-ns (setq features (delete 'ns features))))

;; Makes autocomplete easier to read
(use-package uniquify
  :straight nil ;; it's a built in lib
  :custom (uniquify-buffer-name-style 'forward))

(use-package crux)

(use-package alert
  :custom (alert-default-style 'osx-notifier))

(use-package aggressive-indent)

(use-package receiver
  :straight (:host github :repo "leppert/receiver" :branch "master")
  :init (use-package elnode)
  :config (add-hook 'receiver-request-received-hook
		    (lambda () (alert "New request received."))))
