;; -*- emacs-lisp -*-

;; Enable recursive editing in the minibuffer
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Recursive-Edit.html
;; (setq enable-recursive-minibuffers t)

;; magit
(setq magit-push-always-verify nil)

;; Syntax checking
(add-hook 'after-init-hook #'global-flycheck-mode)

;; HACK - Textmate mode checks for the 'ns version of emacs
;; but not the railwaycat 'mac version
(let ((imitate-ns (and (featurep 'mac)
                       (not (featurep 'ns)))))
  (if imitate-ns (provide 'ns))
  (require 'textmate)
  (textmate-mode)
  ;; Clean up our HACK
  (if imitate-ns (setq features (delete 'ns features))))

;; Makes autocomplete easier to read
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

(require 'crux)

(require 'alert)
(setq alert-default-style 'notifier)

(add-to-list 'load-path "~/github/receiver")
(require 'receiver)
(add-hook 'receiver-request-received-hook
          (lambda () (alert "New request received.")))
