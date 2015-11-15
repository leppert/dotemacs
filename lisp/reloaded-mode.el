;;; reloaded-mode.el --- Support for the Reloaded Clojure workflow with CIDER

(defun reloaded--user-eval (form)
  "Execute within the user namespace."
  (cider-ensure-connected)
  (reloaded--ensure-implements-reloaded-pattern)
  (cider-tooling-eval form (cider-interactive-eval-handler)))

(defun reloaded-init ()
  (interactive)
  (reloaded--user-eval "(init)"))

(defun reloaded-start ()
  (interactive)
  (reloaded--user-eval "(start)"))

(defun reloaded-stop ()
  (interactive)
  (reloaded--user-eval "(stop)"))

(defun reloaded-go ()
  (interactive)
  (reloaded--user-eval "(go)"))

(defun reloaded-clear ()
  (interactive)
  (reloaded--user-eval "(clear)"))

(defun reloaded-suspend ()
  (interactive)
  (reloaded--user-eval "(suspend)"))

(defun reloaded-resume ()
  (interactive)
  (reloaded--user-eval "(resume)"))

(defun reloaded-reset ()
  (interactive)
  (reloaded--user-eval "(reset)"))

(defun reloaded-reset-all ()
  (interactive)
  (reloaded--user-eval "(reset-all)"))

(defun reloaded--implements-reloaded-pattern ()
  (not (equal "nil" (nrepl-dict-get
                     (cider-nrepl-sync-request:eval "(and (resolve 'component/start) (resolve 'component/stop))")
                     "value"))))

(defun reloaded--ensure-implements-reloaded-pattern ()
  (unless (reloaded--implements-reloaded-pattern)
    (error "This project doesn't implement the Reloaded pattern")))

(defun reloaded--reset-on-save ()
  (if cider-connected-p 'reloaded-reset))

(defvar reloaded-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-c C-r") 'reloaded-reset)))
  "Keymap for reloaded-mode.")

;;;###autoload
(define-minor-mode reloaded-mode
  "Minor mode for projects using the Reloaded Clojure workflow with CIDER."
  :lighter "reloaded"
  :keymap  reloaded-mode-map
  :group   cider)

;;;###autoload
(add-hook 'cider-mode-hook
          '(lambda ()
             (if (reloaded--implements-reloaded-pattern) (reloaded-mode))))

(add-hook 'reloaded-mode-hook
          '(lambda ()
             (add-hook 'after-save-hook 'reloaded--reset-on-save nil 'make-it-local)))

(provide 'reloaded-mode)
