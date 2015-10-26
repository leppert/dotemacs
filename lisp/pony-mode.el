(require 'pony-mode)

;;; Code:

;;; https://github.com/davidmiller/pony-mode/blob/88ac384a07f50c46e3e81789317ee0fc25e3b53f/src/pony-mode.el#L955
(defun pony-test-nose (command)
  "Run the test(s) given by `command' using nose syntax."
  (interactive
   (let* ((defuns (subseq (split-string (which-function) "\\.") 0 2))
          (class (first defuns))
          (func (let ((f (second defuns))) (and f (string-match "^test" f) f)))
          (module (pony-get-module))
          (default-command
            (concat module (and module class ":") class (and class func ".") func)))
     (list (read-string "Test: " default-command))))
  (let ((buffer (get-buffer "*ponytests*")))
    (when buffer
      (save-excursion
        (pop-to-buffer buffer)
        (erase-buffer))))
  (pony-manage-pop "ponytests" (pony-manage-cmd)
                   (list "test" (if pony-test-failfast "--failfast" "") command))
  (pony-test-mode))

(pony-key "\C-c\C-pn" 'pony-test-nose)

(provide 'pony-mode-extensions)
;;; pony-mode-extensions.el ends here
