;; -*- emacs-lisp -*-

;;(require 'typopunct)
;;(typopunct-change-language 'english t)

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.\\(md\\|markdown\\)$" . markdown-mode) auto-mode-alist)
  ;; GitHub-flavoured markdown for what look like GH README files
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  ;; I prefer my navigation keys be left alone, please.
  (dolist (binding (list (kbd "M-<up>")
                         (kbd "M-<down>")
                         (kbd "M-<left>")
                         (kbd "M-<right>")))
    (define-key markdown-mode-map binding nil)))

(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; I like to date my diary entries in this format
(defun insert-current-date ()
  "Insert a YYYY-MM-DD representation of the current date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%m" (current-time))))

;;; I wrote a little minor mode for word count because it helps me to
;;; hit targets while writing.
(defvar wordcount-timer nil
  "Timer to kick off word count recomputation.")

(defvar wordcount-current-count 0
  "The result of the last word count.")

(defun wordcount-update-word-count ()
  "Recompute the word count."
  (setq wordcount-current-count (count-words (point-min) (point-max))))

(define-minor-mode wordcount-mode
  "Toggle wordcount mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, the word count for the current buffer
is displayed in the mode-line."
  :init-value nil
  :lighter (:eval (format " [%d words]" wordcount-current-count))
  (if wordcount-mode
      (progn
        (set (make-local-variable 'wordcount-current-count)
             (count-words (point-min) (point-max)))
        (set (make-local-variable 'wordcount-timer)
              (run-with-idle-timer 3 't #'wordcount-update-word-count)))
    (cancel-timer wordcount-timer)))

;;(add-hook 'markdown-mode-hook (lambda ()
;;                                (wordcount-mode)
;;                                (typopunct-mode)))

;; Retain keyboard navigation in org-mode
(use-package org
  :custom
  ;; Retain shift-selection in org-mode as well
  (org-support-shift-select 'always)
  :config
  (define-key org-mode-map [remap backward-paragraph] nil)
  (define-key org-mode-map [remap forward-paragraph] nil)
  (define-key org-mode-map (kbd "s->") 'org-metaright)
  (define-key org-mode-map (kbd "s-<") 'org-metaleft)
  (dolist (binding (list (kbd "M-<up>")
                         (kbd "M-<down>")
                         (kbd "M-<left>")
                         (kbd "M-<right>")
                         (kbd "S-<up>")
                         (kbd "S-<down>")
                         (kbd "C-S-<up>")
                         (kbd "C-S-<down>")
                         (kbd "C-<up>")
                         (kbd "C-<down>")
                         (kbd "M-S-<left>")
                         (kbd "M-S-<right>")
                         (kbd "M-S-<down>")
                         (kbd "M-S-<up>")))
    (define-key org-mode-map binding nil)))

(use-package org-download
  :custom
  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01388.html
  (org-image-actual-width '(300))
  (org-download-image-dir "./media/images")
  (org-download-heading-lvl nil))
