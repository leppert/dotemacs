;; -*- emacs-lisp -*-

;;; DATA MUNGING

(use-package s)

(defun reformat-field (str)
  "Trims leading/trailing whitespace from `str`, then converts it
to a number if the string looks like a number."
  (let ((s (s-trim str)))
    (if (and (string-match "[\$\.,0-9]+" s) (= (match-end 0) (length s)))
        (string-to-number (replace-regexp-in-string "[\$,]" "" s))
      s)))

(defun tsv-to-sexp (tsv)
  "Parses the string `tsv` as a tab-separated-value file,
returning a sexp containing the values with strings converted to
numbers where appropriate."
  (-map (lambda (s) (-map 'reformat-field (s-split "\t" s))) (s-lines tsv)))

;;; MISC HELPER FUNCTIONS

;;; some of the keycodes produced by OSX are inscrutable
(defun insert-key-sequence (key-repr)
  "Reads a literal key sequence from the user and inserts a
representation of it at point, suitable for `global-set-key' or
`local-set-key'."
  (interactive "KKey sequence? ")
  (prin1 key-repr (current-buffer))
  (insert " "))

;;; emacs doesn't include a function to rename a buffer and file at
;;; the same time by default, so here's one.
;;; via: http://stackoverflow.com/a/25212377/313561
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; via: http://www.emacswiki.org/emacs/RevertBuffer
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files."))

;; vim style kill line
;; via: http://stackoverflow.com/a/2173393/313561
(defun kill-current-line (&optional n)
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((kill-whole-line t))
      (kill-line n))))

;; Via: https://github.com/magnars/.emacs.d/blob/5fb9e3170a80614da4fc7a4b0807d22b3465f538/defuns/buffer-defuns.el#L42
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Via: https://github.com/magnars/.emacs.d/blob/5fb9e3170a80614da4fc7a4b0807d22b3465f538/defuns/buffer-defuns.el#L67
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))
