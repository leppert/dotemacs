;; -*- emacs-lisp -*-

;; I will not type 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; turn off safety mode
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; ido-mode is a work of beauty and magic
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; smex is "smart M-x"
(use-package smex
  :custom (smex-key-advice-ignore-menu-bar t)
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

;; nicer buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; chorded backward kill magnifiers
(global-set-key (kbd "C-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-paragraph)

;; chorded backward kill magnifiers
(global-set-key [C-M-kp-delete] 'kill-paragraph)

;; prefer regexp in my backward search, inputrc-compatible binding
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; moving between windows, normalized with iTerm2 and (mod'd) tmux
(global-set-key [M-s-left]  'windmove-left)
(global-set-key [M-s-right] 'windmove-right)
(global-set-key [M-s-up]    'windmove-up)
(global-set-key [M-s-down]  'windmove-down)

;; enhanced completion library, same as inputrc binding
(global-set-key (kbd "M-/") 'hippie-expand)

;; expand-region is super handy! I like having expand- and contract-
;; side by side within easy reach.
(use-package expand-region
  :config
  (global-set-key (kbd "s-1") 'er/expand-region)
  (global-set-key (kbd "s-2") 'er/contract-region))

;; I can't get today's kids interested in set-mark, so I've repurposed
;; C-SPC for ace-jump-mode.
(use-package ace-jump-mode
  :config (global-set-key (kbd "C-SPC") 'ace-jump-mode))

;; Multiple Cursors
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; Bind silver searcher to super shift f
(use-package helm-ag
  :config (global-set-key (kbd "s-F") 'helm-ag-project-root))

(use-package visual-regexp-steroids
  :config
  (define-key global-map (kbd "C-c r") 'vr/replace)
  (define-key global-map (kbd "C-c q") 'vr/query-replace))

;; Go ahead and kill the current buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Use swiper in place of isearch
;; https://github.com/abo-abo/swiper#installation
;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (global-set-key "\C-s" 'swiper)
;; (global-set-key "\C-r" 'swiper)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key [f6] 'ivy-resume)

;; Window resizing
;; via: http://emacswiki.org/emacs/WindowResize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; via: https://github.com/magnars/.emacs.d/blob/2d38d3470eaa1ef9e14ce5720e1690dee54330ed/settings/key-bindings.el#L165-L166
(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)
