;; -*- emacs-lisp -*-

;;; GENERAL

;; four space tab stops in general, using spaces
(setq-default tab-width 4)
(setq c-default-style "k&r"
      c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; line-wrap (fill) comments as I type
(auto-fill-mode 1)
(setq comment-auto-fill-only-comments t)

;; Tired of fighting with incompatible versions of auto-complete and
;; popup, I'm moving to company mode
(add-hook 'after-init-hook 'global-company-mode)

;; after-load sort of hook?
(setq company-idle-delay 0.3)

;; Various superfluous white-space: kill it
(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

;; these are bound to "kill-this-buffer" by default
(global-set-key (kbd "s-K") nil)
(global-set-key (kbd "s-k") nil)
(add-hook 'prog-mode-hook
          (lambda ()
            ;; format as we go
            (define-key (current-local-map) [remap newline] 'reindent-then-newline-and-indent)
            ;; compile short cuts
            (define-key (current-local-map) (kbd "s-K") 'compile)
            (define-key (current-local-map) (kbd "s-k") 'recompile)
            ;; prog-mode-hook to hightlight XXX, BUG and TODO in code
            (font-lock-add-keywords
             nil '(("\\<\\(XXX\\|BUG\\|TODO\\)" 1 font-lock-warning-face prepend)))))

;; reindent-then-newline-and-indent is bad news for languages
;; with syntactically important whitespace
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map [remap newline] 'newline-and-indent)))

(add-hook 'slim-mode-hook
          (lambda ()
            (define-key slim-mode-map [remap newline] 'newline-and-indent)))

(add-hook 'haml-mode-hook
          (lambda ()
            (define-key haml-mode-map [remap newline] 'newline-and-indent)))

;;;;;; LISPS

;; highlight matching parens, please
(show-paren-mode)

;; keybinding stolen from Lighttable, which I'm told stole it from
;; Flash.
(global-set-key (kbd "<s-return>") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "<s-return>") 'eval-defun)

;; ... add shift to eval last expression
(global-set-key (kbd "<S-s-return>") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "<S-s-return>") 'eval-last-sexp)

;;; SMARTPARENS

;; Use smartparens defaults
(require 'smartparens-config)
(sp-use-smartparens-bindings)
(smartparens-global-mode t)

;;; ELISP

(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(eval-after-load 'eldoc
  '(diminish 'eldoc-mode))

;; (defun elisp-popup-doc ()
;;   "Use auto-complete's popup support to look up docs in emacs-lisp buffers."
;;   (interactive)
;;   (popup-tip (ac-symbol-documentation (symbol-at-point))
;;              :around t
;;              :scroll-bar t
;;              :margin t))

(define-key lisp-interaction-mode-map (kbd "C-c d") 'elisp-popup-doc)
(define-key emacs-lisp-mode-map (kbd "C-c d") 'elisp-popup-doc)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)

;; TODO binding?
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;; SCHEME
(require 'geiser)
(setq geiser-active-implementations '(racket))
(add-hook 'geiser-mode-hook
          '(lambda ()
             (define-key geiser-mode-map (kbd "C-c d") 'geiser-doc-symbol-at-point)
             (define-key geiser-mode-map (kbd "<s-return>") 'geiser-eval-definition)
             (define-key geiser-mode-map (kbd "<S-s-return>") 'geiser-eval-last-sexp)))

;;; XXX COMMON LISP moved to private config for the moment

;;; CLOJURE

(eval-after-load "cider"
  '(progn
     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
     (add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
     (setq cider-repl-print-length 1000)
     (setq cider-repl-use-clojure-font-lock t)
     (setq cider-repl-pop-to-buffer-on-connect nil)))

;; I like this keybinding from Lighttable
(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "<s-return>") 'cider-eval-defun-at-point)
     ;; add shift to eval the last expression, rather than the top-level one
     (define-key clojure-mode-map (kbd "<S-s-return>") 'cider-eval-last-expression)))

;;(require 'nrepl-eval-sexp-fu)
;;(setq nrepl-eval-sexp-fu-flash-duration 0.3)

(require 'cider-eval-sexp-fu)
(setq cider-eval-sexp-fu-flash-duration 0.2)

;;;;;; HASKELL 

;;;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

;;;;;; RUBY

;; RVM https://rvm.io
(require 'rvm)
(rvm-use-default)

;; ruby, using robe
(add-hook 'ruby-mode-hook 'robe-mode)

;; Running tests
(require 'rinari)
(define-key rinari-minor-mode-map (kbd "C-c C-c") 'rinari-test)

(require 'rspec-mode)
(define-key rspec-mode-map (kbd "C-c C-c") 'rspec-verify-single)

(require 'minitest)
(setq minitest-use-spring t)
(define-key minitest-mode-map (kbd "C-c C-c") 'minitest-verify-single)

;;;;;; PYTHON

(elpy-enable)
(load "pony-mode.el")
(define-key pony-minor-mode-map (kbd "C-c C-c") 'pony-test)

;;;;;; JAVASCRIPT/COFFEESCRIPT

;; js2-mode
(require 'js2-mode)
(setq-default js2-auto-indent-p t)
(setq-default js2-basic-offset 2)
(setq-default js2-global-externs '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; http://blog.binchen.org/posts/why-emacs-is-better-editor.html
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

;; tern
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(require 'company-tern)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-tern))

;; skewer mode for browser mind control
(require 'skewer-mode)
(require 'skewer-repl)
(require 'skewer-html)
(require 'skewer-css)

;; configure all of mode hooks (previous default)
(skewer-setup)

(defun skewer-start ()
  (interactive)
  (let ((httpd-port 8023))
    (httpd-start)
    (message "Ready to skewer the browser. Now jack in with the bookmarklet.")))

(define-key skewer-mode-map (kbd "<s-return>") 'skewer-eval-defun)
(define-key skewer-mode-map (kbd "<S-s-return>") 'skewer-eval-last-expression)

;; Bookmarklet to connect to skewer from the browser:
;; javascript:(function(){var d=document ;var s=d.createElement('script');s.src='http://localhost:8023/skewer';d.body.appendChild(s);})()

;;two space tabs in coffee
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook '(lambda() (coffee-custom)))

(setq flycheck-coffeelintrc (expand-file-name "config/coffeelint.json"))

(add-to-list 'load-path (expand-file-name "lisp/ac-coffee" user-emacs-directory))
(require 'ac-coffee)

;;;;;; PHP

(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))


;;;;;; CSS & COMPILES TO CSS

(require 'rainbow-mode)
(add-to-list 'auto-mode-alist '("\\.s?css$" . rainbow-mode)) 

;; SCSS
(require 'scss-mode)
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(setq scss-compile-at-save nil)

;;;;;; HTML & TEMPLATING

;; web-mode

(defun web-mode-customizations ()
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'web-mode-customizations)

;; https://mustache.github.io/
(require 'mustache-mode)
