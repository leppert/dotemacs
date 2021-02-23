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
(use-package smartparens
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (smartparens-global-mode t))

;;; ELISP

(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(use-package eldoc
  :straight nil ;; builtin
  :diminish eldoc-mode)

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
(use-package geiser
  :custom (geiser-active-implementations '(racket))
  :config
  (add-hook 'geiser-mode-hook
            '(lambda ()
               (define-key geiser-mode-map (kbd "C-c d") 'geiser-doc-symbol-at-point)
               (define-key geiser-mode-map (kbd "<s-return>") 'geiser-eval-definition)
               (define-key geiser-mode-map (kbd "<S-s-return>") 'geiser-eval-last-sexp))))

;;; CLOJURE

(setq clojure-defun-style-default-indent t)

(use-package cider
  :custom
  (cider-repl-print-length 1000)
  (cider-repl-use-clojure-font-lock t)
  (cider-repl-pop-to-buffer-on-connect nil)
  :config (progn
            (add-hook 'cider-mode-hook 'eldoc-mode)
            (add-hook 'cider-interaction-mode-hook 'eldoc-mode)))

(setq cider-cljs-lein-repl
      "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")

;; via https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl#optional-emacscider-keybinding
(defun cider-figwheel-repl ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
    (cider-repl-return)))

(global-set-key (kbd "C-c C-f") #'cider-figwheel-repl)

;; I like this keybinding from Lighttable
(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "<s-return>") 'cider-eval-defun-at-point)
     ;; add shift to eval the last expression, rather than the top-level one
     (define-key clojure-mode-map (kbd "<S-s-return>") 'cider-eval-last-expression)
     ;; For compojure - https://github.com/weavejester/compojure/wiki/Emacs-indentation
     (define-clojure-indent
       (defroutes 'defun)
       (GET 2)
       (POST 2)
       (PUT 2)
       (DELETE 2)
       (HEAD 2)
       (ANY 2)
       (context 2))))

;;(require 'nrepl-eval-sexp-fu)
;;(setq nrepl-eval-sexp-fu-flash-duration 0.3)

(use-package cider-eval-sexp-fu
  :custom (cider-eval-sexp-fu-flash-duration 0.2))

(setq inf-clojure-program "planck")

;;;;;; HASKELL 

;;;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

;;;;;; RUBY

;; RVM https://rvm.io
(use-package rvm
  :config (rvm-use-default))

(setq flycheck-rubylintrc "ruby-lint.yml")

;; ruby, using robe
(add-hook 'ruby-mode-hook 'robe-mode)

(use-package bundler)

;; Running tests
(use-package rinari
  :config (define-key rinari-minor-mode-map (kbd "C-c C-c") 'rinari-test))

(use-package rspec-mode
  :config (define-key rspec-mode-map (kbd "C-c C-c") 'rspec-verify-single))

(use-package minitest
  :custom (minitest-use-spring t))

;; Hacks to fix https://github.com/arthurnn/minitest-emacs/issues/34
(defun minitest-test-command ()
   (cond (minitest-use-spring '("spring" "rails" "test"))
        ((minitest-zeus-p) '("zeus" "test"))
        (t minitest-default-command)))

(defun minitest-verify-single ()
  "Run on current file."
  (interactive)
  (minitest--file-command-with-line-number))

(defun minitest--file-command-with-line-number (&optional post-command)
  "Run COMMAND on currently visited file."
  (let ((file-name (file-relative-name (buffer-file-name) (minitest-project-root)))
        (line-number (number-to-string (line-number-at-pos))))
    (if (and file-name line-number)
        (minitest-run-file (concat file-name ":" line-number) post-command)
      (error "Buffer is not visiting a file"))))

(define-key minitest-mode-map (kbd "C-c C-c") 'minitest-verify-single)

;;;;;; PYTHON


;; Fixes "Symbol's function definition is void: tramp-tramp-file-p"
(use-package tramp)

(use-package elpy
  :init (elpy-enable))
;;(define-key pony-minor-mode-map (kbd "C-c C-c") 'pony-test)

;;;;;; JAVASCRIPT/COFFEESCRIPT

;; js2-mode
(use-package js2-mode
  :custom
  (js2-auto-indent-p t)
  (js2-basic-offset 2)
  (js2-global-externs '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; http://blog.binchen.org/posts/why-emacs-is-better-editor.html
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))

;; skewer mode for browser mind control
(use-package skewer-mode)

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
(custom-set-variables '(coffee-tab-width 2))
(add-hook 'coffee-mode-hook '(lambda() (coffee-custom)))

(setq flycheck-coffeelintrc (expand-file-name "config/coffeelint.json"))

;;;;;; PHP

(use-package php-mode
  :config (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode)))


;;;;;; CSS & COMPILES TO CSS

(setq-default css-indent-offset 2)

(use-package rainbow-mode
  :config (add-hook 'css-mode-hook 'rainbow-mode))

;; SCSS
(use-package scss-mode
  :custom (scss-compile-at-save nil)
  :config
  (autoload 'scss-mode "scss-mode")
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode)))

;;;;;; HTML & TEMPLATING

;; web-mode

(defun web-mode-customizations ()
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'web-mode-customizations)

;; https://mustache.github.io/
(use-package mustache-mode)

(use-package vue-mode)

(use-package tide
  :hook (tide-mode . setup-tide-mode)
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1)))

(use-package typescript-mode
  :hook (typescript-mode . setup-typescript-mode)
  :config (defun setup-typescript-mode ()
            ;; https://github.com/ananthakumaran/tide/issues/229#issuecomment-357379743
            (setq typescript-indent-level
                  (or (plist-get (tide-tsfmt-options) ':indentSize) 2))))
