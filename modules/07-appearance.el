;; -*- emacs-lisp -*-

;; enable sRGB colors in the Cocoa version of em
;;(setq ns-use-srgb-colorspace t)

(setq
 ;; display line & column numbers in mode-line
 line-number-mode t
 column-number-mode t

 ;; speed up screen re-paint on local sessions
 redisplay-dont-pause t

 ;; use smooth scrolling
 pixel-scroll-mode t

 ;; general look and feel things
 font-lock-maximum-decoration t
 color-theme-is-global t

 truncate-partial-width-windows nil)

;; Default visible bell is... alarming. Use something more subtle.
;; via; https://www.emacswiki.org/emacs/AlarmBell#toc8
(defun modeline-visible-bell ()
  "Briefly inverts the modeline face (for use as ring-bell-function)"
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell nil
      ring-bell-function 'modeline-visible-bell)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; put the current filename and path in the frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; font and spacing
(set-frame-font "Menlo-12")
(setq-default line-spacing 3)

;; color emoji support
;; (require 'emojify)
;; (add-hook 'after-init-hook #'global-emojify-mode)
;; (setq-default emojify-display-style 'unicode)
  (set-fontset-font "fontset-default"
                    '(#x1F600 . #x1F64F)
                    (font-spec :name "Apple Color Emoji") nil 'prepend)

;; unblinking bar-style cursor
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

;; fancy lambda, &c
(global-prettify-symbols-mode 1)
(mapc (lambda (m) (add-hook m (lambda () (push '("fn" . ?ƒ) prettify-symbols-alist))))
     '(clojure-mode-hook clojurescript-mode-hook))

;; diminish global minor modes
(eval-after-load 'undo-tree
  '(diminish 'undo-tree-mode))

;; locally hacked version of noctilux, turning into something else
(add-to-list 'load-path (concat user-emacs-directory "eigengrau/"))
(require 'eigengrau-theme)

;; powerline gives a much aesthetically improved mode line
(require 'powerline)
(powerline-center-theme)

;; dim the parentheses
(require 'parenface-plus)
(set-face-foreground 'paren-face "#666")

;; for the silver surfer highlighting
(setq ag-highlight-search t)

;; customize company-mode's popup
(let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
     `(company-tooltip-common-selection ((t (:inherit font-lock-keyword-face))))
     `(company-tooltip-selection ((t (:inherit font-lock-keyword-face))))))
