;; -*- emacs-lisp -*-

;; turn off splash screen messages
(setq inhibit-startup-echo-area-message t
      inhibit-startup-screen t)

;; no window chrome!
(custom-set-variables
 '(column-number-mode t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

;; I have loads of RAM, don't start GC until I've used a decent chunk
(setq gc-cons-threshold 20000000)

;;; utf-8 all the time
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq slime-net-coding-system 'utf-8-unix)

;; I'll be sending files from the command line
(server-start)

;;; local lisp packages for this configuration live here
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; Use Cask for ELPA-style packages
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Enable pallet-mode so that newly installed
;; packages are written to the Cask file
(require 'pallet)
(pallet-mode t)

;;; Like /etc/rc.d, all startup filenames begin with a number and get
;;; loaded in numerical order.
(mapc #'load-file (directory-files (concat user-emacs-directory "modules") t "[0-9]*.el"))
