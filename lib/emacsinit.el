(setq inhibit-startup-message t)    ; will inhibit startup messages
(setq use-dialog-box nil)           ; disable popup dialog for emacs with own frame
(setq require-final-newline t)      ; will make the last line end in a carriage return
(setq-default indent-tabs-mode nil) ; default to indent with spaces
(fset 'yes-or-no-p 'y-or-n-p)       ; will allow you to type just "y" instead of "yes" when you exit
(setq next-line-add-newlines nil)   ; will disallow creation of new lines with "down key" at end of buffer
;;; (menu-bar-mode nil)                ; Disable menu bar
(tool-bar-mode -1)                  ; No tool bar
(auto-compression-mode 1)           ; Automatic open compressed files
(delete-selection-mode t)           ; Delete selection when typing (like most editors does)
(setq vc-follow-symlinks t)         ; Automatically follow symlinks
(setq split-height-threshold 80)    ; Give preference to horizontal window split
(setq split-width-threshold nil)
(savehist-mode 1)
(electric-pair-mode 1)              ; auto close bracket insertion. New in emacs 24

;;; Key bindings. Emacs convention is that \C-c[a-zA-A] is reserve for user
(global-set-key "\C-cf"              'ffap)         ; find file at point
(global-set-key "\C-x\C-b"           'ibuffer)   ; replaces std buffer list
(global-set-key (kbd "C-c l")        'toggle-flycheck-errors-window)
(global-set-key (kbd "s-w")          'delete-window)  ;; Cmd-W closes an  window, not the frame

;;; Customization placed in its own file. Create it if it doesn't exist
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t)

;;; Install use-package if not yet installed
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (unless package-archive-contents (package-refresh-contents))
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;;; Use use-package to extend use-package with the :chords keyword
(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

;;; package-utils used to to automatically package update
(use-package package-utils
  :ensure t
  :commands package-utils-upgrade-all)

;; An idea: Could use load-file-name to locate shell goodies
(let ((goodies-root (getenv "SHELL_GOODIES_ROOT")))
  (if goodies-root
      (progn
        ;; Load all files in .../lib/elisp.d
        (let ((lib-dir (expand-file-name "lib/elisp.d" goodies-root))
              (loaded (mapcar #'car load-history))) ; All loaded files. Don't load same file twice
          (dolist (file (directory-files lib-dir t ".+\\.elc?$"))
            (when (and (not (member file loaded)) (file-exists-p file))
              (load (file-name-sans-extension file))
              (push file loaded)))))
    (message "SHELL_GOODIES_ROOT not defined. Limited functionality")))

(defun toggle-flycheck-errors-window ()
  (interactive)
  (let ((errors-win (get-buffer-window "*Flycheck errors*")))
    (if errors-win
        (delete-window errors-win)
      (let ((height (window-total-height)))
        (if (< height 20)
            (list-flycheck-errors)
          (split-window nil -5 'below)
          (list-flycheck-errors))))))

;;; Some mode mappings
(add-to-list 'auto-mode-alist '("\\.zsh\\(-theme\\)?\\'" . shell-script-mode))

;;; Customization of shell-script-mode
(add-hook 'sh-mode-hook '(lambda () (modify-syntax-entry ?$ "w")))

;; syntax highlight
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;;; Git key bindings
(defvar git-command-map (make-sparse-keymap))
(global-set-key (kbd "s-i") git-command-map)
(define-key git-command-map (kbd "s-i") 'magit-status)
;; Jump between hunks
(define-key git-command-map (kbd "n") 'git-gutter+-next-hunk)
(define-key git-command-map (kbd "p") 'git-gutter+-previous-hunk)
;; Act on hunks
(define-key git-command-map (kbd "v =") 'git-gutter+-show-hunk)
(define-key git-command-map (kbd "r") 'git-gutter+-revert-hunks)
;; Stage hunk at point.
;; If region is active, stage all hunk lines within the region.
(define-key git-command-map (kbd "t") 'git-gutter+-stage-hunks)
(define-key git-command-map (kbd "c") 'git-gutter+-commit)
(define-key git-command-map (kbd "C") 'git-gutter+-stage-and-commit)
(define-key git-command-map (kbd "C-y") 'git-gutter+-stage-and-commit-whole-buffer)
(define-key git-command-map (kbd "U") 'git-gutter+-unstage-whole-buffer)

;;; ace window
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

;;; ag (the silver searcher)
(use-package ag
  :ensure t
  :config
  (add-hook 'ag-mode-hook '(lambda () (define-key ag-mode-map (kbd "w") #'wgrep-change-to-wgrep-mode)))
  :bind
  ("C-c C-s" . ag))
  ;;(:map ag-mode-map ("w" . wgrep-change-to-wgrep-mode)))

;;; Aggresive indent
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  ;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  :bind ("C-c ai" . aggressive-indent-mode))

;;; Anzu
(use-package anzu
  :ensure t
  :config (global-anzu-mode 1))

;;; buffer-move
(use-package buffer-move
  :ensure t
  :bind
  ("<M-s-up>"     . buf-move-up)
  ("<M-s-down>"   . buf-move-down)
  ("<M-s-left>"   . buf-move-left)
  ("<M-s-right>"  . buf-move-right))

;;; Company
(use-package company
  :ensure t
  :bind ("M-SPC" . company-complete)
  :config (global-company-mode))
(use-package company-shell
  :ensure t
  :config
  (add-to-list 'company-backends 'company-shell)
  ;; company-files is part of backends, but doesn't seem to work when late in the list
  (setq company-backends (cons #'company-files company-backends)))
(use-package "company-flx" :ensure t :config (company-flx-mode +1))
(use-package "company-quickhelp" :ensure t :config (company-quickhelp-mode 1))

(use-package compile
  :config
  (setq compilation-scroll-output t))

;;; Dash
(use-package dash
  :config
  (dash-enable-font-lock))

;;; etags
(use-package etags
  :config
  (setq tags-revert-without-query t))

;;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'flycheck-error-list-after-refresh-hook
            '(lambda () (-when-let (window (flycheck-get-error-list-window t))
                          (with-selected-window window
                            (fit-window-to-buffer window 30)))))
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; Flyspell
(use-package flyspell
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  :bind (:map flyspell-mouse-map
              ([down-mouse-3] . flyspell-correct-word)
              ([mouse-3] . undefined)))

;;; git-gutter+
(use-package git-gutter+
  :ensure t
  :config (global-git-gutter+-mode +1))

;;; git-messenger
(use-package git-messenger
  :ensure t
  :config (setq git-messenger:show-detail t)
  :bind
  (("C-c v p" . git-messenger:popup-message)
   :map git-command-map
   ("m" . git-messenger:popup-message)))

;;; git-modes - ignore, config, attributes
(use-package gitignore-mode :ensure t)

;;; Hippie expand
(use-package hippie-exp
  :bind ("M-/" . hippie-expand) ; replace std Emacs expand key
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

;;; Ido
(use-package ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-enable-flex-matching t
        ido-use-virtual-buffers t))
(use-package "ido-at-point"
  :ensure t
  :init (ido-at-point-mode))
(use-package "ido-ubiquitous"
  :ensure t
  :init
  (ido-ubiquitous-mode 1))

;;; Mark down
(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq markdown-command "pandoc --smart -f markdown -t html")
              (visual-line-mode t)
              (use-package "writegood-mode" :ensure t :init (writegood-mode t))
              (flyspell-mode t))))

;;; Magit
(use-package magit
  :ensure t ;; :pin melpa-stable ;; magit is in stable, but its dependencies isn't :-(
  :commands ido-enter-magit-status
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd "C-x g") 'ido-enter-magit-status))))

;;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

;;; parens - Highlight matching parentheses next to cursor
(use-package paren
  :config
  (show-paren-mode t))

;;; Popwin
(use-package popwin
  :config
  (require 'popwin)
  (popwin-mode 1))

;;; Projectile
(use-package projectile
  :ensure t
  :demand t
  :bind-keymap ("s-p" . projectile-command-map)
  :config
  (setq projectile-tags-file-name ".tags")
  (projectile-global-mode))

;;; Project Explorer
(use-package project-explorer
  :ensure t
  :bind (:map projectile-command-map ("s-p" . project-explorer-toggle)))

;;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; Smart mode line
(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  ;; Rich Minority - package loaded by smart-mode-line
  (require 'cl)
  (setq rm-blacklist (cl-list* " Anzu" " ARev" " company" " GitGutter" rm-blacklist)))

;;; smex
(use-package smex
  :ensure t
  :config
  (smex-initialize)
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands))

;;; Tramp
(use-package tramp
  :config
  (setq tramp-default-method "scp"))

;;; Visible mark
(use-package visible-mark
  :ensure t
  :init
  (defface visible-mark-active
    '((((type tty) (class mono)))
      (t (:background "magenta"))) "")
  :config
  (setq visible-mark-max 2)
  (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))
  (global-visible-mark-mode 1))

;;; wgrep - write to files in search result buffer
(use-package wgrep-ag
  :ensure t
  :commands wgrep-change-to-wgrep-mode
  :init
  (setq wgrep-enable-key "w")
  (setq wgrep-auto-save-buffer t))


;;; IBuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)

;;; Update frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (format "E: %s (%%I)" (abbreviate-file-name (buffer-file-name)))
                 "E: %b"))))
