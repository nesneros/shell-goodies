(setq inhibit-startup-message t)    ; will inhibit startup messages
(setq use-dialog-box nil)           ; disable popup dialog for emacs with own frame
(setq require-final-newline t)      ; will make the last line end in a carriage return
(setq-default indent-tabs-mode nil) ; default to indent with spaces
(fset 'yes-or-no-p 'y-or-n-p)       ; will allow you to type just "y" instead of "yes" when you exit
(setq next-line-add-newlines nil)   ; will disallow creation of new lines with "down key" at end of buffer
;;; (menu-bar-mode nil)                ; Disable menu bar
(tool-bar-mode -1)                  ; No tool bar
(auto-compression-mode 1)           ; Automatic open compressed files
(setq compilation-scroll-output t)  ; Auto scroll *compilation* buffer
(delete-selection-mode t)           ; Delete selection when typing (like most editors does)
(setq vc-follow-symlinks t)         ; Automatically follow symlinks
(setq tags-revert-without-query 1)  ; Automatically reload tags when file changed without prompting
(setq split-height-threshold 80)    ; Give preference to horizontal window splita
(setq split-width-threshold nil)
(savehist-mode 1)
(electric-pair-mode 1)              ; auto close bracket insertion. New in emacs 24

;;; Key bindings. Emacs convention is that \C-c[a-zA-A] is reserve for user
(global-set-key "\C-cf"              'ffap)         ; find file at point
(global-set-key "\M-/"               'hippie-expand) ; replace std Emacs expand key
(global-set-key "\M- "               'hippie-expand) ; replace std Emacs expand key
(global-set-key "\C-x\C-b"           'ibuffer)   ; replaces std buffer list
(global-set-key (kbd "C-c C-s")      'ag)
(global-set-key (kbd "<M-s-up>")     'buf-move-up)
(global-set-key (kbd "<M-s-down>")   'buf-move-down)
(global-set-key (kbd "<M-s-left>")   'buf-move-left)
(global-set-key (kbd "<M-s-right>")  'buf-move-right)
(global-set-key (kbd "C-x o")        'ace-window)
(global-set-key (kbd "C-x v p")      'git-messenger:popup-message)

;;; Customization placed in its own file. Create it if it doesn't exist
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t)

;;; Some mode mappings
(add-to-list 'auto-mode-alist '("\\.zsh\\(-theme\\)?\\'" . shell-script-mode))

;;; Package management
(require 'package) ;; for package-installed-p function
(package-initialize)
(let ((emacs-cask-project (getenv "EMACS_CASK_PROJECT"))
      (cask-el "/usr/local/share/emacs/site-lisp/cask/cask.el")) ;; installed by Homebrew
  (if (file-exists-p cask-el)
      (progn
        (require 'cask cask-el)
        (if (or emacs-cask-project (file-exists-p "~/.emacs.d/Cask"))
            (cask-initialize emacs-cask-project)
          (error "EMACS_CASK_PROJECT not defined and Cask not in default location")))
    (error "cask.el not found at '/usr/local/share/emacs/site-lisp/cask/cask.el'")))

;;; Load all files in .../lib/elisp.d
(require 'dash)
(-when-let* ((shell-goodies-dir (getenv "SHELL_GOODIES_ROOT"))
             (lib-dir (expand-file-name "lib/elisp.d" shell-goodies-dir))
             (loaded (mapcar #'car load-history))) ; All loaded files. Don't load same file twice
  (dolist (file (directory-files lib-dir t ".+\\.elc?$"))
    (unless (member file loaded)
      (load (file-name-sans-extension file))
      (push file loaded))))

;;; Backup settings
(let ((backup-dir (expand-file-name "backup" user-emacs-directory)))
  (unless (file-directory-p backup-dir)
    (make-directory backup-dir)
    (set-file-modes backup-dir 448))

  (setq make-backup-files t
        delete-old-versions t
        kept-old-versions 2
        kept-new-versions 8
        version-control t
        backup-directory-alist `(("." . ,backup-dir))))

;;; Visible mark settings. Must be set before package is loaded
(defface visible-mark-active
  '((((type tty) (class mono)))
    (t (:background "magenta"))) "")
(setq visible-mark-max 2)
(setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))

;;; Tramp
(require 'tramp)
(setq tramp-default-method "scp")

;;; Highlight matching parentheses next to cursor
(require 'paren)
(show-paren-mode t)

;; syntax highlight
(global-font-lock-mode t)
(add-hook 'font-lock-mode-hook
          (lambda () (setq font-lock-maximum-decoration t)))

;;; Hippie expand
(require 'hippie-exp)
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
        try-complete-lisp-symbol))

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

(defmacro for-package (package &rest body)
  (if (package-installed-p (eval package))
      (while body
        (eval (pop body)))
    (message "*** Package '%s' is not installed" (eval package))))
(put 'for-package 'lisp-indent-function 1)

;;; Packages installed by the package management should not be used before the package management is initialized

;;; Aggresive indent
(for-package 'aggressive-indent
  (global-aggressive-indent-mode 1)
  ;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (global-set-key (kbd "C-c ai") 'aggressive-indent-mode))

;;; Anzu
(for-package 'anzu (global-anzu-mode 1))

;;; Dash
(eval-after-load "dash" '(dash-enable-font-lock))

;;; Flycheck
(for-package 'flycheck
  (add-hook 'flycheck-error-list-after-refresh-hook
            '(lambda () (-when-let (window (flycheck-get-error-list-window t))
                          (with-selected-window window
                            (fit-window-to-buffer window 30)))))
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; Flyspell
(eval-after-load "flyspell"
  '(progn
     (add-hook 'text-mode-hook 'flyspell-mode)
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;;; git-gutter+
(for-package 'git-gutter+ (global-git-gutter+-mode +1))

;;; Ido
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)
(for-package 'ido-at-point
  (ido-at-point-mode))
(for-package 'ido-ubiquitous
  (ido-ubiquitous-mode 1))

;;; Mark down
(for-package 'markdown-mode
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq markdown-command "pandoc --smart -f markdown -t html")
              (visual-line-mode t)
              (for-package 'writegood-mode (writegood-mode t))
              (flyspell-mode t))))

;;; Magit
(for-package 'magit
  (global-set-key (kbd "C-x g") 'magit-status)
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (autoload 'ido-enter-magit-status "magit.el")
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd "C-x g") 'ido-enter-magit-status))))

;;; Multiple cursors
(for-package 'multiple-cursors
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;;; Projectile
(for-package 'projectile
  (global-set-key (kbd "s-p") 'projectile-command-map)
  (setq projectile-tags-file-name ".tags")
  (projectile-global-mode))

;;; Project Explorer
(for-package 'project-explorer
  (define-key projectile-command-map (kbd "s-p") 'project-explorer-toggle))

;;; Rainbow Delimiters
(for-package 'rainbow-delimiters (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; Smart mode line
(for-package 'smart-mode-line
  (sml/setup)
  ;; Rich Minority - package loaded by smart-mode-line
  (setq rm-blacklist (cl-list* " Anzu" " ARev" " GitGutter" rm-blacklist)))

;;; smex
(for-package 'smex
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

;;; Visible mark
(for-package 'visible-mark (global-visible-mark-mode 1))
;; Face to use for mark is configured before loading package

;;; wgrep - write to files in search result buffer
(for-package 'wgrep
  (setq wgrep-enable-key "w")
  (setq wgrep-auto-save-buffer t))

;;; IBuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)

;;; Update frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (format "E: %s (%%I)" (abbreviate-file-name (buffer-file-name)))
                 "E: %b"))))
