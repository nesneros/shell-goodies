;; -*- mode: Emacs-Lisp; -*-

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

;;; My Key bindings. Emacs convention is that \C-c[a-zA-A] is reserve for user
(global-set-key "\C-cf"              'ffap)         ; find file at point
(global-set-key "\M-/"               'hippie-expand) ; replace std Emacs expand key
(global-set-key "\C-x\C-b"           'ibuffer)   ; replaces std buffer list
(global-set-key (kbd "C-c C-s")      'ag)
(global-set-key (kbd "<M-s-up>")     'buf-move-up)
(global-set-key (kbd "<M-s-down>")   'buf-move-down)
(global-set-key (kbd "<M-s-left>")   'buf-move-left)
(global-set-key (kbd "<M-s-right>")  'buf-move-right)

;;; Some mode mappings
(add-to-list 'auto-mode-alist '("\\.zsh\\(-theme\\)?\\'" . shell-script-mode))

;;; Backup settings
(let* ((emacs-dir (expand-file-name "~/.emacs.d"))
       (backup-dir (expand-file-name "backup" emacs-dir)))

  ;; Create the directories if they doesn't exist
  (unless (file-directory-p emacs-dir)
    (make-directory emacs-dir)
    (set-file-modes emacs-dir 448))

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

;; From http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;;; Git key bindings
(defvar git-command-map (make-sparse-keymap) "Git keymap")
(global-set-key (kbd "s-i") git-command-map)

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

;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(defmacro for-package (package &rest body)
  (if (package-installed-p (eval package))
      (while body
        (eval (pop body)))
    (message "*** Package '%s' is not installed" (eval package))))

;;; Packages installed by the package management should not be used before the package management is initialized

;;; Aggresive indent
(for-package 'aggressive-indent
             (global-aggressive-indent-mode 1)
             ;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
             (global-set-key (kbd "C-c ai") 'aggressive-indent-mode))

;;; Dash
(eval-after-load "dash" '(dash-enable-font-lock))

;;; Flycheck
(for-package 'flycheck
             (add-hook 'flycheck-error-list-after-refresh-hook
                       '(lambda () (-when-let (window (flycheck-get-error-list-window t))
                                     (with-selected-window window
                                       (fit-window-to-buffer window 30)))))
             (add-hook 'after-init-hook #'global-flycheck-mode))

;;; Flyspell
(eval-after-load "flyspell"
  '(progn
     (add-hook 'text-mode-hook 'flyspell-mode)
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;;; git-gutter+
(for-package 'git-gutter+
             (global-git-gutter+-mode +1)
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
             (define-key git-command-map (kbd "U") 'git-gutter+-unstage-whole-buffer))

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

;;; Projectile
(for-package 'projectile
             (global-set-key (kbd "s-p") 'projectile-command-map)
             (setq projectile-tags-file-name ".tags")
             (projectile-global-mode))

;;; Project Explorer
(for-package 'project-explorer
             (define-key projectile-command-map (kbd "s-p") 'project-explorer-toggle))

;;; smex
(for-package 'smex
             (smex-initialize)
             (global-set-key (kbd "M-x") 'smex)
             (global-set-key (kbd "M-X") 'smex-major-mode-commands))

;;; git-blame
;;;(autoload 'git-blame-mode "git-blame"
;;;  "Minor mode for incremental blame for Git." t)

;;; ace jump
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

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
             (define-key git-command-map (kbd "s-i") 'magit-status)
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
             (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
             )


;;; Visible mark
(for-package 'visible-mark (global-visible-mark-mode 1))
;; Face to use for mark is configured before loading package

;;; wgrep - write to files in search result buffer
(for-package 'wgrep
             (setq wgrep-enable-key "w")
             (setq wgrep-auto-save-buffer t))

;;; IBuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)

(require 'server)
(or (server-running-p)
    (server-start))

;;; Update frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (format "E: %s (%%I)" (abbreviate-file-name (buffer-file-name)))
                 "E: %b"))))

(setq is-darwin (string= system-type "Darwin"))
(setq is-linux (string= system-type "gnu/linux"))

(defun os-open-file(file)
  (cond (is-darwin (call-process "open" nil 0 nil file))
        (is-linux (call-process "xdg-open" nil 0 nil file))))


;;; Dired
(setq wdired-allow-to-change-permissions t)
(setq wdired-allow-to-redirect-links t)
(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (os-open-file file)))
(eval-after-load "dired+" '(define-key dired-mode-map "\C-co" 'dired-open-file))

(eval-after-load "icicles" '(icy-mode))

;; If coreutils is installed tell emacs to use gls as ls in dired mode
(let* ((brew-prefix (or (getenv "BREW_PREFIX") "/usr/local"))
       (ls-prog (expand-file-name "/bin/gls" brew-prefix)))
  (if (file-exists-p ls-prog)
      (setq insert-directory-program ls-prog)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-use-system-font t)
 '(magit-am-arguments (quote ("--3way")))
 '(magit-commit-arguments nil)
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
