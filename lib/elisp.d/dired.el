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

;; If coreutils is installed tell emacs to use gls as ls in dired mode
(let* ((brew-prefix (or (getenv "BREW_PREFIX") "/usr/local"))
       (ls-prog (expand-file-name "/bin/gls" brew-prefix)))
  (if (file-exists-p ls-prog)
      (setq insert-directory-program ls-prog)))
