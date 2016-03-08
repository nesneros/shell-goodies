(require 'dash)
(defconst is-darwin (string= system-type "darwin"))
(defconst is-linux (string= system-type "gnu/linux"))

(defun os-open-file(file)
  (cond (is-darwin (call-process "open" nil 0 nil file))
        (is-linux (call-process "xdg-open" nil 0 nil file))))

;; If coreutils is installed tell emacs to use gls as ls in dired mode
(--when-let (expand-file-name "bin/gls" (or (getenv "BREW_PREFIX") "/usr/local"))
  (when (file-exists-p it)
    (setq insert-directory-program it)))

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (os-open-file file)))

(use-package dired
  :demand
  :bind (:map
         dired-mode-map
         ("C-c o" . dired-open-file)))

(use-package wdired
  :commands wdired-mode
  :init
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-allow-to-redirect-links t))

(use-package dired+
  :demand t
  :ensure t)

(use-package dired-k
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired-hacks packages
;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package dired-rainbow
;;   :ensure t)

(use-package dired-ranger
  :ensure t)

;; (use-package dired-subtree
;;   :ensure t)


