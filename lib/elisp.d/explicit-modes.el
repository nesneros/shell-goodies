;;; Rememeber mode for files with mode explicit set
(require 'cl)

;;; Get the auto mode for the current buffer
(defun get-auto-mode()
  (fset 'orig-set-auto-mode-0 (symbol-function 'set-auto-mode-0))
  (unwind-protect
      (catch 'auto-mode
        ;; set-auto-mode calls set-auto-mode-0 with the mode it finds.
        ;; Change temporarily the definition of set-auto-mode-0
        (fset 'set-auto-mode-0 (lambda (mode keep) (throw 'auto-mode mode)))
        (set-auto-mode))
    (fset 'set-auto-mode-0 (symbol-function 'orig-set-auto-mode-0))))

(defvar explicit-set-mode-alist '())
(setq savehist-additional-variables '(explicit-set-mode-alist))
(setq auto-mode-alist (append explicit-set-mode-alist auto-mode-alist))

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (and
                   buffer-file-name
                   (not (eq major-mode (get-auto-mode)))
                   (not (eq major-mode 'fundamental-mode)))
              (let ((c (cons buffer-file-name major-mode)))
                (setq explicit-set-mode-alist (cons c explicit-set-mode-alist))
                (setq auto-mode-alist (cons c auto-mode-alist))))))

;; trim explicit-set-mode-alist before saving hist
(add-hook 'savehist-save-hook
          (lambda ()
            (setq explicit-set-mode-alist (cl-remove-duplicates explicit-set-mode-alist :key #'car :test #'string= :from-end t))
            (setq auto-mode-alist         (cl-remove-duplicates auto-mode-alist         :key #'car :test #'string= :from-end t))))

