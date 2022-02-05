(setq debug-on-error t)
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

(defun meltyyyyy/add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (unless (file-exists-p default-directory)
          (make-directory default-directory))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(meltyyyyy/add-to-load-path "elisp" "conf")

(prog1 "prepare leaf"
  (prog1 "package"
    (custom-set-variables
     '(package-archives '(("org"   . "https://orgmode.org/elpa/")
                          ("melpa" . "https://melpa.org/packages/")
                          ("gnu"   . "https://elpa.gnu.org/packages/"))))
    (package-initialize))

  (prog1 "leaf"
    (unless (package-installed-p 'leaf)
      (unless (assoc 'leaf package-archive-contents)
        (package-refresh-contents))
      (condition-case err
          (package-install 'leaf)
        (error
         (package-refresh-contents)       ; renew local melpa cache if fail
         (package-install 'leaf))))

    (leaf leaf-keywords
      :ensure t
      :config (leaf-keywords-init)))

  (prog1 "optional packages for leaf-keywords"
    ;; optional packages if you want to use :hydra, :el-get,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t
      :custom ((el-get-git-shallow-clone  . t)))))

(column-number-mode t)

(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "#a9a9a9"
                    :background "#404040"
                    :height 0.9)

(show-paren-mode 1)

(setq inhibit-startup-message t)

(setq-default indent-tabs-mode nil)

(setq backup-directory-alist '((".*"."~/.ehist")))

(defalias 'quit 'kill-emacs)

(define-key global-map (kbd "C-z") 'undo)
(define-key global-map (kbd "C-t") 'suspend-frame)

(setq ring-bell-function 'ignore)

(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "C-x f") 'find-file)
(define-key global-map (kbd "C-<up>") 'scroll-down-line)
(define-key global-map (kbd "C-<down>") 'scroll-up-line)
(define-key global-map (kbd "C-c k") 'kill-buffer-and-window)

(leaf whitespace
  :ensure t
  :custom
  ((whitespace-style . '(face
                         trailing
                         tabs
                         ;; spaces
                         ;; empty
                         space-mark
                         tab-mark))
   (whitespace-display-mappings . '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
   (global-whitespace-mode . t)))

(leaf multi-term
  :ensure t
  :custom `((multi-term-program . ,(getenv "SHELL")))
  :preface
  (defun meltyyyyy/open-shell-sub (new)
   (split-window-below)
   (enlarge-window 5)
   (other-window 1)
   (let ((term) (res))
     (if (or new (null (setq term (dolist (buf (buffer-list) res)
                                    (if (string-match "*terminal<[0-9]+>*" (buffer-name buf))
                                        (setq res buf))))))
         (multi-term)
       (switch-to-buffer term))))
  (defun meltyyyyy/open-shell ()
    (interactive)
    (meltyyyyy/open-shell-sub t))
  (defun meltyyyyy/to-shell ()
    (interactive)
    (meltyyyyy/open-shell-sub nil))
  :bind (("C-^"   . meltyyyyy/to-shell)
         ("C-M-^" . meltyyyyy/open-shell)
         (:term-raw-map
          ("C-t" . other-window))))

(leaf highlight-indent-guides
  :ensure t
  :blackout t
  :hook (((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode))
  :custom (
           (highlight-indent-guides-method . 'character)
           (highlight-indent-guides-auto-enabled . t)
           (highlight-indent-guides-responsive . t)
           (highlight-indent-guides-character . ?\|)))

(leaf rainbow-delimiters
  :ensure t
  :hook
  ((prog-mode-hook       . rainbow-delimiters-mode)))

(leaf mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
            ("C-e" . mwim-end-of-code-or-line)))

(leaf py-isort :ensure t)

(leaf company
  :ensure t
  :leaf-defer nil
  :blackout company-mode
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("C-i" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-tooltip-limit         . 12)
           (company-idle-delay            . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers          . '(company-sort-by-occurrence))
           (global-company-mode           . t)
           (company-selection-wrap-around . t))

(leaf elpy
  :ensure t
  :init (elpy-enable)
  :config
   (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)
  :custom
   (elpy-rpc-python-command . "python3")
  :bind (elpy-mode-map ("C-c C-r f" . elpy-format-code))
  :hook ((elpy-mode-hook . flycheck-mode)))

