;; My init.el.

;; when edit init.el, you must run
;; emacs --batch -f batch-byte-compile init.el

;; this enables this running method
;; emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el

;; references
;; https://emacs-jp.github.io/tips/emacs-in-2020

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf cus-start
  :doc "define customization properties of builtins"
  :custom `((garbage-collection-messages     . t)
            (gc-cons-threshold               . 100000000)
            (fill-column                     . 65)
            (tab-width                       . 4)
            (create-lockfiles                . nil)
            (make-backup-files               . nil)
            (auto-save-default               . t)
            (use-dialog-box                  . nil)
            (column-number-mode              . t)
            (use-file-dialog                 . nil)
            (frame-resize-pixelwise          . t)
            (enable-recursive-minibuffers    . t)
            (history-length                  . 1000)
            (history-delete-duplicates       . t)
            (scroll-preserve-screen-position . t)
            (scroll-margin                   . 5)
            (scroll-conservatively           . 1)
            (next-screen-context-lines       . 5)
            (mouse-wheel-scroll-amount       . '(1 ((control) . 5)))
            (ring-bell-function              . 'ignore)
            (text-quoting-style              . 'straight)
            (truncate-lines                  . nil)
            (truncate-partial-width-windows  . nil)
            (menu-bar-mode                   . nil)
            (tool-bar-mode                   . nil)
            (scroll-bar-mode                 . nil)
            (fringe-mode                     . 10)
            (indent-tabs-mode                . nil)
            (inhibit-startup-message         . t)
            (inhibit-startup-screen          . t)
            (gc-cons-threshold               . ,(* gc-cons-threshold 100))
            (read-process-output-max         . ,(* 1024 1024)))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

(leaf simple
  :custom ((kill-ring-max     . 100)
           (kill-read-only-ok . t)
           (kill-whole-line   . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-lepvel  . nil)))

(leaf elisp-format
  :ensure t)

(leaf abbrev
  :blackout (abbrev-mode . " Abv"))

(leaf files
  :setq-default ((find-file-visit-truename . t)))

(leaf display-line-numbers
  :config (global-display-line-numbers-mode)) ;; 行番号を常に表示

(leaf delsel
  :custom ((delete-selection-mode . t)))

(leaf uniquify
  :custom ((uniquify-buffer-name-style . 'post-forward-angle-brackets)
           (uniquify-min-dir-content . 1)
           (funiquify-ignore-buffers-re . "*[^*]+*")))

(leaf time
  :custom ((display-time-string-forms . '(month "/" day "(" dayname ") " 24-hours ":" minutes))
           (display-time-mode . t)))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 0.1))
  :global-minor-mode global-auto-revert-mode)

(leaf autoinsert
  :doc "automatic mode-dependent insertion of text into new files"
  :custom ((auto-insert-mode . t)))

(leaf paren
  :custom-face (show-paren-match . '((t
                                      (:weight regular
                                               :background "#44475a"
                                               :underline "$ffff00"))))
  :custom ((show-paren-delay . 0.0)
           (show-paren-mode  . t)))

(leaf save-place-mode
  :doc "automatically save place in files"
  :custom ((save-place-mode . t)))

(leaf windmove
  :custom (windmove-wrap-around . t)
  :bind (("C-M-h" . windmove-left)
         ("C-M-k" . windmove-up)
         ("C-M-j" . windmove-down)
         ("C-M-l" . windmove-right)))

(leaf gcmh
  :ensure t
  :custom (gcmh-verbose . t)
  :config (gcmh-mode 1))

(leaf doom-themes
  :ensure t
  :require t
  :custom ((doom-themes-enable-italic . t)
           (doom-themes-enable-bold   . t))
  :custom-face ((doom-modeline-bar . '((t
                                        (:background "#6272a4")))))
  :config (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  (leaf doom-modeline
    :ensure t
    :custom ((doom-modeline-buffer-file-name-style . 'truncate-with-project)
             (doom-modeline-icon . t)
             (doom-modeline-major-mode-icon . nil)
             (doom-modeline-minor-modes . nil))
    :hook ((after-init-hook . doom-modeline-mode))
    :config (set-cursor-color "cyan")))

;; (leaf modus-themes
;;   :ensure t
;;   :require t
;;   :custom
;;   ((modus-themes-italic-constructs . t)
;;    (modus-themes-bold-constructs . t)
;;    (modus-themes-region . '(bg-only no-extend))
;;    (modus-themes-syntax . 'nil)
;;    (modus-themes-diffs . 'deuteranopia)
;;    )
;;   :config
;;   (modus-themes-load-themes)
;;   (modus-themes-load-vivendi))

(leaf smartparens
  :ensure t
  :hook (after-init-hook . smartparens-global-strict-mode) ; strictモードを有効化
  :require smartparens-config
  :custom ((electric-pair-mode . nil))) ; electirc-pair-modeを無効化

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

(leaf whitespace
  :ensure t
  :commands whitespace-mode
  :bind ("C-c W" . whitespace-cleanup)
  :custom ((whitespace-style . '(face
                                trailing
                                tabs
                                spaces
                                empty
                                space-mark
                                tab-mark))
           (whitespace-display-mappings . '((space-mark ?\u3000 [?\u25a1])
                                            (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
           (whitespace-space-regexp . "\\(\u3000+\\)")
           (whitespace-global-modes . '(emacs-lisp-mode shell-script-mode sh-mode python-mode org-mode))
           (global-whitespace-mode . t))

  :config
  (set-face-attribute 'whitespace-trailing nil
                      :background "Black"
                      :foreground "DeepPink"
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                      :background "Black"
                      :foreground "LightSkyBlue"
                      :underline t)
  (set-face-attribute 'whitespace-space nil
                      :background "Black"
                      :foreground "GreenYellow"
                      :weight 'bold)
  (set-face-attribute 'whitespace-empty nil
                      :background "Black"))

(leaf mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

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
           (company-idle-delay            . 0) ;; 補完の遅延なし
           (company-minimum-prefix-length . 1) ;; 1文字から補完開始
           (company-transformers          . '(company-sort-by-occurrence))
           (global-company-mode           . t)
           (company-selection-wrap-around . t)))

(leaf flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode)
  :custom ((flycheck-display-errors-delay . 0.3)
           (flycheck-indication-mode . 'left-margin)) ;terminalで使うので、fringeではなくmarginに警告を表示
  :config (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode) ; flycheckのみでmarginを使用
    (leaf flycheck-inline
      :ensure t
      :hook (flycheck-mode-hook . flycheck-inline-mode)))

(leaf lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :config
  :custom ((lsp-keymap-prefix                  . "C-c l")
           (lsp-log-io                         . t)
           (lsp-keep-workspace-alive           . nil)
           (lsp-document-sync-method           . 2)
           (lsp-response-timeout               . 5)
           (lsp-enable-file-watchers           . nil))
  :hook (lsp-mode-hook . lsp-headerline-breadcrumb-mode)
  :init (leaf lsp-ui
          :ensure t
          :after lsp-mode
          :custom ((lsp-ui-doc-enable            . t)
                   (lsp-ui-doc-position          . 'at-point)
                   (lsp-ui-doc-header            . t)
                   (lsp-ui-doc-include-signature . t)
                   (lsp-ui-doc-max-width         . 150)
                   (lsp-ui-doc-max-height        . 30)
                   (lsp-ui-doc-use-childframe    . nil)
                   (lsp-ui-doc-use-webkit        . nil)
                   (lsp-ui-peek-enable           . t)
                   (lsp-ui-peek-peek-height      . 20)
                   (lsp-ui-peek-list-width       . 50))
          :bind ((lsp-ui-mode-map ([remap xref-find-definitions] .
                                   lsp-ui-peek-find-definitions)
                                  ([remap xref-find-references] .
                                   lsp-ui-peek-find-references))
                 (lsp-mode-map ("C-c s" . lsp-ui-sideline-mode)
                               ("C-c d" . lsp-ui-doc-mode)))
          :hook ((lsp-mode-hook . lsp-ui-mode))))

;; run this command
;; npm install -g pyright
(leaf lsp-pyright
  :ensure t
  :hook (python-mode-hook . (lambda ()
                              (require 'lsp-pyright)
                              (lsp-deferred))))

(leaf blacken
   :ensure t
   :custom ((blacken-line-length . 119)               ; 1行の流さを119文字まで許可
            (blacken-skip-string-normalization . t))) ; 文字リテラルの「'」を「"」に変更しないように抑制

;; run this command
;; npm install -g pyright
(leaf py-isort :ensure t)

(leaf shell-pop
  :ensure t
  :bind* (("C-t" . shell-pop))
  :config (custom-set-variables '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda
                                                                                           nil
                                                                                           (ansi-term
                                                                                            shell-pop-term-shell)))))
                                '(shell-pop-window-size 30)
                                '(shell-pop-full-span t)
                                '(shell-pop-window-position "bottom"))
  ;; 終了時のプロセス確認を無効化
  (defun set-no-process-query-on-exit ()
    (let ((proc (get-buffer-process (current-buffer))))
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil))))
  (add-hook 'term-exec-hook 'set-no-process-query-on-exit))

(leaf neotree
  :ensure t
  :bind ([f8] . neotree-toggle))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indent-guides-suppress-auto-error t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
