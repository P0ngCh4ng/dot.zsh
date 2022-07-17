;; load-pathを追加する関数を定義

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

(setenv "LANG" "en_US.UTF-8")
;; 引数ディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")
;;  カスタムファイルを別ファイルにする
(custom-set-variables '( custom-file(locate-user-emacs-file "custom.el")))
;; カスタムファイルが存在しない場合は作成する
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;; カスタムファイルを読み込む
(load custom-file)
(require 'package) 			;package.elを有効化
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
(leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))

(load-theme 'zenburn t)
(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)
(exec-path-from-shell-initialize)
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(custom-set-variables '(default-tab-width 4))
;; 更新されたファイルを自動的に読み込み直す
(global-auto-revert-mode t)
(global-hl-line-mode t)
(custom-set-faces
 '(hl-line ((t (:background "SteelBlue4")))))
(global-set-key (kbd "C-x o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; paren-mode :対応する括弧を強調して表示する
(custom-set-variables '(show-paren-delay 0))		;表示までの秒数。　初期値は0.125
(show-paren-mode t )			;有効化
;; parenのスタイル : expressionは括弧内も強調表示
(custom-set-variables '(show-paren-style 'expression))
;; フェイスを変更する
(set-face-attribute 'show-paren-match nil
:background 'unspecified)
(set-face-underline 'show-paren-match "red")
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(leaf php-mode
  :ensure t
  )
(leaf rust-mode
  :ensure t
  :custom rust-format-on-save t)
(leaf cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(leaf lsp-mode
  :ensure t
  :hook (rust-mode . lsp)
  (Typescript-mode-hook . lsp)
  :bind ("C-c h". lep-describe-thing-at-point)
  :custom (lsp-rust-server 'rust-analyzer))
(leaf lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(leaf company
  :ensure t)
(global-company-mode) ; 全バッファで有効にする 
(setq lsp-completion-provider t)

(leaf typescript-mode
  :ensure t
  :custom
  (typescript-indent-level . 2)
)

(electric-pair-mode t)

(custom-set-variables '( flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(setq make-backup-files nil)

(leaf projectile
  :ensure t t
  :init
  :config
  (setq projectile-mode-line-prefix " Prj")
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(when (require 'projectile nil t)
  (projectile-mode)
  (add-to-list
   'projectile-globally-ignored-directories
   "node-modules")
  (setq projectile-enable-caching t))
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(when (equal window-system 'mac)
  (setq mac-function-modifier 'meta)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key (kbd "s-+") 'text-scale-adjust)
  (global-set-key (kbd "s--") 'text-scale-adjust))

(leaf haskell-mode
  :ensure t
  :after t
  :defvar flycheck-error-list-buffer
  :custom
  (haskell-hoogle-command . nil)
  (haskell-hoogle-url . "https://www.stackage.org/lts/hoogle?q=%s")
  :init
  (defun haskell-repl-and-flycheck ()
    (interactive)
    (delete-other-windows)
    (flycheck-list-errors)
    (haskell-process-load-file)
    (haskell-interactive-switch)
    (split-window-below)
    (other-window 1)
    (switch-to-buffer flycheck-error-list-buffer)
    (other-window 1))
  :bind (:haskell-mode-map
         ("M-i" . stylish-haskell-toggle)
         ("C-M-z" . haskell-repl-and-flycheck)
         ("C-c C-b" . haskell-hoogle)
         ("C-c C-c" . haskell-session-change-target)
         ("C-c C-l" . haskell-process-load-file)
         ("C-c C-z" . haskell-interactive-switch)
         ([remap indent-whole-buffer] . haskell-mode-stylish-buffer))
  :config
  (add-to-list 'safe-local-variable-values '(haskell-indent-spaces . 4))
  (add-to-list 'safe-local-variable-values '(haskell-process-use-ghci . t)))

(leaf lsp-haskell
    :ensure t
    :hook (haskell-mode-hook . lsp)
    :custom
    ;; フォーマッターをfourmoluにします。fourmoluのデフォルト値も気に入らないがカスタマイズ出来るだけマシ。
    (lsp-haskell-formatting-provider . "fourmolu")
    ;; 補完時にスニペット展開(型が出てくるやつ)を行わないようにします。
    (lsp-haskell-completion-snippets-on . nil)
    ;; 関数補完からの自動importはcompanyから誤爆する可能性が高すぎるので無効化します。
    (lsp-haskell-plugin-ghcide-completions-config-auto-extend-on . nil)
    ;; importされたものが出てくる機能自体の思想は分かり易くて良いのですが、スクロール周りがすごい面倒になるので無効化。
    (lsp-haskell-plugin-import-lens-code-lens-on . nil)
    :defun
    lsp-code-actions-at-point
    lsp:code-action-title
    )
(leaf haskell-customize
    :defvar haskell-stylish-on-save
    :init
    (eval-and-compile
      (defun stylish-haskell-enable ()
        "保存したときに自動的にstylish-haskellを適用する。"
        (interactive)
        (setq-local haskell-stylish-on-save t))
      (defun stylish-haskell-disable ()
        (interactive)
        (setq-local haskell-stylish-on-save nil))
      (defun stylish-haskell-toggle ()
        (interactive)
        (setq-local haskell-stylish-on-save (not haskell-stylish-on-save)))
      (defun stylish-haskell-setup ()
        "プロジェクトディレクトリにstylish-haskellの設定ファイルがある場合、保存したときに自動的にstylish-haskellを適用する。"
        (if (locate-dominating-file default-directory ".stylish-haskell.yaml")
            (stylish-haskell-enable)
          (stylish-haskell-disable))))
    :hook (haskell-mode-hook . stylish-haskell-setup))
  (leaf haskell-interactive-mode
    :after t
    :defvar haskell-interactive-mode-map)
  (leaf haskell-cabal
    :defvar haskell-cabal-mode-map)


(leaf helm
  :ensure t
  ::config
  )
