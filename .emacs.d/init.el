;; load-pathを追加する関数を定義

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(setenv "LANG" "en_US.UTF-8")
;; 引数ディレクトリとそのサブディレクトリをload-pathに追加

(add-to-load-path "elisp" "conf" "public_repos" "themes")

;;  カスタムファイルを別ファイルにする
(custom-set-variables '( custom-file(locate-user-emacs-file "custom.el")))
;; カスタムファイルが存在しない場合は作成する
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;; カスタムファイルを読み込む
(load custom-file)
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
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
    (leaf-keywords-init)))
(keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-?") 'help-for-help)
(define-key global-map [?¥] [?\\])
(setq inhibit-startup-screen t)

(leaf switch-window
  :ensure  t
  :defvar switch-window-shortcut-style
  :config
  (setq switch-window-shortcut-style 'qwerty)
  )

(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

(global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
(global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
(global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)

(global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)

(global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer)




;;macの設定
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



(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :defvar (flycheck-highlighting-mode  flycheck-check-syntax-automatically)
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)
 (setq flycheck-highlighting-mode 'lines  ;; columns symbolsm sexps lines
       flycheck-check-syntax-automatically '(save))
(leaf exec-path-from-shell
  :ensure t)
(exec-path-from-shell-initialize)
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(custom-set-variables '(default-tab-width 4))
;; 更新されたファイルを自動的に読み込み直す
(global-auto-revert-mode t)
(global-hl-line-mode t)
(leaf zenburn-theme
  :ensure t)
(load-theme 'zenburn)


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

(electric-pair-mode t)

(custom-set-variables '( flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(setq make-backup-files nil)
(setq default-directory "~/")
(setq command-line-default-directory "~/")

(leaf midnight
  :custom
  ((clean-buffer-list-delay-general . 1))
  :hook
  (emacs-startup-hook . midnight-mode))
(midnight-delay-set 'midnight-delay "4:30am")
(leaf uniquify
  :custom
  ((uniquify-buffer-name-style . 'post-forward-angle-brackets)
   (uniquify-min-dir-content   . 1))
  )

;;総合的なプログラミングの便利ツールの設定
(leaf lsp-mode
  :ensure t
  :hook( (rust-mode . lsp)
	 (typescript-mode-hook . lsp))
  :bind ("C-c h". lsp-describe-thing-at-point)
  :custom( (lsp-rust-server 'rust-analyzer))
  `((lsp-keymap-prefix                  . "C-c l")
    (lsp-inhibit-message                . t)
    (lsp-message-project-root-warning   . t)
    (create-lockfiles                   . nil)
    (lsp-signature-auto-activate        . t)
    (lsp-signature-doc-lines            . 1)
    (lsp-print-performance              . t)
    (lsp-log-io                         . t)
    (lsp-eldoc-render-all               . t)
    (lsp-enable-completion-at-point     . t)
    (lsp-enable-xref                    . t)
    (lsp-keep-workspace-alive           . nil)
    (lsp-enable-snippet                 . t)
    (lsp-server-trace                   . nil)
    (lsp-auto-guess-root                . nil)
    (lsp-document-sync-method           . 'lsp--sync-incremental)
    (lsp-document-sync-method           . 2)
    (lsp-diagnostics-provider           . :flycheck)
    (lsp-response-timeout               . 5)
    (lsp-idle-delay                     . 0.500)
    (lsp-enable-file-watchers           . nil)
    (lsp-completion-provider            . :capf)
    (lsp-headerline-breadcrumb-segments . '(symbols)))
  :commands
  (lsp lsp-deferred)
  :hook
  (prog-major-mode . lsp-prog-major-mode-enable)
  (lsp-mode-hook . lsp-ui-mode)
  (lsp-mode-hook . lsp-headerline-breadcrumb-mode)
  :init
  (leaf lsp-ui
    :emacs>= 26.1
    :ensure t
    :custom
    ((lsp-ui-doc-enable            . t)
     (lsp-ui-doc-deley             . 0.5)
     (lsp-ui-doc-header            . t)
     (lsp-ui-doc-include-signature . t)
     (lsp-ui-doc-position          . 'at-point)
     (lsp-ui-doc-max-width         . 150)
     (lsp-ui-doc-max-height        . 30)
     (lsp-ui-doc-use-childframe    . t)
     (lsp-ui-doc-use-webkit        . t)
     (lsp-ui-flycheck-enable       . t)
     ;; lsp-ui-sideline
     (lsp-ui-sideline-enable       .  t)
     (lsp-ui-sideline-ignore-duplicate . t)
     (lsp-ui-sideline-show-symbol  .  t)
     (lsp-ui-sideline-show-hover   .  t)
     (lsp-ui-sideline-show-diagnostics . t)
     (lsp-ui-sideline-show-code-actions . t)
     
     ;; lsp-ui-imenu
     (lsp-ui-imenu-enable . nil)
     (lsp-ui-imenu-kind-position  . 'top)
     (lsp-ui-peek-enable           . t)
     (lsp-ui-peek-peek-height      . 20)
     (lsp-ui-peek-list-width       . 50)
     (lsp-ui-peek-fontify          . 'on-demand) ;; never, on-demand, or always
     )
    :hook ((lsp-mode-hook . lsp-ui-mode))
    ))
(leaf company
  :ensure t)
(global-company-mode) ; 全バッファで有効にする 



(leaf helm
  :ensure t
  ::config
  )

(leaf projectile
  :bind
  ("s-p" . projectile-command-map)
  :init
  :config
  (progn
    (projectile-mode 1)
    ))

(leaf helm-projectile
  :ensure t)
(helm-projectile-on)

(leaf magit
  :ensure t)
(leaf magit-delta
      :ensure t
      :after magit
      :hook (magit-mode-hook))
;;言語ごとの設定



(leaf php-mode
  :ensure t
  )
(leaf rust-mode
  :ensure t
  :leaf-defer t
  :config
  (setq-default rust-format-on-save t))
(leaf cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))


(leaf typescript-mode
  :ensure t
  :custom
  (typescript-indent-level . 2)
)
(leaf prettier-js
  :ensure t)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook
  (lambda ()
    (add-hook 'after-save-hook 'prettier t t)))


(leaf python-mode
  :ensure t)

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
    :config
    (setq lsp-haskell-formatting-provider "fourmolu")
    (setq lsp-haskell-server-path "haskell-language-server-wrapper")
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

(put 'dired-find-alternate-file 'disabled nil)






;; Org-captureの設定

;; Org-captureを呼び出すキーシーケンス
(define-key global-map "\C-cc" 'org-capture)
;; Org-captureのテンプレート（メニュー）の設定
(leaf org-capture
  :commands org-capture
  :defvar org-capture-templates
  :config
  (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo.org" "INBOX")
	 "* TODO %?\n %i\n %a")
	("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
	 "* %?\nEntered on %U\n %i\n %a")
	("g" "Gabage" entry (file+headline "~/org/gabage.org" "gabage")
	 "*  Gabage %?\n %U\n %i\n %a")
	)))
;; メモをC-M-^一発で見るための設定
;; https://qiita.com/takaxp/items/0b717ad1d0488b74429d から拝借
(defun show-org-buffer (file)
  "Show an org-file FILE on the current buffer."
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat "~//org/" file))))
(global-set-key (kbd "C-M-^") (lambda () (interactive)
                                 (show-org-buffer "notes.org")))


;;ブログ書く関数
(defun make-new-blog-file (name)
  (interactive "s")
  (find-file (concat "~/p0ngch4ng.github.io/posts/" name ".md"))
  )
(global-set-key (kbd "C-c f b") 'make-new-blog-file)








