;; load-pathを追加する関数を定義

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))
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

;; 更新されたファイルを自動的に読み込み直す
(global-auto-revert-mode t)
;; 現在行のハイライト
(defface my-hl-line-face
  ;; 背景がdarkなら背景色を紺に
  '((((class color)(background dark))
     (:background "NavyBlue" t))
    ;; 背景がlightならば背景色を青に
    (((class color)(background light))
     (:background "LightSkyBlue"t))
    (t (:bold t)))
  "hi-line's my face")
(custom-set-variables '( hl-line-face 'my-hl-line-face))
(global-hl-line-mode t)

;; paren-mode :対応する括弧を強調して表示する
(custom-set-variables '(show-paren-delay 0))		;表示までの秒数。　初期値は0.125
(show-paren-mode t )			;有効化
;; parenのスタイル : expressionは括弧内も強調表示
(custom-set-variables '(show-paren-style 'expression))
;; フェイスを変更する
(set-face-attribute 'show-paren-match nil
:background 'unspecified)
(set-face-underline 'show-paren-match "red")

(leaf php-mode
  :ensure t
  )

(electric-pair-mode t)

(custom-set-variables '( flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(custom-set-variables '(make-backup-files t)) 
