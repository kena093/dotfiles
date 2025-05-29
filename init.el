;; 日本語環境と基本設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)

(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      display-line-numbers-type 'relative
      use-short-answers t
      ring-bell-function 'ignore
      case-fold-search t
      show-trailing-whitespace t
      default-file-modes 420) ;; 0644

(global-display-line-numbers-mode t)
(global-set-key (kbd "C-h") 'delete-backward-char)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-font-lock-mode t)

;; ターミナル環境でのマウス有効化
(when (not (display-graphic-p))
  (xterm-mouse-mode 1))

;; フォント設定（GUI用）
(set-face-attribute 'default nil :family "HackGen Console NF" :height 160)

;; macOSのコマンドキーをMetaに
(setq mac-command-modifier 'meta)

;; 警告音もフラッシュも全て無効
(setq ring-bell-function 'ignore)

;; native-comp無効化
(setq native-comp-deferred-compilation nil)
(setq native-comp-jit-compilation nil)

;; フォントサイズを調整するキーバインド
(global-set-key (kbd "M-=") 'text-scale-increase)  ;; M-+（=キー）
(global-set-key (kbd "M--") 'text-scale-decrease)  ;; M-−
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))  ;; M-0でリセット

(require 'package)

;; パッケージリポジトリの設定（MELPAは必須）
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; 初期化
(package-initialize)

;; 初回のみ use-package をインストール
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 基本設定（推奨）
(require 'use-package)
(setq use-package-always-ensure t) ; 自動でパッケージをインストールする

;; which-key（キーバインド補助表示）
(use-package which-key
  :config
  (which-key-mode))

;; magit(gitクライアント)
(use-package magit
  :bind ("C-x g" . magit-status)) ; C-x g で magit を起動

;; doom-theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(## company consult doom-modeline doom-themes magit orderless puni
	rust-mode vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; company-mode: 補完の基本
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1))

;; 括弧系
(use-package puni
  :ensure t
  :config
  (puni-global-mode +1)  ;; 全バッファに自動で有効化

  ;; 必要ならグローバルキー設定
  (define-key global-map (kbd "C-c l") #'puni-mark-list-around-point)
  (define-key global-map (kbd "C-c x") #'puni-mark-sexp-around-point)
  (define-key global-map (kbd "C-c v") #'puni-expand-region)
  (define-key puni-mode-map (kbd "C-c s") #'puni-splice)
  (define-key global-map (kbd "C-c (") #'puni-wrap-round)
  (define-key global-map (kbd "C-c [") #'puni-wrap-square)
  (define-key global-map (kbd "C-c {") #'puni-wrap-curly))


;; puniと競合しなようにオフ
(electric-pair-mode 1)

;; rust-mode: Rustファイルに対応
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook (rust-mode . eglot-ensure))

;; eglot: 軽量LSPクライアント（Emacs内蔵 or MELPA版）
(use-package eglot
  :ensure t
  :commands eglot eglot-ensure
  :config
  ;; rust-analyzerを明示的に指定（macなどで必要な場合）
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer"))))

;; 保存時に rustfmt を使って整形
(setq rust-format-on-save t)

;; Emacsに Homebrew の PATH を認識させる
(add-to-list 'exec-path "/opt/homebrew/bin")
(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))

;; モードラインをモダンにする
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

(use-package consult
  :ensure t
  :bind (("C-s"     . consult-line)        ; バッファ内検索
         ("C-x b"   . consult-buffer)      ; バッファ一覧
         ("M-y"     . consult-yank-pop)    ; kill-ring を補完
         ("M-g g"   . consult-goto-line))) ; 行ジャンプ

