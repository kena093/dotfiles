;; -----------------------------------------------------------
;; 0. パッケージ管理と基本設定
;; -----------------------------------------------------------
;; macOS かつ、GUI版で起動している時だけ Command を Meta にする
(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; telescope代替
;; -----------------------------------------------------------
;; 5. 補完・検索インターフェース (Telescopeの代替)
;; -----------------------------------------------------------

;; 1. Vertico: 縦型補完UI (これがTelescopeの見た目部分)
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; 2. Orderless: あいまい検索 (スペース区切りで絞り込み)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; 3. Marginalia: 補完候補の横に詳細情報を表示
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :init
  ;; 【修正点】ここを :init にして、確実に変数を上書きします
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; consult-xrefの設定
  (setq consult-xref-preserve-buffer-source t)
  :bind (;; --- 標準的な置き換え ---
         ("C-s" . consult-line)          ; Swiper/Helm-swoop の代わり
         ("C-x b" . consult-buffer)      ; Ido/Helm-buffer の代わり
         ("M-y" . consult-yank-pop)      ; 貼り付け履歴

         ;; --- rexim (Tsoding) スタイルのキーバインド ---
         ;; 彼は "C-c h" (helm) プレフィックスを多用していましたが、
         ;; ここでは同じキーで Consult を呼び出すようにします。

         ("C-c r" . consult-recent-file) ; 最近開いたファイル
         ("C-c g g" . consult-git-grep)  ; Git grep
         ("C-c f" . consult-find)        ; find (locate)

         ;; 行ジャンプ (reximは M-g g を使うことが多いですが、consult-goto-line も M-g g が標準です)
         ("M-g g" . consult-goto-line)))
;; -----------------------------------------------------------
;; 4. フォントと表示設定
;; -----------------------------------------------------------
(set-face-attribute 'default nil
                    :family "JetBrains Mono NL"
                    :height 120) ;; 120 = 12pt

;; 行番号を表示 (変数を設定するだけでは表示されないので、モードをONにします)
(setq display-line-numbers-type t)
(global-display-line-numbers-mode t) ;; これが必要です
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -----------------------------------------------------------
;; 6. インバッファ補完 (Corfu)
;;    LSPの補完候補をVSCodeのようにポップアップ表示します
;; -----------------------------------------------------------
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                 ;; 自動補完を有効にする
  (corfu-auto-delay 0.1)         ;; 入力してからの遅延 (秒)
  (corfu-auto-prefix 2)          ;; 2文字入力で補完開始
  (corfu-cycle t)                ;; 候補リストをループさせる
  :init
  (global-corfu-mode))

;; -----------------------------------------------------------
;; 7. 各言語用モードの準備
;; -----------------------------------------------------------
;; Rust用モード (標準では入っていないため追加)
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")
;; C, C++, Python はEmacs標準のモードが使われます

(use-package magit
  :ensure t
  :bind (;; rexim のキーバインド
         ("C-c m s" . magit-status) ; Gitステータス画面を開く
         ("C-c m l" . magit-log))   ; Gitログを見る
  :config
  ;; 【重要】rexim 特有の設定
  ;; Magit操作後に、開いているファイルのバッファを自動更新(revert)しないようにする
  (setq magit-auto-revert-mode t))

;; (オプション) git diff をシンタックスハイライトで見やすくする
(use-package magit-delta
  :ensure t
  :after magit
  :hook (magit-mode . magit-delta-mode))

;; カッコの構造を維持する（Lispを書くなら必須級）
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (scheme-mode . paredit-mode)))

;; 複数カーソル (VSCodeのCtrl+d的なやつ)
(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; 行の入れ替え (M-p / M-n)
(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

;; スニペットエンジン
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; -----------------------------------------------------------
;; テーマ設定 (Doom版 Ayu Mirage)
;; モダンなプラグイン(Vertico/Magit等)との相性が抜群です
;; -----------------------------------------------------------
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; テーマをロード
  (load-theme 'doom-ayu-mirage t)

  ;; (オプション) モデルラインを見やすくする
  (doom-themes-org-config))

;; 行末の空白を削除 & 可視化
(use-package whitespace
  :init
  (global-whitespace-mode 1)
  :config
  (setq whitespace-style '(face trailing tabs spaces newline empty indentation space-after-tab space-before-tab))
  ;; 保存時に行末の空白を自動削除
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; -----------------------------------------------------------
;; 9. Dired (ファイラー) 設定
;; -----------------------------------------------------------
(use-package dired
  :ensure nil ;; Emacs標準機能のため nil
  :commands (dired dired-jump)
  :custom
  ;; リスト表示のオプション
  ;; -a: 隠しファイルも全て含める (omit-modeで隠すため一旦全取得)
  ;; -l: 詳細リスト表示
  ;; -h: ファイルサイズを読みやすく (1K, 1Mなど)
  (dired-listing-switches "-alh")

  ;; 画面分割時、もう片方のディレクトリをコピー・移動のデフォルト宛先にする
  ;; (これが非常に便利です)
  (dired-dwim-target t)

  ;; マウスでファイルをドラッグして外部アプリ(Finder等)に渡せるようにする
  (dired-mouse-drag-files t)

  :hook
  ;; Dired起動時に dired-omit-mode (不要なファイルを隠す機能) をオンにする
  (dired-mode . dired-omit-mode)

  :config
  (require 'dired-x))

;; -----------------------------------------------------------
;; 10. Tramp (リモート接続) 設定
;; -----------------------------------------------------------
(use-package tramp
  :ensure nil
  :custom
  ;; リモートファイルを編集する際、自動保存ファイル(#file#)を
  ;; リモート側ではなくローカルの /tmp に作成する。
  ;; これにより、SSH接続時の書き込みラグを軽減できます。
  (tramp-auto-save-directory "/tmp"))

;; 最近開いたファイルを保存する設定 (Consultを使うなら必須)
(use-package recentf
  :ensure nil ;; Emacs標準機能
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200) ;; 200個まで履歴を保存
  (recentf-auto-cleanup 'never)) ;; 履歴の自動削除を無効化（ssh先などが消えないように）

;; -----------------------------------------------------------
;; 選択範囲(リージョン)の文字列でプロジェクト検索する機能
;; (reximの C-x p s の再現 + モダン化)
;; -----------------------------------------------------------
(defun my/consult-grep-from-region ()
  "現在選択している文字列を使って consult-git-grep (または ripgrep) を開始します"
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                "")))
    ;; ripgrep (rg) がインストールされているなら consult-ripgrep を、
    ;; なければ consult-git-grep を使います
    (if (executable-find "rg")
        (consult-ripgrep nil text)
      (consult-git-grep nil text))))

;; キーバインド設定
(global-set-key (kbd "C-x p s") 'my/consult-grep-from-region)

;; -----------------------------------------------------------
;; 定義ジャンプ (Dumb Jump)
;; -----------------------------------------------------------
;;(use-package dumb-jump
;;  :ensure t
;;  :init
;;  ;; Emacs標準のジャンプ機能(xref)のエンジンとして dumb-jump を登録
;;  ;; これにより、M-. を押した時に dumb-jump が動くようになります
;;  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;
;;  :config
;;  ;; ripgrep (rg) を強制的に使う設定
;;  (setq dumb-jump-force-searcher 'rg)
;;  (setq dumb-jump-prefer-searcher 'rg)
;;
;;  ;; ホームディレクトリなどを検索対象に含めないための設定
;;  (setq dumb-jump-default-project ""))

;; -----------------------------------------------------------
;; C/C++ 用 LSP設定 (ミニマリスト設定)
;; -----------------------------------------------------------
(use-package eglot
  :ensure t
  ;; C/C++ モードで起動
  :hook ((c-mode c++-mode) . eglot-ensure)
  :config
  ;; Eglotが起動した直後に、余計な表示機能をオフにする設定
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; 1. エラー表示 (Flymake) を無効化
              ;;    これで赤い波線が出なくなります
              (flymake-mode -1)

              ;; 2. 補完ポップアップ (Corfu) をこのバッファだけ無効化
              ;;    これで勝手に候補が出なくなります
              (corfu-mode -1)

              ;; 3. ヒント表示 (Inlay Hints) を無効化
              (eglot-inlay-hints-mode -1)))

  ;; clangd の設定 (バックグラウンドでの解析はさせておく)
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "-j=4"
                    "--background-index"
                    "--header-insertion=never"))))

(setq delete-by-moving-to-trash t)
