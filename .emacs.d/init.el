(when (equal system-type 'darwin)
(setq mac-command-modifier 'meta))

(define-key key-translation-map [?\C-h] [?\C-?])

(setq warning-minimum-level :error)
(setq native-comp-async-report-warnings-errors nil)

(setq-default cursor-type 'box)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

(defun my-backward-other-window ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x p") #'my-backward-other-window)

(put 'my-backward-other-window 'repeat-map
     (let ((map (make-sparse-keymap)))
       (define-key map "p" #'my-backward-other-window)
       map))

(repeat-mode 1)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package plan9-theme
  :ensure t
  :config
  (load-theme 'plan9 t))

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

(add-hook 'diff-hl-mode-on-hook
          (lambda ()
            (unless (display-graphic-p)
              (diff-hl-margin-local-mode))))

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
	 ("C-c j" . consult-imenu)

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
                    :family "JetBrains Mono"
                    :height 140) ;; 120 = 12pt

;; 行番号を表示 (変数を設定するだけでは表示されないので、モードをONにします)
(setq display-line-numbers-type t)
(global-display-line-numbers-mode t) ;; これが必要です
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimiters which-key plan9-theme term/xterm zenburn-theme modus-themes eat smear-cursor beacon ultra-cursor embark expand-region ace-window vterm doom-themes yasnippet move-text multiple-cursors paredit magit-delta rust-mode corfu consult marginalia orderless vertico))
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

 ;; ターミナル環境用の設定を追加
(use-package corfu-terminal
  :ensure t
  :after corfu
  :init
  (unless (display-graphic-p)    ;; GUIではない（ターミナルの）場合のみ有効化
    (corfu-terminal-mode 1)))

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
  (setq magit-section-initial-visibility-alist
        '((unstaged . show)
          (staged . show)
          (untracked . show)))
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

(use-package whitespace
  :init
  (global-whitespace-mode 1)
  :config
  (setq whitespace-style '(face trailing tabs newline empty indentation space-after-tab space-before-tab))
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
(global-set-key (kbd "M-s") 'my/consult-grep-from-region)

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
  :hook ((c-mode c++-mode python-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "-j=4"
                    "--background-index"
                    "--header-insertion=never")))
  (add-to-list 'eglot-server-programs
               `(python-mode . ("pylsp"))))

(setq delete-by-moving-to-trash t)

(use-package vterm
  ;; requirements: brew install cmake libvterm libtool
  :ensure t
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm: %s")
  ;; delete "C-h", add <f1> and <f2>
  (vterm-keymap-exceptions
   '("<f1>" "<f2>" "C-c" "C-x" "C-u" "C-g" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y" "M-w"))
  :config
  ;; Workaround of not working counsel-yank-pop
  ;; https://github.com/akermu/emacs-libvterm#counsel-yank-pop-doesnt-work
  (defun my/vterm-counsel-yank-pop-action (orig-fun &rest args)
    (if (equal major-mode 'vterm-mode)
        (let ((inhibit-read-only t)
              (yank-undo-function (lambda (_start _end) (vterm-undo))))
          (cl-letf (((symbol-function 'insert-for-yank)
                     (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
      (apply orig-fun args)))

  (advice-add 'counsel-yank-pop-action :around #'my/vterm-counsel-yank-pop-action))

(use-package eat
  :ensure t
  :bind ("C-c v" . eat-project-other-window)
  :config
  (setq eat-kill-ring-max 1000)
  (eat-eshell-mode 1)
  (eat-reload))

(use-package expand-region
  :ensure t
  :bind ("M-o" . er/expand-region))

;; Embarkのインストールと設定
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)	; 通常時も C-. でEmbarkを使えるようにする（便利です）
   ("C-:" . embark-dwim)		; 文脈に応じたアクション
   ("C-h B" . embark-bindings))		; キーバインド一覧
  :init
  ;; ミニバッファでC-oを押すとEmbarkが動くようにするなど
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char-timer)
  :config
  ;; -----------------------------------------------------------
  ;; 1. カスタムアクション関数の定義
  ;;    Avy読み込み後に定義されるようにここに書きます
  ;; -----------------------------------------------------------

  ;; 行削除 (K)
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  ;; 行コピー (W)
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  ;; 行ヤンク (Y)
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  ;; 行移動 (T)
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank))
    t)

  ;; マーク (SPC)
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  ;; スペルチェック (;)
  (defun avy-action-flyspell (pt)
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-auto-correct-word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  ;; Embark (.)
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; -----------------------------------------------------------
  ;; 2. キー割り当て (変数 avy-dispatch-alist の編集)
  ;; -----------------------------------------------------------
  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?\s avy-dispatch-alist) 'avy-action-mark-to-char ; SPCキーは ?\s
        (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell
        (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(defun my-recenter (&rest _) (recenter))
(advice-add 'scroll-up-command :after #'my-recenter)
(advice-add 'scroll-down-command :after #'my-recenter)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  (setq which-key-popup-type 'minibuffer)

  (which-key-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))
