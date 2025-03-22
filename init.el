;;; ====================
;;; 日本語環境と基本設定
;;; ====================
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

;; フォント設定（PlemolJP Console 使用）
(set-face-attribute 'default nil :family "PlemolJP Console" :height 120)

(global-display-line-numbers-mode t)
(global-set-key (kbd "C-h") 'delete-backward-char)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-font-lock-mode t)
(global-hl-line-mode +1)

;; ターミナル環境でのマウス有効化
(when (not (display-graphic-p))
  (xterm-mouse-mode 1))
  
;; ツールバーを非表示
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; メニューバーを非表示
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; スクロールバーも非表示（任意）
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; ====================
;;; パッケージ設定
;;; ====================
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; ====================
;;; 補完・検索関連
;;; ====================
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-g g" . consult-goto-line)))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;; ====================
;;; Corfu + Cape 補完システム
;;; ====================
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-on-exact-match nil)
  (tab-always-indent 'complete)
  :bind (:map corfu-map
              ("S-SPC" . corfu-insert))
  :init
  (global-corfu-mode +1))

;; LSP補完は Corfu に任せる
(with-eval-after-load 'lsp-mode
  (setq lsp-completion-provider :none))

;; ターミナル用 Corfu 拡張
(use-package corfu-terminal
  :after corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; Cape による補完ソース追加
(use-package cape
  :init
  (dolist (hook '(python-mode-hook c-mode-hook))
    (add-hook hook
              (lambda ()
                (add-to-list 'completion-at-point-functions #'eglot-completion-at-point))))
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

;;; ====================
;;; smartparens（括弧補完）
;;; ====================
(use-package smartparens
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-f" . sp-forward-symbol)
              ("C-M-b" . sp-backward-symbol)
              ("C-s-f" . sp-forward-sexp)
              ("C-s-b" . sp-backward-sexp)
              ("C-c (" . wrap-with-parens)
              ("C-c [" . wrap-with-brackets)
              ("C-c {" . wrap-with-braces)
              ("C-c '" . wrap-with-single-quotes)
              ("C-c \"" . wrap-with-double-quotes)
              ("M-]" . sp-unwrap-sexp)
              ("M-k" . sp-kill-sexp))
  :init
  (smartparens-global-mode t)
  (require 'smartparens-config)
  :config
  ;; 囲み用のラップ関数定義マクロ
  (defmacro def-pairs (pairs)
    `(progn
       ,@(cl-loop for (key . val) in pairs
                  collect
                  `(defun ,(intern (concat "wrap-with-" (symbol-name key) "s"))
                       (&optional arg)
                     (interactive "p")
                     (sp-wrap-with-pair ,val)))))
  (def-pairs ((paren . "(")
              (bracket . "[")
              (brace . "{")
              (single-quote . "'")
              (double-quote . "\""))))

;;; ====================
;;; org-mode 設定
;;; ====================
(use-package org-modern
  :custom
  (org-modern-progress '("○" "◔" "◑" "◕" "✅"))
  (org-modern-star '("●" "○" "■" "◆" "◇" "✿"))
  (org-ellipsis " ▼")
  :hook
  ((org-mode . org-modern-mode)
   (org-mode . org-indent-mode)
   (org-agenda-finalize . org-modern-agenda)))

;; カスタム TODO 状態を定義
(setq org-todo-keywords
      '((sequence "TODO" "WAIT" "SOME" "|" "DONE" "CANCELLED")))

;; TODO やチェックボックスの変更時に統計を自動更新
(add-hook 'org-after-todo-state-change-hook #'org-update-statistics-cookies)
(add-hook 'org-checkbox-statistics-hook #'org-update-statistics-cookies)

;; その他 org-mode 設定
(setq org-use-speed-commands t)
(setq org-directory (expand-file-name "c:/Users/Documents/org/"))
(setq org-agenda-files (list org-directory))
(setq org-agenda-window-setup 'current-window)
(define-key global-map "\C-ca" 'org-agenda)

;;; ====================
;;; Custom-generated variables（Emacsが自動追加する領域）
;;; ====================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-startup-folded t)
 '(package-selected-packages nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
