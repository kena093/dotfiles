(when (equal system-type 'darwin)
(setq mac-command-modifier 'meta))

(define-key key-translation-map [?\C-h] [?\C-?])

(setq warning-minimum-level :error)
(setq native-comp-async-report-warnings-errors nil)
(setq inhibit-startup-screen t)

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

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

(add-hook 'diff-hl-mode-on-hook
          (lambda ()
            (unless (display-graphic-p)
              (diff-hl-margin-local-mode))))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-xref-preserve-buffer-source t)
  :bind (
         ("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
	 ("C-c j" . consult-imenu)
         ("C-c r" . consult-recent-file)
         ("C-c g g" . consult-git-grep)
         ("C-c f" . consult-find)
         ("M-g g" . consult-goto-line)))

(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 140) ;; 120 = 12pt

(setq display-line-numbers-type t)
(global-display-line-numbers-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ayu-theme go-mode rainbow-delimiters which-key plan9-theme term/xterm zenburn-theme modus-themes eat smear-cursor beacon ultra-cursor embark expand-region ace-window vterm doom-themes yasnippet move-text multiple-cursors paredit magit-delta rust-mode corfu consult marginalia orderless vertico))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :init
  (global-corfu-mode))

(use-package corfu-terminal
  :ensure t
  :after corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package go-mode
  :ensure t)

(use-package magit
  :ensure t
  :bind (
         ("C-c m s" . magit-status)
         ("C-c m l" . magit-log))
  :config
  (setq magit-section-initial-visibility-alist
        '((unstaged . show)
          (staged . show)
          (untracked . show)))
  (setq magit-auto-revert-mode t))

(use-package magit-delta
  :ensure t
  :after magit
  :hook (magit-mode . magit-delta-mode))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (scheme-mode . paredit-mode)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package whitespace
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-style '(face trailing tabs newline empty indentation space-after-tab space-before-tab))
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-alh")
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  :hook
  (dired-mode . dired-omit-mode)
  :config
  (require 'dired-x))

(use-package tramp
  :ensure nil
  :custom
  (tramp-auto-save-directory "/tmp"))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 'never))

(defun my/consult-grep-from-region ()
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                "")))
    (if (executable-find "rg")
        (consult-ripgrep nil text)
      (consult-git-grep nil text))))

(global-set-key (kbd "M-s") 'my/consult-grep-from-region)

;;(use-package dumb-jump
;;  :ensure t
;;  :init
;;  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;  :config
;;  (setq dumb-jump-force-searcher 'rg)
;;  (setq dumb-jump-prefer-searcher 'rg)
;;  (setq dumb-jump-default-project ""))

(use-package eglot
  :ensure t
  :hook ((c-mode c++-mode python-mode go-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "-j=4"
                    "--background-index"
                    "--header-insertion=never")))
  (add-to-list 'eglot-server-programs
               `(python-mode . ("pylsp"))))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c l") #'flymake-show-project-diagnostics)

  (define-key eglot-mode-map (kbd "M-S-n") #'flymake-goto-next-error)
  (define-key eglot-mode-map (kbd "M-S-p") #'flymake-goto-prev-error))

(setq delete-by-moving-to-trash t)

(use-package vterm
  :ensure t
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm: %s")
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

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-:" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char-timer)
  :config
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank))
    t)
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  (defun avy-action-flyspell (pt)
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-auto-correct-word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?\s avy-dispatch-alist) 'avy-action-mark-to-char
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

(defvar my-music-scroll-pos 0 "現在のスクロール位置")
(defvar my-music-display-width 20 "モードラインに表示する文字数")
(defvar my-music-string "" "モードライン表示用の文字列キャッシュ")

(defun my-get-player-status ()
  "playerctlから情報を非同期っぽく取得（エラー時は空文字）"
  (let* ((cmd "playerctl metadata --format '{{ uc(status) }}: {{ artist }} | {{ title }} ' 2>/dev/null")
         ;; call-processで直接取得し、シェル起動のオーバーヘッドを減らす
         (raw (shell-command-to-string cmd)))
    (replace-regexp-in-string "\n$" "" raw)))

(defun my-update-music-string ()
  "タイマーから呼ばれる更新関数"
  (let* ((raw-status (my-get-player-status))
         (clean-status (if (or (string-empty-p raw-status)
                               (string-match-p "No players" raw-status))
                           ""
                         raw-status))
         (len (length clean-status)))
    (setq my-music-string
          (if (<= len my-music-display-width)
              clean-status
            (let* ((padded (concat clean-status " | "))
                   (padded-len (length padded))
                   (double-str (concat padded padded))
                   (result (substring double-str
                                      my-music-scroll-pos
                                      (+ my-music-scroll-pos my-music-display-width))))
              (setq my-music-scroll-pos (% (1+ my-music-scroll-pos) padded-len))
              result)))
    (force-mode-line-update t)))

(defvar my-music-timer
  (run-with-timer 0 1 #'my-update-music-string))

(defun my/eglot-toggle-with-highlight ()
  "Eglotとすべての色付けを完全にトグルします。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (if (bound-and-true-p eglot--managed-mode)
        (progn
          (eglot-shutdown (eglot-current-server))
          (font-lock-mode -1)
          (when (fboundp 'eglot-semantic-tokens-mode)
            (eglot-semantic-tokens-mode -1))
          (message "LSP & Highlight: OFF"))
      (progn
        (call-interactively 'eglot)
        (font-lock-mode 1)
        (message "LSP & Highlight: ON")))))

(global-set-key (kbd "C-c e") 'my/eglot-toggle-with-highlight)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c e") nil))

(defun my/copy-last-message-to-clipboard ()
  (interactive)
  (with-current-buffer "*Messages*"
    (save-excursion
      (goto-char (point-max))
      (forward-line -1)
      (let ((last-msg (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (kill-new last-msg)
        (message "Copied to clipboard: %s" last-msg)))))

(global-set-key (kbd "C-c t") 'my/copy-last-message-to-clipboard)

(defvar my-compile-slots (make-hash-table)
  "番号 -> (directory . command) のテーブル")

(defun my-compile-slot (num)
  (interactive "nSlot: ")
  (let* ((slot (gethash num my-compile-slots))
         (buf-name (format "*compilation-%d*" num))
         (compilation-buffer-name-function (lambda (_) buf-name))
         (slot (if current-prefix-arg nil slot)))
    (if slot
        (let ((default-directory (car slot)))
          (with-current-buffer (get-buffer-create buf-name)
            (setq-local compile-command (cdr slot))
            (recompile)))
      (let* ((dir default-directory)
             (cmd (read-shell-command "Compile command: " compile-command)))
        (puthash num (cons dir cmd) my-compile-slots)
        (let ((default-directory dir))
          (compile cmd))))))

(global-set-key (kbd "C-c 1") (lambda (p) (interactive "P") (let ((current-prefix-arg p)) (my-compile-slot 1))))
(global-set-key (kbd "C-c 2") (lambda (p) (interactive "P") (let ((current-prefix-arg p)) (my-compile-slot 2))))
(global-set-key (kbd "C-c 3") (lambda (p) (interactive "P") (let ((current-prefix-arg p)) (my-compile-slot 3))))
(global-set-key (kbd "C-c 4") (lambda (p) (interactive "P") (let ((current-prefix-arg p)) (my-compile-slot 4))))
