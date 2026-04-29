(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)

(bind-key* "C-h" 'delete-backward-char)
(delete-selection-mode 1)

(setq warning-minimum-level :error)
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))
(setq byte-compile-warnings nil)
(setq byte-compile-warnings '(not obsolete))


(defconst my-backup-dir (locate-user-emacs-file "var/backup/"))
(defconst my-autosave-dir (locate-user-emacs-file "var/auto-save/"))

(unless (file-exists-p my-backup-dir)
  (make-directory my-backup-dir t))
(unless (file-exists-p my-autosave-dir)
  (make-directory my-autosave-dir t))

(setq make-backup-files t)
(setq backup-directory-alist `((".*" . ,my-backup-dir)))

(setq auto-save-default t)
(setq auto-save-file-name-transforms `((".*" ,my-autosave-dir t)))

(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package clipetty
  :ensure t
  :config
  (global-clipetty-mode 1)
  (setq select-enable-clipboard t)
  (setq yank-pop-change-selection t))

(use-package expand-region
  :ensure t
  :bind ("M-o" . er/expand-region))

(use-package centered-cursor-mode
  :ensure t
  :config
  (global-centered-cursor-mode 1))

(use-package whitespace
  :init
  (global-whitespace-mode 1)
  :config
  (setq whitespace-style '(face trailing))
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
  (setq delete-by-moving-to-trash t)
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

(use-package dash
  :ensure t)

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

(use-package magit-delta
  :ensure t
  :after magit
  :hook (magit-mode . magit-delta-mode))

(defun command (command-args)
  (interactive "sCommand:")
  (compilation-start (command-args) 'grep-mode))

(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

(add-to-list 'load-path user-emacs-directory)
(require 'lsp)


(setq file-name-handler-alist default-file-name-handler-alist)
(setq gc-cons-threshold (* 16 1024 1024))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
