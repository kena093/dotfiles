(use-package go-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package eglot
  :ensure nil
  :hook
  ((python-mode . eglot-ensure)
   (c-mode      . eglot-ensure)
   (c++-mode    . eglot-ensure)
   (go-mode     . eglot-ensure))
  :config
  (add-hook 'before-save-hook 'eglot-format-buffer)
  (add-to-list 'eglot-server-programs `(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs `(go-mode . ("~/go/bin/gopls" "serve"))))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.1)
  (setq corfu-auto-prefix 2)
  (setq corfu-cycle t)
  (unless (display-graphic-p)
    (use-package popon
      :ensure t)
    (use-package corfu-terminal
      :ensure t
      :config
      (corfu-terminal-mode 1))))

(provide 'lsp)
