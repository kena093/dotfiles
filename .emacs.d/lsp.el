(use-package go-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package eglot
  :ensure nil
  :hook
  ((go-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (python-mode . eglot-ensure)))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.1)
  (setq corfu-auto-prefix 1)
  (setq corfu-cycle t)
  (unless (display-graphic-p)
    (use-package popon
      :ensure t)
    (use-package corfu-terminal
      :ensure t
      :config
      (corfu-terminal-mode 1))))

(provide 'lsp)
