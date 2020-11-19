;; (use-package rustic
;;   :ensure t
;;   :after eglot
;;   :config
;;   (setq rustic-lsp-server 'rls)
;;   (setq rustic-lsp-client 'eglot))

(use-package rust-mode
  :ensure t
  :after eglot)

(provide 'lang:rust)
;;; haskell.el ends here
