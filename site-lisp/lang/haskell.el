(use-package haskell-mode
  :ensure t
;;  :config (progn (add-hook 'haskell-mode-hook #'lsp))
  :hook (haskell-mode . eglot-ensure)
  :after eglot)

(provide 'lang:haskell)
;;; haskell.el ends here
