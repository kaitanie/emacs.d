(use-package haskell-mode
  :ensure t
;;  :config (progn (add-hook 'haskell-mode-hook #'lsp))
  :hook (haskell-mode . eglot-ensure)
  :after eglot)

;;(use-package ormolu
;;  :ensure t
;;  :hook (haskell-mode . ormolu-format-on-save-mode)
;;  :bind
;;  (:map haskell-mode-map
;;        ("C-c r" . ormolu-format-buffer)))

(provide 'lang:haskell)
;;; haskell.el ends here
