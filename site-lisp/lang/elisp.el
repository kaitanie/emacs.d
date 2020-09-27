;;; package --- Summary:
;;; Commentary:
;;; Code:

(use-package elisp-mode
  :mode ("\\.el'" . elisp-mode)
  :after flycheck)

(use-package paredit
  :ensure t
  :after elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode))

(provide 'lang:elisp)
;;; elisp.el ends here
