;;; package --- Summary:
;;; Commentary:
;;; Code:

(use-package elisp-mode
  :mode ("\\.el'" . tuareg-mode)
  :after flycheck)

(use-package paredit
  :ensure t
  :hook
  (elisp-mode . turn-on-paredit))

(provide 'lang:elisp)
;;; elisp.el ends here
