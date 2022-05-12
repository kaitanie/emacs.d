;;; package --- Summary:
;;; Commentary:
;;; Code:

(use-package lfe-mode
  :ensure t
  :mode ("\\.lfe'" . lfe-mode)
  :after flycheck)

(use-package erlang
  :ensure t
  :mode ("\\.erl'" . erlang-mode)
  :after flycheck)

;;(use-package paredit
;;  :ensure t
;;  :after elisp-mode
;;  :config
;;  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
;;  ;; enable in the *scratch* buffer
;;  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
;;  (add-hook 'ielm-mode-hook #'paredit-mode)
;;  (add-hook 'lisp-mode-hook #'paredit-mode))

(provide 'lang:erlang)
;;; erlang.el ends here
