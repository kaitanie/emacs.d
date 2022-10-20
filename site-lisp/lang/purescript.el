;;; package --- Summary:
;;; Commentary:
;;; Code:
;;  -*- lexical-binding: t; -*-

(use-package purescript-mode
  :mode ("\\.purs" . purescript-mode)
  :init
  (add-to-list 'load-path "~/.emacs.d/vendor/new-purescript-mode/")
  (add-to-list 'Info-default-directory-list "~/.emacs.d/vendor/new-purescript-mode/"))

(use-package dash
  :ensure t)

(use-package dash-functional
  :ensure t)

(use-package s
  :ensure t)

(use-package let-alist
  :ensure t)

(use-package seq
  :ensure t)

(use-package json
  :ensure t)

(use-package psc-ide
  :ensure t
  :after dash dash-functional company s flycheck let-alist seq json
  :hook
  (purescript-mode . psc-ide-mode)
  :bind
  ("<f11>" . psc-ide-goto-definition)
  ;; :init
  ;;  (add-to-list 'load-path "~/.emacs.d/vendor/psc-ide-emacs/")
  )

(provide 'lang:purescript)
;;; purescript.el ends here
