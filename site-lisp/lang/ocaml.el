;;; package --- Summary:
;;; Commentary:
;;; Code:

(defun in-nix-shell-p ()
  "Check if we run in Nix shell."
  (string-equal (getenv "IN_NIX_SHELL") "1"))

(use-package tuareg
  :ensure t
  :mode ("\\.ml[ily]?$" . tuareg-mode))

(defvar custom/merlin-site-elisp (getenv "MERLIN_SITE_LISP"))
(defvar custom/utop-site-elisp (getenv "UTOP_SITE_LISP"))
(defvar custom/ocp-site-elisp (getenv "OCP_SITE_LISP"))

(use-package merlin
  :if (and custom/merlin-site-elisp
           (in-nix-shell-p))
  :after flycheck
  :load-path custom/merlin-site-elisp
  :hook
  (tuareg-mode . merlin-mode)
  (merlin-mode . company-mode)
  :config
  (define-key evil-motion-state-map (kbd "g t") 'merlin-type-enclosing)
  (define-key evil-motion-state-map (kbd "g d") 'merlin-document)
  :custom
  (merlin-command "ocamlmerlin"))

(use-package utop
  :if (and custom/utop-site-elisp
           (in-nix-shell-p))
  :load-path custom/utop-site-elisp
  :hook
  (tuareg-mode . utop-minor-mode))

(use-package ocp-indent
  :if (and custom/ocp-site-elisp
           (in-nix-shell-p))
  :load-path custom/ocp-site-elisp
  )

(provide 'lang:ocaml)
;;; ocaml.el ends here
