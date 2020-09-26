;; (use-package purescript-mode-autoloads
;;   :mode "\\.purs\\'"
;;   )

;; (use-package psc-ide-mode
;;   :after purescript-mode-autoloads
;;   :mode "\\.purs\\'"
;;   :config (progn (flycheck-mode)
;;                  (turn-on-purescript-indentation)))

;; (global-set-key (kbd "M-j") 'windmove-down)
;; (global-set-key (kbd "M-k") 'windmove-up)
;; (global-set-key (kbd "M-h") 'windmove-left)
;; (global-set-key (kbd "M-l") 'windmove-right)

;;(setq magit-auto-revert-mode nil)
;;(setq magit-last-seen-setup-instructions "1.4.0")
;;(require 'magit-gitflow)

;;(menu-bar-mode -1)

;;(global-hl-line-mode 1)

;;(projectile-mode +1)
;;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Haskell
;;--(require 'haskell-mode)
;;--(require 'haskell-session)
;;--(require 'haskell-indentation)

;; (defun my-haskell-mode-hook ()
;;    (haskell-indentation-mode -1) ;; turn off, just to be sure
;;    (yas-minor-mode 1) ; for adding require/use/import
;;    (haskell-indent-mode 1)       ;; turn on indent-mode

;;  ;;  (turn-on-haskell-simple-indent)
;;    (haskell-indentation)

;;    ;; further customisations go here.  For example:
;;    (setq locale-coding-system 'utf-8 )
;;    (flyspell-prog-mode)  ;; spell-checking in comments and strings
;;    ;; etc.

;;    )

;;(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

;;--(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;--(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; JSX mode
;;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
;;(autoload 'jsx-mode "jsx-mode" "JSX mode" t)
;;(add-hook 'jsx-mode-hook
;;        (lambda () (setq forward-sexp-function nil)))
;; (add-hook 'javascript-mode-hook (lambda () (setq forward-sexp-function nil)))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

;; LSP
;; (use-package flycheck
;;   :ensure t
;;   :init
;;   (global-flycheck-mode t))
;; (use-package yasnippet
;;   :ensure t)
;; (use-package lsp-mode
;;   :ensure t
;;   :hook (haskell-mode . lsp)
;;   :commands lsp)
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)
;; (use-package lsp-haskell
;;  :ensure t
;;  :config
;;  (setq lsp-haskell-process-path-hie "ghcide")
;;  (setq lsp-haskell-process-args-hie '())
;;  ;; Comment/uncomment this line to see interactions between lsp client/server.
;;  ;;(setq lsp-log-io t)
;; )
;; (use-package lsp-mode
;;   :ensure t
;;     ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   :init (setq lsp-keymap-prefix "M-l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          ;; (XXX-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; (use-package company-lsp
;;   :ensure t
;; ;;  :after lsp-mode
;;   :config (progn
;;             (push 'company-lsp company-backends)))

;; (use-package haskell-mode
;;   :ensure t
;; ;;  :after company-lsp
;;   :config (progn
;;             (flycheck-mode 1)
;; ;;            (interactive-haskell-mode)
;;             ))

;; (use-package lsp-haskell
;;   :ensure t
;; ;;  :after haskell-mode
;;   :config (progn
;;             (require 'haskell-mode)
;;             (require 'lsp-mode)
;;             (setq lsp-haskell-process-path-hie "ghcide")
;;             (setq lsp-haskell-process-args-hie '("--lsp"))
;;             ;;(add-hook 'haskell-mode-hook #'lsp)
;;             ))
;; (defun my-merlin-locate ()
;;   "Locate the identifier under point and keep track of point  for backwards navigation"
;;   (interactive)
;;   (push-mark)
;;   (merlin--locate-result (merlin/locate)))

;; Add opam emacs directory to the load-path

;; Add Opam site-lisp directory to load-path to use Emacs Lisp
;; programs installed using Opam (the OCaml package manager) if Opam
;; is installed.


;; (use-package tuareg
;;   :ensure t
;;   :mode ("\\.ml[ily]?$" . tuareg-mode)
;;   :config (progn
;;             (let* (
;;                       (merlin-dir (directory-file-name (file-name-directory (shell-command-to-string "which ocamlmerlin"))))
;;                       (share-dir (string-join (list merlin-dir ".." "share") "/"))
;;                       )
;;               (when (not (string-equal share-dir ""))
;;                 (let ((opam-share share-dir))
;;                   (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;;                   (require 'merlin)
;;                   (require 'utop)
;;                   (add-hook 'tuareg-mode-hook 'merlin-mode t)
;;                   (add-hook 'caml-mode-hook 'merlin-mode t)
;;                   (setq merlin-use-auto-complete-mode 'easy)
;;                   (setq merlin-command 'opam)
;;                   (setq tuareg-lazy-= t)
;;                   (setq tuareg-in-indent 0))))))

;; Start merlin on ocaml files
;;(add-hook 'tuareg-mode-hook 'merlin-mode t)
;;(add-hook 'caml-mode-hook 'merlin-mode t)
;; Enable auto-complete
;;(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
;;(setq merlin-command 'opam)

;; Indent `=' like a standard keyword.
;;(setq tuareg-lazy-= t)
;; Indent [({ like standard keywords.
;;(setq tuareg-lazy-paren t)
;; No indentation after `in' keywords.
;;(setq tuareg-in-indent 0)


;; (add-hook 'tuareg-mode-hook
;;           ;; Turn on auto-fill minor mode.
;;           (lambda () (auto-fill-mode 1)))
