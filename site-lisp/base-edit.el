;; package --- Summary:
;;; Commentary:
;;; Code:
;;  -*- lexical-binding: t; -*-

(use-package cl)

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to `C-c o'

;; Eglot LSP integration
(use-package eglot
  :ensure t
  :config
  (progn
   ;; (add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp")))
    (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server" "--lsp")))
    (add-to-list 'eglot-server-programs '(rust-mode . ("rls")))
    (add-to-list 'eglot-server-programs '(rustic . ("rls")))))

;; LSP mode
;; (use-package lsp-mode
;;   :ensure t
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (rust-mode . lsp)
;;          (haskell-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; (use-package lsp-ivy
;;   :ensure t
;;   :commands lsp-ivy-workspace-symbol)

;; (use-package lsp-haskell
;;   :ensure t)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(defun open-line-above ()
  "Open a line above the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (forward-line -1)
;;  (indent-according-to-mode)
  )

(define-key global-map (kbd "C-o") 'open-line-above)

;; ivy
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (bind-key "C-c C-r" 'ivy-resume))

;; projectile
(use-package projectile
  :ensure t
  :after ivy
  :config (progn
            (projectile-global-mode)
            (global-set-key (kbd "C-c p f") 'projectile-find-file)
            (setq projectile-mode-line
                  '(:eval (format " [%s]" (projectile-project-name))))
            (setq projectile-remember-window-configs t)
            (setq projectile-completion-system 'ivy)))

(use-package counsel
  :ensure t
  :after projectile
  :config (counsel-mode 1))

;;(use-package yasnippet
;;  :ensure t
;;  :hook (prog-mode . yas-minor-mode)
;;  :after cl
;;  :config
;;  (progn
;;    (require 'cl)
;;    (require 'yasnippet)
;;    (defun yas-ido-expand ()
;;      "Lets you select (and expand) a yasnippet key"
;;      (interactive)
;;      (let ((original-point (point)))
;;        (while (and
;;                (not (= (point) (point-min) ))
;;                (not
;;                 (string-match "[[:space:]\n]" (char-to-string (char-before)))))
;;          (backward-word 1))
;;        (let* ((init-word (point))
;;               (word (buffer-substring init-word original-point))
;;               (list (yas-active-keys)))
;;          (goto-char original-point)
;;          (let ((key (remove-if-not
;;                      (lambda (s) (string-match (concat "^" word) s)) list)))
;;            (if (= (length key) 1)
;;                (setq key (pop key))
;;              (setq key (ido-completing-read "key: " list nil nil word)))
;;            (delete-char (- init-word original-point))
;;            (insert key)
;;            (yas-expand)))))
;;    )
;;  :bind
;;  ("<C-tab>" . yas-ido-expand)
;;  )

(use-package evil
  :ensure t
  :config
  (tool-bar-mode -1)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (evil-mode 1)
  (evil-define-key 'normal global-map
    (kbd "q") 'quit-window
    (kbd "C-M-j") 'windmove-down
    (kbd "C-M-k") 'windmove-up
    (kbd "C-M-h") 'windmove-left
    (kbd "C-M-l") 'windmove-right))

(use-package swiper
  :ensure t
  :after counsel evil
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
  (define-key evil-motion-state-map (kbd "/") 'swiper)
  (define-key evil-motion-state-map (kbd "?") 'swiper)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(use-package flycheck
  :ensure t
  :hook
  (after-init . global-flycheck-mode)
  (prog-mode . flycheck-mode))

;;(add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))

;; ---------------------------------------------------------------------
;; Company

;;(add-hook 'after-init-hook 'global-company-mode)
;;(define-key company-active-map (kbd "C-n") 'company-select-next)
;;(define-key company-active-map (kbd "C-p") 'company-select-previous)
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 5)
  (setq company-auto-commit t)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (global-set-key (kbd "<C-tab>") 'company-complete))

;; (use-package ido
;;   :ensure t
;;   :config
;;   (ido-mode 1)
;;   (ido-everywhere 1)
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-use-faces nil))

;; (use-package flx-ido
;;   :ensure t
;;   :config
;;   (flx-ido-mode 1))

;; Org-mode
(use-package org
  :ensure t
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t))


(use-package highlight-indentation
  :ensure t
  :config (progn
            (set-face-background 'highlight-indentation-face "#696969")
            (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
            (add-hook 'prog-mode-hook 'highlight-indentation-mode)))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package magit
  :ensure t
  :bind
  ("<f12>" . magit-status))

;; (use-package evil-magit
;;   :ensure t
;;   :after magit)

(use-package ag
  :ensure t)

(use-package csv-mode
  :ensure t)

;; (use-package evil-collection :ensure t ;;  :config ;;  (evil-collection-init))

;; Mu4e email
(use-package mu4e
  :config
  (require 'mu4e)
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Gmail"
             :match-func (lambda (msg) (when msg
                                         (string-prefix-p "/PersonalGmail" (mu4e-message-field msg :maildir))))
             :vars '(
                     (mu4e-trash-folder . "/PersonalGmail/[Gmail].Trash")
                     (mu4e-refile-folder . "/PersonalGmail/[Gmail].Archive")
                     ))
           )
        ))

(provide 'base-edit)
;;; base-edit.el ends here
