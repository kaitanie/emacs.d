;;; package --- Emacs configuration file.
;;; Commentary:
;;; Emacs config file.

(require 'cl)

;; ELPA/MELPA
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")

 ;;            '("melpa-stable" . "https://stable.melpa.org/packages/") t
)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

;; activate installed packages
(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(let ((installation-results (ensure-package-installed 'magit
                                                      'web-mode
                                                      ;; 'magit-gitflow
                                                      'ag
;;                                                      'rust-mode
                                                      'evil
                                                      'evil-magit
                                                      'evil-collection
;;                                                      'psc-ide
                                                      'csv-mode
						      'dash-functional
                                                      'evil-lisp-state
                                                      'flycheck
                                                      'utop
                                                      'undo-tree
                                                      'cider
                                                      'clojure-mode
                                                      'clj-refactor
                                                      'projectile
;;                                                      'haskell-mode
                                                      'opam
                                                      'systemd
                                                      'haskell-snippets
						      'flx
                                                      'flycheck-clj-kondo
						      'flx-ido
;;                                                      'jsx-mode
                                                      'react-snippets
                                                      'use-package
                                                      'ghc
                                                      'org
                                                      'clojure-snippets
                                                      'abyss-theme
                                                      'xkcd
                                                      'paredit
                                                      'company
                                                      'hackernews
                                                      'gist
                                                      'sos
                                                      'company-cabal
                                                      'company-ghc
                                                      'rainbow-mode)))
  (when (delq 'nil installation-results)
    ;; activate newly installed packages
    (package-initialize)))

(add-to-list 'load-path "~/.emacs.d/vendor/new-purescript-mode/")
(require 'purescript-mode)
(add-to-list 'Info-default-directory-list "~/.emacs.d/vendor/new-purescript-mode/")
(add-to-list 'load-path "~/.emacs.d/vendor/psc-ide-emacs/")
(require 'psc-ide)

;; (use-package purescript-mode-autoloads
;;   :mode "\\.purs\\'"
;;   )

;; (use-package psc-ide-mode
;;   :after purescript-mode-autoloads
;;   :mode "\\.purs\\'"
;;   :config (progn (flycheck-mode)
;;                  (turn-on-purescript-indentation)))

(add-hook 'purescript-mode-hook (lambda ()
                                  (psc-ide-mode)
                                  (company-mode)
                                  (flycheck-mode)
                                  (global-set-key (kbd "<f11>") 'psc-ide-goto-definition)
                                  (global-set-key (kbd "<f10>") 'psc-ide-load-module)
                                  ;;(turn-on-purescript-indentation)
                                  ))

(global-set-key (kbd "C-M-i") 'company-complete)

;; (global-set-key (kbd "M-j") 'windmove-down)
;; (global-set-key (kbd "M-k") 'windmove-up)
;; (global-set-key (kbd "M-h") 'windmove-left)
;; (global-set-key (kbd "M-l") 'windmove-right)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(company-idle-delay 2.0)
 '(custom-enabled-themes (quote (abyss)))
 '(custom-safe-themes
   (quote
    ("d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "dd2346baba899fa7eee2bba4936cfcdf30ca55cdc2df0a1a4c9808320c4d4b22" default)))
 '(inhibit-startup-screen t)
 '(lsp-haskell-process-path-hie "ghcide")
 '(package-selected-packages
   (quote
    (flycheck-rust cargo toml-mode lsp-ui javascript-eslint web-mode wand lsp-haskell lsp-mode company-lsp rust-mode highlight-indentation highlight-indent-guides-mode markdown-mode nix-mode counsel swiper ivy use-package csv-mode overcast-theme flycheck evil-lisp-state evil-collection evil-magit projectile evil abyss-theme xkcd utop undo-tree typed-clojure-mode systemd sos react-snippets rainbow-mode opam magit-gitflow lusty-explorer jsx-mode haskell-snippets hackernews gist flx-ido company-ghc company-cabal clojure-snippets clj-refactor)))
 '(safe-local-variable-values
   (quote
    ((psc-ide-source-globs "src/**/*.purs" "test/**/*.purs" "examples/**/*.purs")
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ido
(require 'ido)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'yasnippet)
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
    (let ((original-point (point)))
      (while (and
              (not (= (point) (point-min) ))
              (not
               (string-match "[[:space:]\n]" (char-to-string (char-before)))))
        (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))

(define-key yas-minor-mode-map (kbd "<C-tab>")     'yas-ido-expand)

;; Magit
;;(setq magit-auto-revert-mode nil)
;;(setq magit-last-seen-setup-instructions "1.4.0")
;;(require 'magit-gitflow)
(require 'evil-magit)
;;(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(define-key global-map (kbd "<f12>") 'magit-status)

;; No tabs
(setq-default indent-tabs-mode nil)

;; UI
;;(menu-bar-mode -1)
(tool-bar-mode -1)

(require 'evil)
(evil-mode 1)

(evil-define-key 'normal global-map
  (kbd "C-M-j") 'windmove-down
  (kbd "C-M-k") 'windmove-up
  (kbd "C-M-h") 'windmove-left
  (kbd "C-M-l") 'windmove-right)

;;(global-hl-line-mode 1)

;;(projectile-mode +1)
;;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode)

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
(require 'company)
(require 'company-ghc)
(add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))

;; ---------------------------------------------------------------------
;; Company

(setq company-idle-delay 0.25)
(setq company-auto-complete t)

(add-hook 'after-init-hook 'global-company-mode)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

(add-to-list 'company-backends 'company-restclient)

;; Clojure
(defun turn-on-paredit () (paredit-mode 1))

(require 'rainbow-mode)
(require 'paredit)
(require 'cider)
;;(require 'clj-refactor)

(defun my-clojure-mode-hook ()
    (require 'flycheck-clj-kondo)
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
;;    (rainbow-turn-on 1)
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
;;(add-hook 'clojure-mode-hook #'rainbow-mode)
(add-hook 'clojure-mode-hook #'turn-on-paredit)
(add-hook 'ielm-mode-hook #'turn-on-paredit)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(define-key company-active-map (kbd "<return>") nil)
;;(add-hook 'cider-repl-mode-hook #'rainbow-mode)
;;(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook #'turn-on-paredit)
(setq cider-test-show-report-on-success t)
(setq cider-interactive-eval-result-prefix ";; => ")
(add-hook 'cider-mode-hook #'eldoc-mode)
;; Use clojure-mode for the Boot build files
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-(") 'paredit-forward-barf-sexp)
(define-key clojure-mode-map (kbd "C-M-z") 'cider-eval-buffer)

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)

;; JSX mode
;;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
;;(autoload 'jsx-mode "jsx-mode" "JSX mode" t)
;;(add-hook 'jsx-mode-hook
;;        (lambda () (setq forward-sexp-function nil)))
;; (add-hook 'javascript-mode-hook (lambda () (setq forward-sexp-function nil)))

;; Org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Keybindings
;; Place your bindings here.

;; For example:
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(defun open-line-above ()
  "Open a line above the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
;;  (indent-according-to-mode)
  )

(define-key global-map (kbd "C-o") 'open-line-above)

(require 'use-package)

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

(require 'flycheck)
(require 'web-mode)

;; Javascript/JSX
(use-package web-mode
  :ensure t
  :mode "\\.jsx\\'"
  :init (progn
         (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
          ;; disable jshint since we prefer eslint checking
          (setq-default flycheck-disabled-checkers
                        (append flycheck-disabled-checkers
                                '(javascript-jshint)))
          ;; use eslint with web-mode for jsx files
          (flycheck-add-mode 'javascript-eslint 'web-mode)

          (global-flycheck-mode 't)
          ))

;; Haskell

(use-package lsp-mode
  :ensure t
    ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (setq lsp-keymap-prefix "M-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package company-lsp
  :ensure t
;;  :after lsp-mode
  :config (progn
            (push 'company-lsp company-backends)))

(use-package haskell-mode
  :ensure t
;;  :after company-lsp
  :config (progn
            (flycheck-mode 1)
            (interactive-haskell-mode)
            ))

(use-package lsp-haskell
  :ensure t
;;  :after haskell-mode
  :config (progn
            (require 'haskell-mode)
            (require 'lsp-mode)
            (setq lsp-haskell-process-path-hie "ghcide")
            (setq lsp-haskell-process-args-hie '())
            ;;(add-hook 'haskell-mode-hook #'lsp)
            ))


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

(use-package swiper
  :ensure t
  :after counsel
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
  (define-key evil-motion-state-map (kbd "/") 'swiper)
  (define-key evil-motion-state-map (kbd "?") 'swiper)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(add-to-list 'load-path "~/.emacs.d/vendor/dhall-mode/")
(use-package dhall-mode
  :mode "\\.dhall\\'")

(use-package highlight-indentation
  :ensure t
  :config (progn
            (set-face-background 'highlight-indentation-face "#696969")
            (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
            (add-hook 'prog-mode-hook 'highlight-indentation-mode)))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")
;; OCaml

;; New OCaml setup
(setq custom/merlin-site-elisp (getenv "MERLIN_SITE_LISP"))
(setq custom/utop-site-elisp (getenv "UTOP_SITE_LISP"))
(setq custom/ocp-site-elisp (getenv "OCP_INDENT_SITE_LISP"))

(defun in-nix-shell-p ()
  (string-equal (getenv "IN_NIX_SHELL") "1"))

(use-package merlin
  :if (and custom/merlin-site-elisp
           (in-nix-shell-p))
  :load-path custom/merlin-site-elisp
  :hook
  (tuareg-mode . merlin-mode)
  (merlin-mode . company-mode)
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
  :load-path custom/ocp-site-elisp)

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


;; Rust
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config (setq company-tooltip-align-annotations t)
          (setq company-minimum-prefix-length 1))

(use-package lsp-mode
  :ensure t
    ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (setq lsp-keymap-prefix "M-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
;;  :config (require 'lsp-clients)
  )

(use-package lsp-ui
  :ensure t)

;; Allow Company to get info from LSP (use-package company-lsp)

;; The actual Rust-specific stuff:

(defun my/normalize-rust-windows! ()
  "Foo."
  (interactive)
  (let ((original-buffer (current-buffer)))
    (get-buffer-create "*lsp-help*")
    (flycheck-list-errors)
    (switch-to-buffer original-buffer)
    (delete-other-windows)
    (let* ((side-buffer-width (* (window-width) 0.33))
           (split-point (ceiling (- (window-width) side-buffer-width))))
      (split-window-vertically (- (window-height) 10))
      (split-window-horizontally split-point))
    (windmove-down)
    (switch-to-buffer "*Flycheck errors*")
    (windmove-up)
    (windmove-right)
    (switch-to-buffer "*lsp-help*")
    (windmove-left)))

(use-package toml-mode
  :ensure t
  :config (progn
            (bind-key "<f9>" 'my/normalize-rust-windows!)))

(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Theme
;;(lush-theme)
(abyss-theme)

(provide 'init)
;;; Init.el ends here
