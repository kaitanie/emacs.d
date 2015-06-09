;; ELPA/MELPA
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; activate installed packages
(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
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

(ensure-package-installed 'magit
                          'merlin
                          'utop
                          'undo-tree
                          'cider
                          'haskell-mode
                          'scala-mode2
                          'lush-theme
                          'opam
                          'systemd
                          'haskell-snippets
                          'ghc
                          'org
                          'clojure-snippets
                          'lusty-explorer
                          'paredit
                          'company
                          'company-cabal
                          'company-ghc
                          'company-ghci
                          'rainbow-mode)

;; activate installed packages
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Magit
;;(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

;; No tabs
(setq-default indent-tabs-mode nil)

;; UI
;;(menu-bar-mode -1)
(tool-bar-mode -1)

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Haskell
(require 'haskell-mode)
(require 'haskell-session)
(require 'haskell-indentation)

(defun my-haskell-mode-hook ()
   (haskell-indentation-mode -1) ;; turn off, just to be sure
   (haskell-indent-mode 1)       ;; turn on indent-mode

 ;;  (turn-on-haskell-simple-indent)
   (haskell-indentation)

   ;; further customisations go here.  For example:
   (setq locale-coding-system 'utf-8 )
   (flyspell-prog-mode)  ;; spell-checking in comments and strings
   ;; etc.

   )

;;(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
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

;; Clojure
(defun turn-on-paredit () (paredit-mode 1))

(require 'rainbow-mode)
(require 'paredit)
(require 'cider)
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-mode)
(add-hook 'cider-repl-mode-hook #'turn-on-paredit)
(setq cider-test-show-report-on-success t)
(setq cider-interactive-eval-result-prefix ";; => ")
(add-hook 'cider-mode-hook #'eldoc-mode)
;; Use clojure-mode for the Boot build files
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-(") 'paredit-forward-barf-sexp)

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)

;; OCaml
;; Add opam emacs directory to the load-path

(setq opam-config (shell-command-to-string "opam config var share 2> /dev/null"))
 (when (not (string-equal opam-config ""))
   (setq opam-share (substring opam-config 0 -1))
   (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
   ;; Load merlin-mode
   (require 'merlin)
   (require 'utop))

;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

;; Indent `=' like a standard keyword.
(setq tuareg-lazy-= t)
;; Indent [({ like standard keywords.
(setq tuareg-lazy-paren t)
;; No indentation after `in' keywords.
(setq tuareg-in-indent 0)


;; (add-hook 'tuareg-mode-hook
;;           ;; Turn on auto-fill minor mode.
;;           (lambda () (auto-fill-mode 1)))

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

(define-key global-map (kbd "C-`") 'ac-complete-ensime-completions)

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

;; Theme
(lush-theme)
