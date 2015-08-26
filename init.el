(require 'cl)

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
                                                      'magit-gitflow
                                                      'utop
                                                      'undo-tree
                                                      'cider
                                                      'clojure-mode
                                                      'clj-refactor
                                                      'typed-clojure-mode
                                                      'haskell-mode
                                                      'scala-mode2
                                                      'lush-theme
                                                      'opam
                                                      'systemd
                                                      'haskell-snippets
						      'flx
						      'flx-ido
                                                      'jsx-mode
                                                      'react-snippets
                                                      'ghc
                                                      'hamlet-mode
                                                      'org
                                                      'org-trello
                                                      'clojure-snippets
                                                      'datomic-snippets
                                                      'java-snippets
                                                      'lusty-explorer
                                                      'xkcd
                                                      'paredit
                                                      'company
                                                      'hackernews
                                                      'gist
                                                      'restclient
                                                      'company-restclient
                                                      'sos
                                                      'company-cabal
                                                      'company-ghc
                                                      'company-ghci
                                                      'rainbow-mode)))
  (when (delq 'nil installation-results)
    ;; activate newly installed packages
    (package-initialize)))

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
(setq magit-last-seen-setup-instructions "1.4.0")
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(define-key global-map (kbd "<f12>") 'magit-status)

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
   (yas-minor-mode 1) ; for adding require/use/import
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

(add-to-list 'company-backends 'company-restclient)

;; Clojure
(defun turn-on-paredit () (paredit-mode 1))

(require 'rainbow-mode)
(require 'paredit)
(require 'cider)
;;(require 'clj-refactor)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-m"))

;;    (rainbow-turn-on 1)
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

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-mode)
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
;;(add-hook 'clojure-mode-hook #'rainbow-mode)
(add-hook 'clojure-mode-hook #'turn-on-paredit)
(add-hook 'ielm-mode-hook #'turn-on-paredit)
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

;; OCaml
;; Add opam emacs directory to the load-path

;; Add Opam site-lisp directory to load-path to use Emacs Lisp
;; programs installed using Opam (the OCaml package manager) if Opam
;; is installed.
(let ((opam-config (shell-command-to-string "opam config var share 2> /dev/null")))
  (when (not (string-equal opam-config ""))
    (let ((opam-share (substring opam-config 0 -1)))
      (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
      (require 'merlin)
      (require 'utop))))

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

;; JSX mode
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

;; Org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(require 'org-trello)
(custom-set-variables '(org-trello-files '("~/scratch/trello/clojure.org")))

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
