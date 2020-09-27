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

(let ((installation-results (ensure-package-installed 'use-package
                                                      'abyss-theme)))
  (when (delq 'nil installation-results)
    ;; activate newly installed packages
    (package-initialize)))

;; Load configs
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(load "base-edit.el")
(require 'lang:elisp "lang/elisp")
(require 'lang:purescript "lang/purescript")
(require 'lang:ocaml "lang/ocaml")

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
    ("3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "dd2346baba899fa7eee2bba4936cfcdf30ca55cdc2df0a1a4c9808320c4d4b22" default)))
 '(inhibit-startup-screen t)
 '(merlin-completion-with-doc t)
 '(package-selected-packages
   (quote
    (magit s dash-functional flycheck-rust cargo toml-mode lsp-ui javascript-eslint web-mode wand lsp-haskell lsp-mode company-lsp rust-mode highlight-indentation highlight-indent-guides-mode markdown-mode nix-mode counsel swiper ivy use-package csv-mode overcast-theme flycheck evil-lisp-state evil-collection evil-magit projectile evil abyss-theme xkcd utop undo-tree typed-clojure-mode systemd sos react-snippets rainbow-mode opam magit-gitflow lusty-explorer jsx-mode haskell-snippets hackernews gist flx-ido company-ghc company-cabal clojure-snippets clj-refactor)))
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

;; Magit
;;(require 'evil-magit)

;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;(define-key global-map (kbd "<f12>") 'magit-status)

;; No tabs
(setq-default indent-tabs-mode nil)

(defun exitmessage ()
  "Exit message"
  (interactive)
  (message "You probably don't wanna quit. Please use the menubar or :q for quitting!"))

;; Disable C-c C-x
(global-set-key (kbd "C-x C-c") 'exitmessage)


;; Theme
;;(lush-theme)
(abyss-theme)

(provide 'init)
;;; Init.el ends here
