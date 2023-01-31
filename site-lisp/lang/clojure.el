;; Clojure mode --- under construction, not set up yet!!!
;; Clojure
;;(defun turn-on-paredit () (paredit-mode 1))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package cider
  :ensure t
  :hook ((cider-repl-mode . company-mode)))

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj[cs]?$" . clojure-mode)
  :bind
  (("C-M-z" . cider-load-buffer))
  :config
  (require 'flycheck-clj-kondo))

(use-package paredit
  :ensure t
  :hook ((clojure-mode cider-repl-mode) . paredit-mode))

;; (require 'rainbow-mode)
;; (require 'paredit)
;; (require 'cider)
;; ;;(require 'clj-refactor)

;; (defun my-clojure-mode-hook ()
;;     (require 'flycheck-clj-kondo)
;;     (clj-refactor-mode 1)
;;     (yas-minor-mode 1) ; for adding require/use/import
;; ;;    (rainbow-turn-on 1)
;;     (cljr-add-keybindings-with-prefix "C-c C-m"))

;; (eval-after-load 'clojure-mode
;;   '(font-lock-add-keywords
;;     'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
;;                      (0 (progn (compose-region (match-beginning 1)
;;                                                (match-end 1) "λ")
;;                                nil))))))

;; (eval-after-load 'clojure-mode
;;   '(font-lock-add-keywords
;;     'clojure-mode `(("\\(#\\)("
;;                      (0 (progn (compose-region (match-beginning 1)
;;                                                (match-end 1) "ƒ")
;;                                nil))))))

;; (eval-after-load 'clojure-mode
;;   '(font-lock-add-keywords
;;     'clojure-mode `(("\\(#\\){"
;;                      (0 (progn (compose-region (match-beginning 1)
;;                                                (match-end 1) "∈")
;;                                nil))))))

;; (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
;; ;;(add-hook 'clojure-mode-hook #'rainbow-mode)
;; (add-hook 'clojure-mode-hook #'turn-on-paredit)
;; (add-hook 'ielm-mode-hook #'turn-on-paredit)
;; (add-hook 'cider-repl-mode-hook #'company-mode)
;; (add-hook 'cider-mode-hook #'company-mode)
;; (define-key company-active-map (kbd "<return>") nil)
;; ;;(add-hook 'cider-repl-mode-hook #'rainbow-mode)
;; ;;(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
;; (add-hook 'cider-repl-mode-hook #'turn-on-paredit)
;; (setq cider-test-show-report-on-success t)
;; (setq cider-interactive-eval-result-prefix ";; => ")
;; (add-hook 'cider-mode-hook #'eldoc-mode)
;; ;; Use clojure-mode for the Boot build files
;; (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

;; (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
;; (define-key paredit-mode-map (kbd "M-(") 'paredit-forward-barf-sexp)
;; (define-key clojure-mode-map (kbd "C-M-z") 'cider-eval-buffer)

(provide 'lang:clojure)
