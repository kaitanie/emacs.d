;;; package --- Summary:
;;; Commentary:
;;; Code:

(defun get-signal-nix-hash ()
  (interactive)
  (let* ((signal-version "1.36.3")
         (beginning "https://updates.signal.org/desktop/apt/pool/main/s/signal-desktop/signal-desktop_")
         (ending "_amd64.deb")
         (url (string-join (list beginning signal-version ending)))
         (cmd (string-join (list "nix-prefetch-url " url)))
         (result (shell-command-to-string cmd)))
    (message result)
    ))

(provide 'utils:sys)
;;; sys.el ends here
