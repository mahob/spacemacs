(defun ruby/pre-init-dap-mode ()
  (when (eq ruby-backend 'lsp)
    (add-to-list 'spacemacs--dap-supported-modes 'ruby-mode))
  (add-hook 'ruby-mode-local-vars-hook #'spacemacs//ruby-setup-dap))

;; required to debug ruby
;; see https://emacs-lsp.github.io/dap-mode/page/configuration/#ruby
(require 'dap-ruby)
