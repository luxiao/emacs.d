(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun sanityinc/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (sanityinc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (sanityinc/utf8-locale-p (getenv "LC_ALL"))
      (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
      (sanityinc/utf8-locale-p (getenv "LANG"))))

(when (or window-system (sanityinc/locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))

(add-to-list 'default-frame-alist '(font . "Source Code Pro-13"))
(elpy-enable)
(elpy-use-ipython)

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(setq sql-mysql-login-params
      '((user :default "risk_user_dev")
        (database :default "risk_list_00")
        (server :default "rdsuueafyuqbzfu.mysql.rds.aliyuncs.com")
        (port :default 3306)
        (password :default "risk_user_dev_a7fbd6")))

(setq sql-connection-alist
      '((rds.dev (sql-product 'mysql)
                 (sql-port 3306)
                 (sql-server "rdsuueafyuqbzfu.mysql.rds.aliyuncs.com")
                 (sql-user "risk_user_dev")
                 (sql-database "risk_list_00")
                 (sql-password "risk_user_dev_a7fbd6"))
        (local.mysql (sql-product 'mysql)
                     (sql-port 3306)
                     (sql-server "127.0.0.1")
                     (sql-user "root")
                     (sql-password "")
                     (sql-database "blacklist"))))

(defun za-rds-dev ()
  (interactive)
  (my-sql-connect 'mysql 'rds.dev))

(defun local-mysql ()
  (interactive)
  (my-sql-connect 'mysql 'local.mysql))

(defun my-sql-connect (product connection)
  (setq sql-product product)
  (sql-connect connection))
(global-set-key "\C-cs" 'shell)

                                        ;(require 'init-auto-complete)
(require 'solidity-mode)
(require 'restclient)
(add-to-list 'auto-mode-alist '("\\.restc\\'" . restclient-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(provide 'init-locales)
