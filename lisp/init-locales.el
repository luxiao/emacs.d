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

                                        ;python dev
(elpy-enable)
(elpy-use-ipython)
(require 'init-auto-complete)

;mysql client
(setq sql-mysql-login-params
      '((user :default "your_user_dev")
        (database :default "your_database")
        (server :default "your_mysql")
        (port :default 3306)
        (password :default "your_password")))

(setq sql-connection-alist
      '((yours.dev (sql-product 'mysql)
                   (sql-port 3306)
                   (sql-server "your_mysql")
                   (sql-user "your_user_dev")
                   (sql-database "your_db")
                   (sql-password "your_password"))
        (local.mysql (sql-product 'mysql)
                     (sql-port 3306)
                     (sql-server "127.0.0.1")
                     (sql-user "root")
                     (sql-password "root")
                     (sql-database "test"))))

(defun za-rds-dev ()
  (interactive)
  (my-sql-connect 'mysql 'yours.dev))

(defun local-mysql ()
  (interactive)
  (my-sql-connect 'mysql 'local.mysql))

(defun my-sql-connect (product connection)
  (setq sql-product product)
  (sql-connect connection))
(global-set-key "\C-cs" 'shell)
(global-set-key "\C-ct" 'ansi-term)

(provide 'init-locales)
