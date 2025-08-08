;;; init-locales.el --- Configure default locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/locale-var-encoding (v)
  "Return the encoding portion of the locale string V, or nil if missing."
  (when v
    (save-match-data
      (let ((case-fold-search t))
        (when (string-match "\\.\\([^.]*\\)\\'" v)
          (intern (downcase (match-string 1 v))))))))

(dolist (varname '("LC_ALL" "LANG" "LC_CTYPE"))
  (let ((encoding (sanityinc/locale-var-encoding (getenv varname))))
    (unless (memq encoding '(nil utf8 utf-8))
      (message "Warning: non-UTF8 encoding in environment variable %s may cause interop problems with this Emacs configuration." varname))))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
                                        ; list the packages you want
(setq package-list '(dracula-theme web-mode undo-tree go-mode lua-mode yasnippet-snippets restclient emms quelpa-use-package graphviz-dot-mode gt ))
(dolist (pkg package-list)
  (unless (package-installed-p pkg)
    (package-install pkg))
  )
(require 'use-package)
(use-package undo-tree  :ensure t)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history nil)
(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match-p "UTF-8" v)))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(setq sql-connection-alist
      '((local.mysql (sql-product 'mysql)
                     (sql-port 3306)
                     (sql-server "127.0.0.1")
                     (sql-user "root")
                     (sql-password "")
                     (sql-database "blacklist"))))

(defun local-mysql ()
  (interactive)
  (my-sql-connect 'mysql 'local.mysql))

(defun my-sql-connect (product connection)
  (setq sql-product product)
  (sql-connect connection))

(set-frame-font "Cascadia Code 17" nil t)
;;(set-frame-font "Fira Code 17" nil t)
                                        ;(require 'init-auto-complete)
(require 'restclient)
(add-to-list 'auto-mode-alist '("\\.restc\\'" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
;;(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 2 indent-tabs-mode 1))
(add-hook 'go-mode-hook 'my-go-mode-hook)
(defvar xah-run-current-file-before-hook nil "Hook for `xah-run-current-file'. Before the file is run.")

(defvar xah-run-current-file-after-hook nil "Hook for `xah-run-current-file'. After the file is run.")
(defun xah-run-current-go-file ()
  "Run or build current golang file.

To build, call `universal-argument' first.

Version 2018-10-12"
  (interactive)
  (when (not (buffer-file-name)) (save-buffer))
  (when (buffer-modified-p) (save-buffer))
  (let* (
         ($outputb "*xah-run output*")
         (resize-mini-windows nil)
         ($fname (buffer-file-name))
         ($fSuffix (file-name-extension $fname))
         ($prog-name "go")
         $cmd-str)
    (setq $cmd-str (concat $prog-name " \""   $fname "\" &"))
    (if current-prefix-arg
        (progn
          (setq $cmd-str (format "%s build \"%s\" " $prog-name $fname)))
      (progn
        (setq $cmd-str (format "%s run \"%s\" &" $prog-name $fname))))
    (progn
      (message "running %s" $fname)
      (message "%s" $cmd-str)
      (shell-command $cmd-str $outputb )
      ;;
      )))

(defun xah-run-current-file ()
  "Execute the current file.
For example, if the`' current buffer is x.py, then it'll call 「python x.py」 in a shell.
Output is printed to buffer “*xah-run output*”.

The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, TypeScript, golang, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
Version 2018-10-12"
  (interactive)
  (let (
        ($outputb "*xah-run output*")
        (resize-mini-windows nil)
        ($suffix-map
         ;; (‹extension› . ‹shell program name›)
         `(
           ("php" . "php")
           ("pl" . "perl")
           ("py" . "python")
           ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
           ("rb" . "ruby")
           ("go" . "GOPROXY=https://goproxy.io go run")
           ("hs" . "runhaskell")
           ("js" . "node")
           ("mjs" . "node --experimental-modules ")
           ("ts" . "tsc") ; TypeScript
           ("tsx" . "tsc")
           ("sh" . "bash")
           ("clj" . "java -cp ~/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
           ("rkt" . "racket")
           ("ml" . "ocaml")
           ("vbs" . "cscript")
           ("tex" . "pdflatex")
           ("latex" . "pdflatex")
           ("java" . "javac")
           ("lua" . "resty ")
           ("yml" . "ansible-playbook -i ~/.ansible/hosts -u admin --become -K --become-user root")
           ("proto" . "protoc -I=/Users/lux/za/projects/dlp/backend/proto2/src --python_out=/Users/lux/za/projects/dlp/backend/proto2/py")
           ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
           ))
        $fname
        $fSuffix
        $prog-name
        $cmd-str)
    (when (not (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq $fname (buffer-file-name))
    (setq $fSuffix (file-name-extension $fname))
    (setq $prog-name (cdr (assoc $fSuffix $suffix-map)))
    (setq $cmd-str (concat $prog-name " \""   $fname "\" &"))
    (run-hooks 'xah-run-current-file-before-hook)
    (cond
     ((string-equal $fSuffix "el")
      (load $fname))
     ((or (string-equal $fSuffix "ts") (string-equal $fSuffix "tsx"))
      (if (fboundp 'xah-ts-compile-file)
          (progn
            (xah-ts-compile-file current-prefix-arg))
        (if $prog-name
            (progn
              (message "Running")
              (shell-command $cmd-str $outputb ))
          (error "No recognized program file suffix for this file."))))
     ((string-equal $fSuffix "go")
      (xah-run-current-go-file))
     ((string-equal $fSuffix "java")
      (progn
        (shell-command (format "java %s" (file-name-sans-extension (file-name-nondirectory $fname))) $outputb )))
     (t (if $prog-name
            (progn
              (message "Running")
              (shell-command $cmd-str $outputb ))
          (error "No recognized program file suffix for this file."))))
    (run-hooks 'xah-run-current-file-after-hook)))
                                        ;
(global-set-key "\C-xx" 'xah-run-current-file)
(global-set-key (kbd "M-.") #'xref-find-definitions)
(global-set-key (kbd "M-,") #'xref-pop-marker-stack)
(global-set-key "\C-xC-6d" 'base64-decode-region)
(global-set-key "\C-xC-6e" 'base64-encode-region)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)
;;(global-linum-mode 0)

(electric-pair-mode 1)
(setq cua-enable-cua-keys nil)
;;; translate
(use-package gt :ensure t)
(setq gt-langs '(en zh))
(setq gt-default-translator
      (gt-translator
       :taker   (gt-taker :text 'buffer :pick 'paragraph)
       :engines (list (gt-google-engine
                       ))
       :render  (gt-buffer-render)))
(setq gt-http-proxy "socks5://127.0.0.1:1080")

;;; proxy
(require 'socks)
(setq socks-noproxy (split-string (or (getenv "no_proxy") "localhost,127.0.0.1") ","))
(setq url-gateway-method 'socks)
(setq socks-server '("Default server" "127.0.0.1" 1080 5))

;; chatgpt
;;(use-package chatgpt  :bind ("C-c q" . chatgpt-query))

;; forge & ghub
;;(with-eval-after-load 'magit
;;  (require 'forge))


(princ (concat (format "Emacs version: %s\n" (emacs-version))
               (format "org version: %s\n" (org-version))))
;; epub
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; emms
(require 'emms-setup)
(emms-all)
(setq emms-player-list '(emms-player-mpv))
(setq-default
 emms-volume-change-function 'emms-volume-mpv-change
 emms-volume-mpv-method 'smart
 emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-audio-display" "--force-window=no" "--vo=null"))
(setq my-mpv-ipc-socket (expand-file-name "emms/mpv-ipc.sock" user-emacs-directory))
(defun my-mpv-ipc-send (cmd)
  "Send JSON command to mpv via its IPC socket."
  (let ((proc (make-network-process
               :name "mpv-ipc"
               :buffer nil
               :family 'local
               :service my-mpv-ipc-socket)))
    (process-send-string proc (concat cmd "\n"))
    (delete-process proc)))
(defun my-emms-volume-up ()
  (interactive)
  (my-mpv-ipc-send "{ \"command\": [\"add\", \"volume\", 5] }"))
(defun my-emms-volume-down ()
  (interactive)
  (my-mpv-ipc-send "{ \"command\": [\"add\", \"volume\", -5] }"))
(require 'transient)
(transient-define-prefix emms-menu ()
  "EMMS commands menu"
  ["播放"
   ("f" "show" emms-show)
   ("P" "Play" emms-start)
   ("p" "Pause" emms-pause)
   ("s" "Stop" emms-stop)
   ("n" "Next Track" emms-next)
   ("b" "Previous Track" emms-previous)
   ("+" "Volume Up" my-emms-volume-up :transient t)
   ("-" "Volume Down" my-emms-volume-down :transient t)
   ("q" "Quit" transient-quit-one)
   ]
  ["歌单"
   ("z" "Save Playlist" emms-playlist-save)
   ("o" "Play Playlist" emms-play-playlist)
   ("a" "Play Directory" emms-play-directory)
   ]
  )
(setq emms-source-file-default-directory "~/Music/")
(global-set-key (kbd "C-c e") 'emms-menu)
(require 'emms-mode-line)
(emms-mode-line 1)
;; yas
(add-to-list 'load-path (locate-user-emacs-file "snippets/"))
(require 'yasnippet)
(yas-global-mode 1)
(global-set-key (kbd "C-c y") 'yas-insert-snippet)
;; info
(add-to-list 'Info-directory-list "~/.emacs.d/info/")
;; Load LilyPond
(setq load-path (append '("/opt/homebrew/share/emacs/site-lisp/lilypond") load-path))
(autoload 'LilyPond-mode "lilypond-mode")
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))

(defun url-encode-region (start end)
  "URL encode the text in the region from START to END."
  (interactive "r")
  (if (use-region-p)
      (let ((text (buffer-substring start end)))
        (delete-region start end)
        (insert (url-hexify-string text)))
    (message "No region selected")))

(defun url-decode-region (start end)
  "URL decode the text in the region from START to END."
  (interactive "r")
  (if (use-region-p)
      (let ((text (buffer-substring start end)))
        (delete-region start end)
        (insert (url-unhex-string text)))
    (message "No region selected")))
;; tree-sitter
(treesit-available-p)
;;(treesit-ready-p 'python)
;;(treesit-language-available-p 'python)
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :config
  (setq treesit-extra-load-path (list (locate-user-emacs-file "tree-sitter/"))))
;; 自动安装全部语言
;; (dolist (lang treesit-language-source-alist)
;;   (unless (treesit-language-available-p (car lang))
;;     (treesit-install-language-grammar (car lang))))

;; eglot
(use-package eglot
  :ensure t
  :defer t
  :custom
  ;; Optimize performance
  (eglot-send-changes-idle-time 0.5)
  (eglot-extend-to-xref t)
  ;; Configure language servers
  (eglot-server-programs
   '((python-ts-mode . ("pyright-langserver" "--stdio"
                        :initializationOptions (:pyright (:plugins (
                                                                    :ruff (:enabled t
                                                                                    :lineLength 88
                                                                                    :exclude ["E501"] ; Disable line length warnings
                                                                                    :select ["E", "F", "I", "UP"]) ; Enable specific rule families
                                                                    :pycodestyle (:enabled nil) ; Disable other linters since we're using ruff
                                                                    :pyflakes (:enabled nil)
                                                                    :pylint (:enabled nil)
                                                                    :rope_completion (:enabled t)
                                                                    :autopep8 (:enabled nil))))))
     ((js-ts-mode typescript-ts-mode tsx-ts-mode) .
      ("typescript-language-server" "--stdio"))))
  :config
  (setq-default
   eglot-workspace-configuration
   '(:pyright (
               :typeCheckingMode "off"
               )
              :pyright.analysis (
                                 :diagnosticSeverityOverrides (
                                                               :reportUnusedCallResult "none"
                                                               )
                                 :inlayHints (
                                              :callArgumentNames :json-false
                                              )
                                 )))
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l h" . eglot-inlay-hints-mode)
              ("C-c l q" . eglot-shutdown-all))
  :hook ((python-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         ;; Python-specific settings
         (python-ts-mode . (lambda ()
                             (setq-local indent-tabs-mode nil
                                         tab-width 4
                                         python-indent-offset 4)
                             (superword-mode 1)
                             (hs-minor-mode 1)
                             (set-fill-column 88)
                             (display-line-numbers-mode 1)))))
(dolist (hook '(tsx-ts-mode-hook
                typescript-ts-mode-hook
                js-ts-mode-hook))
  (Add-hook hook
            (lambda ()
              (setq tab-width 4
                    indent-tabs-mode nil))))
(require 'alert)

(alert "This is an alert" :style 'osx-notifier :severity 'high :title "hello alert")
;;claude code
(use-package eat :ensure t)
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map))
(defun my-claude-notify (title message)
  "Display a macOS notification with sound."
  (call-process "osascript" nil nil nil
                "-e" (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                             message title)))

(provide 'init-locales)
;;; init-locales.el ends here
