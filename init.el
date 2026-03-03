
;; Para que todo vaya perfectamente hay que instalar algunos paquetes externos de manera manual:


;; ------ nerd-icons ------
;; Recordar evaluar esto para instalar los iconos si es la primera vez que se usa este archivo de configuracion.
;; Inside emacs: M-x nerd-icons-install-fonts


;; ------ Docker ------
;; Para que todo funcione correctamente en docker, es imprescindible que los permisos vayan acorde con
;; los que tenemos en local. En particular, git puede fallar haciendo que project.el no vaya correctamente
;; usando Tramp en Docker. Una solución rápida y sencilla es ejecutar el siguiente comando en la imagen
;; de Docker:
;; RUN git config --global --add safe.directory /path/to/project
;;
;; No se, pero esto no funciona. Sólo funciona si se ejecuta el comando dentro del contenedor.

;; Por otro lado, para que Eglot vaya correctamente en Docker necesitamos los servidores LSP instalados
;; en la imagen. A continuación se muestra qué hay que instalar en dicha imagen para cada lenguaje de
;; programación.


;; ------ eglot ------
;; Para usar C++ hay que instalar clangd. Pero tambien hay que asegurarse de que clang
;; usa el compilador de c++. En ubuntu 24 he tenido que instalar g++-14.
;; On terminal: sudo apt install clangd g++-14

;; Para usar cmake
;; Dependiendo de si es en local o en Docker, vale la pena usar una opción u otra:
;; En local:
;;   Más abajo se modifica el exec-path para que contenga ~/.local/bin, que es donde se almacena cmake-language-server
;;   On terminal: sudo apt install python3 python-is-python3 pipx
;;   On terminal: sudo pipx install cmake-language-server
;; En Docker:
;;   On terminal: sudo apt install python3 python-is-python3 pip
;;   On terminal: sudo pip install cmake-language-server --break-system-packages

;; Para usar python
;; Igual que con cmake, depende de si se usa en local o en Docker
;; En local:
;;   Más abajo se modifica el exec-path para que contenga ~/.local/bin, que es donde se almacena cmake-language-server
;;   On terminal: sudo apt install python3 python-is-python3 pipx
;;   On terminal: sudo pipx install python-lsp-server
;; En Docker:
;;   On terminal: sudo apt install python3 python-is-python3 pip
;;   On terminal: sudo pip install python-lsp-server --break-system-packages


;; ------ sly ------
;; Para usar sly necesitamos un compilador de Common Lisp. O dos, ¿por qué no?
;; On terminal: sudo apt install sbcl clisp


;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------


;; ------ Starting up emacs ------
(setq inhibit-startup-message t)                             ; Disable startup message
(setq initial-scratch-message nil)                           ; Disable initial scratch message
(scroll-bar-mode -1)                                         ; Disable scrollbar
(horizontal-scroll-bar-mode -1)                              ; Disable scrollbar
(tool-bar-mode -1)                                           ; Disable the toolbar
(tooltip-mode -1)                                            ; Disable tooltips
(menu-bar-mode -1)                                           ; Disable menu bar                          
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; Initialize emacs maximized
(setq frame-resize-pixelwise t)                              ; Adjust to window correctly
(delete-selection-mode 1)                                    ; Follow the convention of modern editors
(setq-default indent-tabs-mode nil)                          ; Prevents extraneous tabs
;; (set-face-attribute 'default nil :height 100)                ; Make font scale a bit larger
(blink-cursor-mode 0)                                        ; Stops blink cursor
(setq make-backup-files nil)                                 ; Disable backup files
(setq warning-minimum-level :error)                          ; Disable the pop-up of *Warnings* buffer
(put 'upcase-region 'disabled nil)                           ; Enables the command upcase-region
(put 'downcase-region 'disabled nil)                         ; Enables the command downcase-region


;; ------ global keybindings ------
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x p a") 'ff-find-other-file)
(global-unset-key (kbd "C-z"))


;; ------ Scrolling ------
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(defun custom-scroll-up ()
  "Scroll up 2 lines."
  (interactive)
  (scroll-up 2))

(defun custom-scroll-down ()
  "Scroll down 2 lines."
  (interactive)
  (scroll-down 2))

(defun fast-scroll-up ()
  "Scroll up 5 lines."
  (interactive)
  (scroll-up 5))

(defun fast-scroll-down ()
  "Scroll down 5 lines."
  (interactive)
  (scroll-down 5))

(global-set-key (kbd "<M-up>") 'custom-scroll-down)
(global-set-key (kbd "<M-down>") 'custom-scroll-up)
(global-set-key (kbd "<M-S-up>") 'fast-scroll-down)
(global-set-key (kbd "<M-S-down>") 'fast-scroll-up)


;; ------ electric-pair-mode ------
(electric-pair-mode 1)


;; ------ show-paren-mode ------
(show-paren-mode 1)


;; ------ shell ------
(setq sh-basic-offset 2)


;; ------ PATH ------
(add-to-list 'exec-path (expand-file-name "~/.local/bin")) ; Necesario para cmake-language-server en local


;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------


;; ----- Melpa -----
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)


;; ----- use-package -----
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;; ----- Theme -----
(use-package vscode-dark-plus-theme
  :config
  (load-theme 'vscode-dark-plus t))


;; ------ Doom modeline ------
(use-package doom-modeline
  :init (doom-modeline-mode 1))


;; ------ dogears ------
(use-package dogears
  ;; These bindings are optional, of course:
  :bind (:map global-map
              ("M-<left>" . dogears-back)
              ("M-<right>" . dogears-forward))
  :config
  (setq dogears-functions '(beginning-of-buffer end-of-buffer))
  (dogears-mode))


;; ------ project ------
(setq project-switch-commands 'project-find-file)


;; ------ magit ------
(use-package magit)


;; ------ dired ------
(use-package dired
  :ensure nil
  :custom
  ((dired-listing-switches "-agvho --group-directories-first")
   (delete-by-moving-to-trash t)
   (dired-dwim-target t)))

(use-package dired-hide-dotfiles
  :bind (:map dired-mode-map
              ("H" . dired-hide-dotfiles-mode)))

;; ------ yasnippet ------
;; (use-package yasnippet
;;   :config
;;   (yas-global-mode 1)
;;   (setq yas-snippet-dirs '("~/.emacs.d/snippets")))


;; ------ corfu ------
(use-package corfu
  :init
  (global-corfu-mode)
  :bind (:map corfu-map
              ("RET" . nil))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-quit-no-match t)
  (corfu-on-exact-match nil)
  :config
  (add-hook 'after-save-hook #'corfu-quit))



;; ------ vertico ------
(use-package vertico
  :config
  (vertico-mode 1)
  (setq vertico-cycle t))

(use-package vertico-directory
  :ensure nil
  :bind (:map vertico-map
              ("TAB" . vertico-directory-enter)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))


;; ------ marginalia ------
(use-package marginalia
  :init
  (marginalia-mode 1))


;; ------ orderless ------
(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)))


;; ------ consult ------
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("C-s" . consult-line)
         ("M-y" . consult-yank-pop)))


;; ----- nerd-icons ------
(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook (dired-mode))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode 1))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;; ------- Which key mode -------
(use-package which-key
  :init (setq which-key-idle-delay 3.0)
  :config (which-key-mode 1))


;; ------- Multiple cursors -------
(use-package multiple-cursors
  :bind (("C-S-c" . mc/edit-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("<return>" . nil)))


;; ------ line-numbers ------
(use-package display-line-numbers
  :ensure nil
  :config
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


;; ------ cmake-mode ------
(use-package cmake-mode)


;; ------ crontab ------
(use-package crontab-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.crontab\\'" . crontab-mode)))


;; ------ bash-completion ------
(use-package bash-completion
  :config
  (bash-completion-setup))

;; ------ dockerfile ------
(use-package dockerfile-mode)


;; ------ markdown ------
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


;; ------ clang-format ------
(use-package clang-format)

(defmacro setup-custom-style (mode)
  `(add-hook ',(derived-mode-hook-name mode)
             (lambda ()
               (setq clang-format-style "WebKit")
               (add-hook 'before-save-hook #'clang-format-buffer nil t))))

(setup-custom-style c-mode)
(setup-custom-style c++-mode)


;; ------ semantic-refactor ------ Me esta dando errores con TRAMP
;; (use-package srefactor
;;   :hook (c++-mode . semantic-mode)
;;   :bind (("C-c r" . srefactor-refactor-at-point)))


;; ------ python-black ------
(use-package python-black
  :after python
  :hook (python-mode . python-black-on-save-mode))


;; ------ eglot ------
(defmacro define-eglot-modes (&rest modes)
  `(progn
     ,@(mapcar (lambda (mode)
                 `(add-hook ',(derived-mode-hook-name mode) 'eglot-ensure))
               modes)))

(define-eglot-modes c-mode c++-mode cmake-mode python-mode sh-mode)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd"
                                      "--header-insertion=never"
                                      "--fallback-style=webkit")))
  (add-to-list 'eglot-server-programs
               '((sh-mode bash-ts-mode) . ("bash-language-server" "start"))))


;; ------ sly ------
(use-package sly
  :init
  (setq sly-lisp-implementations '((sbcl  ("/usr/bin/sbcl"))
                                   (clisp ("/usr/bin/clisp"))))
  :bind (:map sly-mode-map
              ("C-c r" . sly-restart-inferior-lisp)
              ("C-l" . sly-mrepl-clear-repl)
              ("C-<up>" . sly-mrepl-previous-input-or-button)
              ("C-<down>" . sly-mrepl-next-input-or-button)))

(define-key lisp-mode-map (kbd "C-c C-d C-s") #'hyperspec-lookup)


;; ------ scrbl ------
(use-package scribble-mode
  :config
  (modify-syntax-entry ?: "_ " scribble-mode-syntax-table)
  (push `(,(rx (or space "(" "[" "{") (group (zero-or-one "#") ":" (+ (not (any space ")" "]" "}")))))
          (1 font-lock-keyword-face))
        scribble-mode-font-lock-keywords))


;; ------ spacious-padding ------
(use-package spacious-padding
  :hook (after-init . spacious-padding-mode))


;; ------ google translate ------
(use-package google-translate
  :config
  (global-set-key (kbd "C-c t") 'google-translate-at-point)
  (global-set-key (kbd "C-c T") 'google-translate-smooth-translate))


;; ------ gcloud tramp ------
(require 'tramp)
(add-to-list 'tramp-methods
             '("gctest"
               (tramp-login-program        "gcloud compute ssh --project=meteored-test")
               (tramp-login-args           (("%h")))
               (tramp-async-args           (("-q")))
               (tramp-remote-shell         "/bin/sh")
               (tramp-remote-shell-args    ("-c"))
               (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                            ("-o" "UserKnownHostsFile=/dev/null")
                                            ("-o" "StrictHostKeyChecking=no")))
               (tramp-default-port         22)))
(add-to-list 'tramp-methods
             '("gcprod"
               (tramp-login-program        "gcloud compute ssh --project=api-project-858154548956")
               (tramp-login-args           (("%h")))
               (tramp-async-args           (("-q")))
               (tramp-remote-shell         "/bin/sh")
               (tramp-remote-shell-args    ("-c"))
               (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                            ("-o" "UserKnownHostsFile=/dev/null")
                                            ("-o" "StrictHostKeyChecking=no")))
               (tramp-default-port         22)))
;; (add-to-list 'tramp-methods
;;              '("gcssh-test"
;;                (tramp-login-program        "gcloud compute ssh")
;;                (tramp-login-args           (("%h")))
;;                (tramp-async-args           (("-q")))
;;                (tramp-remote-shell         "/bin/sh")
;;                (tramp-remote-shell-args    ("-c"))
;;                (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
;;                                             ("-o" "UserKnownHostsFile=/dev/null")
;;                                             ("-o" "StrictHostKeyChecking=no")))
;;                (tramp-default-port         22)))

;; (add-to-list 'tramp-methods
;;              '("gcssh-prod"
;;                (tramp-login-program        "gcloud compute ssh --project=api-project-858154548956")
;;                (tramp-login-args           (("%h")))
;;                (tramp-async-args           (("-q")))
;;                (tramp-remote-shell         "/bin/sh")
;;                (tramp-remote-shell-args    ("-c"))
;;                (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
;;                                             ("-o" "UserKnownHostsFile=/dev/null")
;;                                             ("-o" "StrictHostKeyChecking=no")))
;;                (tramp-default-port         22)))

(defun prepare-instances (project)
  (let* ((command (concat "gcloud compute instances list --project=" project " --format='value(name)' --filter='status=RUNNING'"))
         (output (shell-command-to-string command))
         (instances (split-string output "\n" t)))
    (mapcar (lambda (instance)
              (list nil instance))
            instances)))

(defun tramp-gcloud-test-completion-function (file)
  "Retorna lista de instancias de Google Cloud Compute."
  (prepare-instances "meteored-test"))

(defun tramp-gcloud-prod-completion-function (file)
  "Retorna lista de instancias de Google Cloud Compute."
  (prepare-instances "api-project-858154548956"))

(tramp-set-completion-function "gctest"
                               '((tramp-gcloud-test-completion-function "")))
(tramp-set-completion-function "gcprod"
                               '((tramp-gcloud-prod-completion-function "")))


;; ------ treemacs ------
(use-package treemacs
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs-add-and-display-current-project)
   ("C-x t d"   . treemacs-select-directory)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag)))


;; ------ copilot ------
(add-to-list 'load-path "~/copilot.el")
(require 'copilot)

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-ignored-server-capabilities '(:inlayHintProvider))
 '(package-selected-packages
   '(bash-completion clang-format+ cmake-mode consult corfu crontab-mode
                     dired-hide-dotfiles dockerfile-mode dogears
                     doom-modeline flymake-python-pyflakes
                     google-translate magit marginalia markdown-mode
                     multiple-cursors nerd-icons-completion
                     nerd-icons-corfu nerd-icons-dired orderless
                     php-mode pylint python-black scribble-mode sly
                     spacious-padding srefactor treemacs
                     typescript-mode vertico vscode-dark-plus-theme
                     which-key yaml-mode yasnippet))
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch
              "main"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sh-heredoc ((t (:foreground "#ce9178")))))
