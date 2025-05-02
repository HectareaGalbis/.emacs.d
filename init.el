
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

;; Al parecer, esto no funciona. El comando solo funciona si se ejecuta una vez estamos dentro del contenedor.

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


;; ------ sly o slime ------
;; Para usar sly o slime necesitamos un compilador de Common Lisp. O dos, ¿por qué no?
;; On terminal: sudo apt install sbcl clisp


;; ------ gptel ------
;; Para usar gptel es necesario usar un modelo de IA. En concreto uso gpt4all.
;; Una vez instalado hay que asegurarse de activar la opcion 'Enable Local API Server'.
;; Instalamos un modelo y especificamos su nombre en la configuracion de gptel.


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
(set-face-attribute 'default nil :height 120)                ; Make font scale a bit larger
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


;; ------ corfu ------
(use-package corfu
  :init
  (global-corfu-mode)
  :bind (:map corfu-map
              ("RET" . nil)
              ("TAB" . #'corfu-complete))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
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
         ("C-s" . consult-line)))


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


;; ------ dockerfile ------
(use-package dockerfile-mode)


;; ------ markdown ------
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


;; ------ clang-format ------
(use-package clang-format)
(add-hook 'c++-mode-hook (lambda ()
                           (setq clang-format-style "WebKit")
                           (add-hook 'before-save-hook #'clang-format-buffer nil t)))


;; ------ eglot ------
(defmacro define-eglot-modes (&rest modes)
  `(progn
     ,@(mapcar (lambda (mode)
                 `(add-hook ',(derived-mode-hook-name mode) 'eglot-ensure))
               modes)))

(define-eglot-modes c++-mode cmake-mode python-mode)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd"
                                      "--header-insertion=never"
                                      "--clang-tidy"
                                      "--fallback-style=webkit")))
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (eglot-inlay-hints-mode -1))))


;; ------ sly/slime ------
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


;; ------ cl-scrbl ------
(add-to-list 'load-path (concat user-emacs-directory "cl-scribble-mode/"))
(load "cl-scribble-mode")


;; ------ visual-fill-column ------
(use-package visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
  (setq-default visual-fill-column-center-text t)
  (setq-default visual-fill-column-width 100))


;; ------ spacious-padding ------
(use-package spacious-padding
  :hook (after-init . spacious-padding-mode))


;; ------ gptel ------
(use-package gptel

  :config
  (setq
   gptel-max-tokens 500
   gptel-model "Meta-Llama-3-8B-Instruct.Q4_0.gguf"
   gptel-backend (gptel-make-gpt4all "GPT4All"
                   :protocol "http"
                   :host "localhost:4891"
                   :models '("Meta-Llama-3-8B-Instruct.Q4_0.gguf"))))
