
;; ------ Automatic emacs lines -------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3199be8536de4a8300eaf9ce6d864a35aa802088c0925e944e2b74a574c68fd0" "a0415d8fc6aeec455376f0cbcc1bee5f8c408295d1c2b9a1336db6947b89dd98" default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(projectile-mode exwm dirtrack ivy slime avy markdown-mode flycheck-pkg-config undo-tree ivy-xref dumb-jump flycheck modern-cpp-font-lock auto-complete pdf-continuous-scroll-mode pdf-tools paredit parinfer-rust multiple-cursors cmake-mode which-key use-package spacemacs-theme solo-jazz-theme solarized-theme rainbow-delimiters projectile parinfer-rust-mode one-themes modus-themes ivy-rich helpful doom-themes doom-modeline counsel))
 '(undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; ------ Starting up emacs ------
(defun initial-setup ()
  (setq inhibit-startup-message t)                             ; Disable startup message
  (setq initial-scratch-message nil)                           ; Disable initial scratch message
  (scroll-bar-mode -1)                                         ; Disable scrollbar
  (horizontal-scroll-bar-mode -1)                              ; Disable scrollbar
  (tool-bar-mode -1)                                           ; Disable the toolbar
  (tooltip-mode -1)                                            ; Disable tooltips
  (menu-bar-mode -1)                                           ; Disable menu bar                          
  (set-fringe-mode 10)                                         ; Give some breathing room
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; Initialize emacs maximized
  (setq frame-resize-pixelwise t)                              ; Adjust to window correctly
  (delete-selection-mode 1)                                    ; Follow the convention of modern editors
  (setq-default indent-tabs-mode nil)                          ; Prevents extraneous tabs
  (set-face-attribute 'default nil :height 120)                ; Make font scale a bit larger
  (setq browse-url-browser-function 'eww-browse-url)           ; Set the the default url browser
  )

(initial-setup)


;; ------ Record mode ------
(defun record-mode-on ()
  (set-face-attribute 'default nil :height 140))

(defun record-mode-off ()
  (set-face-attribute 'default nil :height 120))


;; ----- Emacs backup and autosave files -----
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))             ; Put backup files (ie foo~) in ~/.emacs.d/.
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)                                 ; create the autosave dir if necessary, since emacs won't.
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t))) ; Put autosave files (ie #foo#) in ~/.emacs.d/ (I think)



;; ------ Global keybindings ------
(global-set-key (kbd "C-c d") 'delete-pair)


;; ------ Scrolling and window resizing------
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
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)


;; ----- Melpa -----
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))


;; ----- use-package -----
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t) ; Makes a package to be installed if it isn't


;; ----- Theme -----
(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))
  :bind ("<f5>" . modus-themes-toggle))
(load-theme 'modus-vivendi)


;; ------- Doom modeline -------
(use-package doom-modeline
  :init (doom-modeline-mode 1))


;; ----- thing at point -----
(require 'thingatpt)


;; ----- Slime -----
(use-package slime
  :init
  (setq slime-lisp-implementations         '((sbcl  ("/usr/bin/sbcl")))))

(defun slime-frame ()
  "Init slime in a new frame."
  (interactive)
  (let ((display-buffer-alist '(("\\*inferior-lisp\\*" display-buffer-pop-up-frame (nil)))))
    (slime)))

(defun slime-quit ()
  "Terminate inferior Lisp process and kill the buffer *inferior lisp*."
  (interactive)
  (slime-quit-lisp)
  (kill-buffer "*inferior lisp*"))

(defun hyperspec-lookup-other-window (symbol-name)
  "Another wrapper for hyperspec-lookup"
  (interactive (list (common-lisp-hyperspec-read-symbol-name
                      (slime-symbol-at-point))))
  (let ((window-to-use (or (get-buffer-window "*eww*")
			   (if (and (eq browse-url-browser-function 'eww-browse-url)
				    (= (count-windows) 1))
			       (split-window-right)
			     (next-window)))))
    (with-selected-window (or window-to-use (selected-window))
      (hyperspec-lookup symbol-name))))

(define-key slime-editing-map (kbd "C-c r") 'slime-restart-inferior-lisp)
(define-key slime-editing-map (kbd "C-c q") 'slime-quit)
(define-key slime-editing-map (kbd "C-c C-d C-s") 'hyperspec-lookup-other-window)


(defun create-lisp-image (systems file)
  "Create a core Lisp image with SYSTEMS loaded on it.  Save the image at FILE path."
  (interactive "MWrite the systems to be loaded in the image:\nFWrite the path where to store the image:")
  (let* ((system-names (split-string systems))
	 (eval-forms (apply 'concat (mapcar (lambda (name)
						(format " --eval \"(asdf:load-system :%s)\" " name))
					    system-names)))
	 (save-form (format " --eval \"(sb-ext:save-lisp-and-die #P\\\"%s\\\")\" " file))
	 (command (concat "sbcl " eval-forms save-form)))
    (async-shell-command command)))

(defun slime-create-executable (system file)
  "Create a core Lisp image with SYSTEMS loaded on it.  Save the image at FILE path."
  (interactive "MWrite the systems to be loaded in the image:\nFWrite the path where to store the image:")
  (let* ((system-names (split-string systems))
	 (eval-form (format " --eval \"(asdf:load-system :%s)\" " system))
	 (save-form (format " --eval \"(sb-ext:save-lisp-and-die #P\\\"%s\\\" :toplevel #'main :executable t)\" " file))
	 (command (concat "sbcl " eval-forms save-form)))
    (async-shell-command command)))

(defun slime-from-image (image)
  "Initiate slime with a given core Lisp IMAGE."
  (interactive "fWrite the path where the image is stored:")
  (let* ((core-form (format " --core %s " image))
	 (command (concat "sbcl " core-form)))
    (slime command)))


;; ----- Undo tree -----
(use-package undo-tree
  :bind (("C-x u" . undo-tree-visualize)
	 ("C-/" . custom-undo-tree-undo)
	 ("C-?" . undo-tree-redo)))

(defun activate-undo-tree-on-window-change (window-or-frame)
  "Activate undo-tree when entering at WINDOW-OR-FRAME."
  (when (or (not (boundp 'undo-tree-mode)) (not undo-tree-mode))
    (undo-tree-mode t)))

(setq window-selection-change-functions (cons #'activate-undo-tree-on-window-change window-selection-change-functions))
;(add-to-list 'undo-tree-history-directory-alist `("." . ,(expand-file-name "undo-tree-history/" user-emacs-directory)))


;; -------- Electric pair --------
(add-hook 'emacs-lisp-mode-hook       #'electric-pair-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'electric-pair-mode)
(add-hook 'ielm-mode-hook             #'electric-pair-mode)
(add-hook 'lisp-mode-hook             #'electric-pair-mode)
(add-hook 'lisp-interaction-mode-hook #'electric-pair-mode)
(add-hook 'scheme-mode-hook           #'electric-pair-mode)


;; ------- Show paren -------
(add-hook 'emacs-lisp-mode-hook       #'show-paren-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'show-paren-mode)
(add-hook 'ielm-mode-hook             #'show-paren-mode)
(add-hook 'lisp-mode-hook             #'show-paren-mode)
(add-hook 'lisp-interaction-mode-hook #'show-paren-mode)
(add-hook 'scheme-mode-hook           #'show-paren-mode)


;; ------- Line numbers -------
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(eshell-mode-hook)) ; Indicate in which modes we don't want this mode
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; ------- Which key mode -------
(use-package which-key
  :init (setq which-key-idle-delay 3.0)
  :config (which-key-mode))


;; ------- Ivy mode -------
(use-package ivy
  :init (ivy-mode)
  :bind (("C-s" . swiper)
	 ("C-S-s" . swiper-isearch-thing-at-point)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


;; ------ Ivy rich ------
(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))


;; ------ Counsel ------
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))


;; ----- Ivy xref -----
(use-package ivy-xref
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))


;; ------ helpful ------
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;; ------ projectile ------
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))


;; ------- Multiple cursors -------
(use-package multiple-cursors)
(global-set-key (kbd "C-S-c") 'mc/edit-lines)
(define-key mc/keymap (kbd "<return>") nil)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)


;; ------ org-bullets ------
(add-to-list 'load-path "~/.emacs.d/org-bullets/")
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; ----- Modern C++ font -----
(use-package modern-cpp-font-lock)


;; ----- Dumb jump -----
(use-package dumb-jump)
(setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions))
(add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)


;; ----- Avy -----
(use-package avy
  :bind ("M-s" . avy-goto-char-2))


;; ---- Dirtrack ----
(use-package dirtrack) 
(add-hook 'shell-mode-hook
	  (lambda ()
            (shell-dirtrack-mode 0) 
            (setq dirtrack-list '(":*\\([A-Za-z]*:*~*[\/\\].*?\\)[^-+A-Za-z0-9_.()//\\ ]" 1)) 
            (dirtrack-mode))) 

(defun shell-dir (name dir)
  (interactive "sShell name: \nDDirectory: ")
  (let ((default-directory dir))
    (shell name)))


;; ----- Markdown -----
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))


;; ----- Shell -----
(global-set-key (kbd "C-c t") 'shell)


;; ----- UE4Editor -----
(defvar ue4-projects '(("ProjectChaos" "/media/hectarea/d7101242-e372-47ae-80ea-4bb601f1c53c/UnrealEngine/PChaos/ProjectChaos")
		       ("Sandbox" "/media/hectarea/d7101242-e372-47ae-80ea-4bb601f1c53c/UnrealEngine/Sandbox")
		       ("ProjectRouter" "/media/hectarea/d7101242-e372-47ae-80ea-4bb601f1c53c/UnrealEngine/ProjectRouter/ProjectRouter")))
(defvar ue4-editor "/media/hectarea/d7101242-e372-47ae-80ea-4bb601f1c53c/UnrealEngine/UnrealEngine-4.27/Engine/Binaries/Linux/UE4Editor")
(defvar ue5-projects '(("LEVEL_UP_Project" "/home/hectarea/Unreal Projects/LEVEL_UP_Project")))
(defvar ue5-editor "/media/hectarea/d7101242-e372-47ae-80ea-4bb601f1c53c/UnrealEngine/UnrealEngine5/Engine/Binaries/Linux/UnrealEditor-Cmd")


(defun get-ue4-projects ()
  (mapcar #'car ue4-projects))

(defun get-ue4-project-directory (project)
  (let ((project-found nil)
	(rest-ue4-projects ue4-projects))
    (while (and (not project-found) (not (null rest-ue4-projects)))
      (let ((current-project (caar rest-ue4-projects)))
	(when (equal current-project project)
	  (setq project-found (cadar rest-ue4-projects)))
	(setq rest-ue4-projects (cdr rest-ue4-projects))))
    (directory-file-name project-found)))

(defun ue4-compile-project ()
  (interactive)
  (ivy-read "Select UE4 project: " (get-ue4-projects)
	    :require-match t
	    :action (lambda (project)
		      (if (y-or-n-p (concat "Do you really want to compile " project "?"))
			  (let ((project-dir (get-ue4-project-directory project)))
			    (unless project-dir
			      (error "Project '%s' not found" project))
			    (let* ((makefile (concat project-dir "/Makefile"))
				   (command (concat "make -f" makefile " " project "Editor")))
			      (async-shell-command command)))
			(ue4-compile-project)))))

(defun ue4-open-project ()
  (interactive)
  (ivy-read "Select UE4 project: " (get-ue4-projects)
	    :require-match t
	    :action (lambda (project)
		      (if (y-or-n-p (concat "Do you really want to open " project "?"))
			  (let ((project-dir (get-ue4-project-directory project)))
			    (unless project-dir
			      (error "Project '%s' not found" project))
			    (let* ((uproject (concat project-dir "/" project ".uproject"))
				   (command (concat ue4-editor " " uproject)))
			      (async-shell-command command)))
			(ue4-open-project)))))

(defun ue4-run-editor ()
  (interactive)
  (if (y-or-n-p "Do you really want to open the Unreal Engine 4 editor?")
      (async-shell-command ue4-editor)))


(defun get-ue5-projects ()
  (mapcar #'car ue5-projects))

(defun get-ue5-project-directory (project)
  (let ((project-found nil)
	(rest-ue5-projects ue5-projects))
    (while (and (not project-found) (not (null rest-ue5-projects)))
      (let ((current-project (caar rest-ue5-projects)))
	(when (equal current-project project)
	  (setq project-found (cadar rest-ue5-projects)))
	(setq rest-ue5-projects (cdr rest-ue5-projects))))
    (directory-file-name project-found)))

(defun ue5-compile-project ()
  (interactive)
  (ivy-read "Select UE5 project: " (get-ue5-projects)
	    :require-match t
	    :action (lambda (project)
		      (if (y-or-n-p (concat "Do you really want to compile " project "?"))
			  (let ((project-dir (get-ue5-project-directory project)))
			    (unless project-dir
			      (error "Project '%s' not found" project))
			    (let* ((makefile (concat project-dir "/Makefile"))
				   (command (concat "make -f" makefile " " project "Editor")))
			      (async-shell-command command)))
			(ue5-compile-project)))))

(defun ue5-open-project ()
  (interactive)
  (ivy-read "Select UE5 project: " (get-ue5-projects)
	    :require-match t
	    :action (lambda (project)
		      (if (y-or-n-p (concat "Do you really want to open " project "?"))
			  (let ((project-dir (get-ue5-project-directory project)))
			    (unless project-dir
			      (error "Project '%s' not found" project))
			    (let* ((uproject (concat "\"" project-dir "/" project ".uproject" "\""))
				   (command (concat ue5-editor " " uproject)))
			      (async-shell-command command)))
			(ue5-open-project)))))


(defun ue5-run-editor ()
  (interactive)
  (if (y-or-n-p "Do you really want to open the Unreal Engine 5 editor?")
      (async-shell-command ue5-editor)))
