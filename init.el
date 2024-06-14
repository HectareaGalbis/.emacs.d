

;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------


;; En esta primera parte del fichero de configuración se ajusta el comportamiento de emacs de manera global y usando
;; únicamente variables propias de emacs. Se cambian cosas como algunos atajos de teclado o la apariencia de emacs.


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

;; Global keybindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
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


;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------


;; En esta parte se instalan paquetes que estarán activados de manera global.


;; ----- Melpa -----
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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


;; ------ vertico ------
(use-package vertico
  :config
  (vertico-mode 1)
  (setq vertico-cycle t))

(use-package vertico-directory
  :ensure nil
  :bind (:map vertico-map
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

;; Recordar evaluar esto para instalar los iconos si es la primera vez que se usa este archivo de configuracion.
;; (nerd-icons-install-fonts)

(use-package nerd-icons-dired
  :hook (dired-mode))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode 1))


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


;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------


;; En esta sección se instalan . Cada modo mayor se encarga de elegir qué modos menores activar.


;; ------ company ------
;; (use-package company
;;   :bind (:map company-active-map
;;               ("<tab>" . company-complete-selection)))



;; ----- Slime -----
;; (use-package slime
;;   :init
;;   (setq slime-lisp-implementations         '((sbcl  ("/usr/bin/sbcl"))))
;;   :config
;;   (slime-setup '(slime-fancy slime-company)))

;; (defun slime-frame ()
;;   "Init slime in a new frame."
;;   (interactive)
;;   (let ((display-buffer-alist '(("\\*inferior-lisp\\*" display-buffer-pop-up-frame (nil)))))
;;     (slime)))

;; (defun slime-remote ()
;;   (interactive)
;;   (shell-command "ssh -M -S ~/slime-tunnel -o \"ExitOnForwardFailure yes\" -L4005:localhost:4005 -fN root@134.122.82.19")
;;   (slime-connect "localhost" 4005))

;; (defun slime-quit ()
;;   "Terminate inferior Lisp process and kill the buffer *inferior lisp*."
;;   (interactive)
;;   (if (get-buffer "*inferior-lisp*")
;;       (slime-quit-lisp)
;;     (slime-disconnect)
;;     (shell-command "ssh -S ~/slime-tunnel -O exit root@134.122.82.19"))
;;   (message "Connection closed."))

;; (defun hyperspec-lookup-other-window (symbol-name)
;;   "Another wrapper for hyperspec-lookup"
;;   (interactive (list (common-lisp-hyperspec-read-symbol-name
;;                       (slime-symbol-at-point))))
;;   (let ((window-to-use (or (get-buffer-window "*eww*")
;; 			   (if (and (eq browse-url-browser-function 'eww-browse-url)
;; 				    (= (count-windows) 1))
;; 			       (split-window-right)
;; 			     (next-window)))))
;;     (with-selected-window (or window-to-use (selected-window))
;;       (hyperspec-lookup symbol-name))))

;; (defun slime-restart ()
;;   (interactive)
;;   (slime-restart-inferior-lisp)
;;   (slime-repl-clear-buffer))

;; (define-key slime-editing-map (kbd "C-c r") 'slime-restart)
;; (define-key slime-editing-map (kbd "C-c q") 'slime-quit)
;; (define-key slime-editing-map (kbd "C-c e") 'slime-eval-buffer)
;; (define-key slime-editing-map (kbd "C-c C-d C-s") 'hyperspec-lookup-other-window)


;; (defun create-lisp-image (systems file)
;;   "Create a core Lisp image with SYSTEMS loaded on it.  Save the image at FILE path."
;;   (interactive "MWrite the systems to be loaded in the image:\nFWrite the path where to store the image:")
;;   (let* ((system-names (split-string systems))
;; 	 (eval-forms (apply 'concat (mapcar (lambda (name)
;; 						(format " --eval \"(asdf:load-system :%s)\" " name))
;; 					    system-names)))
;; 	 (save-form (format " --eval \"(sb-ext:save-lisp-and-die #P\\\"%s\\\")\" " file))
;; 	 (command (concat "sbcl " eval-forms save-form)))
;;     (async-shell-command command)))

;; (defun slime-create-executable (system file)
;;   "Create a core Lisp image with SYSTEMS loaded on it.  Save the image at FILE path."
;;   (interactive "MWrite the systems to be loaded in the image:\nFWrite the path where to store the image:")
;;   (let* ((system-names (split-string systems))
;; 	 (eval-form (format " --eval \"(asdf:load-system :%s)\" " system))
;; 	 (save-form (format " --eval \"(sb-ext:save-lisp-and-die #P\\\"%s\\\" :toplevel #'main :executable t)\" " file))
;; 	 (command (concat "sbcl " eval-forms save-form)))
;;     (async-shell-command command)))

;; (defun slime-from-image (image)
;;   "Initiate slime with a given core Lisp IMAGE."
;;   (interactive "fWrite the path where the image is stored:")
;;   (let* ((core-form (format " --core %s " image))
;; 	 (command (concat "sbcl " core-form)))
;;     (slime command)))


;; ;; ------ scrbl ------
;; (use-package scribble-mode
;;   :config
;;   (modify-syntax-entry ?: "_ " scribble-mode-syntax-table)
;;   (push `(,(rx (or space "(" "[" "{") (group (zero-or-one "#") ":" (+ (not (any space ")" "]" "}")))))
;;           (1 font-lock-keyword-face))
;;         scribble-mode-font-lock-keywords))



;; ;; ------ slime-company ------
;; (use-package slime-company
;;   :after (slime company scribble-mode)
;;   :config
;;   (push 'scribble-mode slime-company-major-modes)
;;   (setq slime-company-completion 'fuzzy))




;; ;; ------ org-mode ------
;; (require 'org)
;; (setq org-hide-emphasis-markers t)
;; (setq org-support-shift-select 'always)
;; (setq org-startup-with-inline-images t)
;; (setq org-image-actual-width nil)

;; (define-key org-mode-map (kbd "C-M-<up>") 'org-metaup)
;; (define-key org-mode-map (kbd "M-<up>") 'custom-scroll-down)
;; (define-key org-mode-map (kbd "C-M-<down>") 'org-metadown)
;; (define-key org-mode-map (kbd "M-<down>") 'custom-scroll-up)

;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))
;;                           ("^\\*+ "
;;                            (0
;;                             (prog1 nil
;;                               (put-text-property (match-beginning 0) (match-end 0)
;;                                                  'invisible t))))))

;; (let* ((variable-tuple
;;         (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;               ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;   (custom-theme-set-faces
;;    'user
;;    `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.3))))
;;    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 2.0))))
;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.5 :underline nil))))))

;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "ETBembo" :height 150 :weight thin))))
;;  '(fixed-pitch ((t (:family "Fira Code Retina" :height 120)))))

;; (add-hook 'org-mode-hook 'variable-pitch-mode)
;; (add-hook 'org-mode-hook 'visual-line-mode)
;; ;;(add-hook 'org-mode-hook 'org-indent-mode)

;; (custom-theme-set-faces
;;  'user
;;  '(org-block ((t (:inherit fixed-pitch))))
;;  '(org-code ((t (:inherit (shadow fixed-pitch)))))
;;  '(org-document-info ((t (:foreground "dark orange"))))
;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;;  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
;;  '(org-link ((t (:foreground "royal blue" :underline t))))
;;  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;  '(org-property-value ((t (:inherit fixed-pitch))) t)
;;  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;;  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;;  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; (defun org-insert-literal-character (c)
;;   "Insert a literal character at point."
;;   (interactive "cWhat character?")
;;   (insert ?\u200B c ?\u200B))

;; (defun org-insert-lisp-src-block-with-results ()
;;   (interactive)
;;   (insert "#+begin_src lisp :exports both :eval never-export
;; ")
;;   (save-excursion
;;     (insert "
;; #+end_src")))

;; (defun org-insert-lisp-src-block ()
;;   (interactive)
;;   (insert "#+begin_src lisp
;; ")
;;   (save-excursion
;;     (insert "
;; #+end_src")))

;; (define-key org-mode-map (kbd "C-c i l") 'org-insert-literal-character)
;; (define-key org-mode-map (kbd "C-c s r") 'org-insert-lisp-src-block-with-results)
;; (define-key org-mode-map (kbd "C-c s s") 'org-insert-lisp-src-block)


;; ;; ------ babel ------
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((lisp . t)))

;; (defun org-babel-execute-src-block (&optional arg info params)
;;   "Execute the current source code block.
;; Insert the results of execution into the buffer.  Source code
;; execution and the collection and formatting of results can be
;; controlled through a variety of header arguments.

;; With prefix argument ARG, force re-execution even if an existing
;; result cached in the buffer would otherwise have been returned.

;; Optionally supply a value for INFO in the form returned by
;; `org-babel-get-src-block-info'.

;; Optionally supply a value for PARAMS which will be merged with
;; the header arguments specified at the front of the source code
;; block."
;;   (interactive)
;;   (let* ((org-babel-current-src-block-location
;; 	  (or org-babel-current-src-block-location
;; 	      (nth 5 info)
;; 	      (org-babel-where-is-src-block-head)))
;; 	 (info (if info (copy-tree info) (org-babel-get-src-block-info))))
;;     ;; Merge PARAMS with INFO before considering source block
;;     ;; evaluation since both could disagree.
;;     (cl-callf org-babel-merge-params (nth 2 info) params)
;;     (when (org-babel-check-evaluate info)
;;       (cl-callf org-babel-process-params (nth 2 info))
;;       (let* ((params (nth 2 info))
;; 	     (cache (let ((c (cdr (assq :cache params))))
;; 		      (and (not arg) c (string= "yes" c))))
;; 	     (new-hash (and cache (org-babel-sha1-hash info :eval)))
;; 	     (old-hash (and cache (org-babel-current-result-hash)))
;; 	     (current-cache (and new-hash (equal new-hash old-hash))))
;; 	(cond
;; 	 (current-cache
;; 	  (save-excursion		;Return cached result.
;; 	    (goto-char (org-babel-where-is-src-block-result nil info))
;; 	    (forward-line)
;; 	    (skip-chars-forward " \t")
;; 	    (let ((result (org-babel-read-result)))
;; 	      (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
;; 	      result)))
;; 	 ((org-babel-confirm-evaluate info)
;; 	  (let* ((lang (nth 0 info))
;; 		 (result-params (cdr (assq :result-params params)))
;; 		 ;; Expand noweb references in BODY and remove any
;; 		 ;; coderef.
;; 		 (body
;; 		  (let ((coderef (nth 6 info))
;; 			(expand
;; 			 (if (org-babel-noweb-p params :eval)
;; 			     (org-babel-expand-noweb-references info)
;; 			   (nth 1 info))))
;; 		    (if (not coderef) expand
;; 		      (replace-regexp-in-string
;; 		       (org-src-coderef-regexp coderef) "" expand nil nil 1))))
;;                  (body (if (equal lang "lisp")
;;                            (format "
;; (macrolet ((#1=#:functional-and-script ()
;;              (let ((output (gensym)) 
;;                    (result (gensym)))
;;                `(let* ((,output (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
;;                        (,result (multiple-value-list (with-output-to-string (*standard-output* ,output)
;;                                                        %s
;;                                                        ))))
;;                   (format nil \"~a~&~{~s~^~&~}\" ,output ,result)))))
;;   (#1#))"
;;                                    body)
;;                          body))
;; 		 (dir (cdr (assq :dir params)))
;; 		 (mkdirp (cdr (assq :mkdirp params)))
;; 		 (default-directory
;; 		   (cond
;; 		    ((not dir) default-directory)
;; 		    ((member mkdirp '("no" "nil" nil))
;; 		     (file-name-as-directory (expand-file-name dir)))
;; 		    (t
;; 		     (let ((d (file-name-as-directory (expand-file-name dir))))
;; 		       (make-directory d 'parents)
;; 		       d))))
;; 		 (cmd (intern (concat "org-babel-execute:" lang)))
;; 		 result)
;; 	    (unless (fboundp cmd)
;; 	      (error "No org-babel-execute function for %s!" lang))
;; 	    (message "executing %s code block%s..."
;; 		     (capitalize lang)
;; 		     (let ((name (nth 4 info)))
;; 		       (if name (format " (%s)" name) "")))
;; 	    (if (member "none" result-params)
;; 		(progn (funcall cmd body params)
;; 		       (message "result silenced"))
;; 	      (setq result
;; 		    (let ((r (funcall cmd body params)))
;; 		      (if (and (eq (cdr (assq :result-type params)) 'value)
;; 			       (or (member "vector" result-params)
;; 				   (member "table" result-params))
;; 			       (not (listp r)))
;; 			  (list (list r))
;; 			r)))
;; 	      (let ((file (and (member "file" result-params)
;; 			       (cdr (assq :file params)))))
;; 		;; If non-empty result and :file then write to :file.
;; 		(when file
;; 		  ;; If `:results' are special types like `link' or
;; 		  ;; `graphics', don't write result to `:file'.  Only
;; 		  ;; insert a link to `:file'.
;; 		  (when (and result
;; 			     (not (or (member "link" result-params)
;; 				      (member "graphics" result-params))))
;; 		    (with-temp-file file
;; 		      (insert (org-babel-format-result
;; 			       result
;; 			       (cdr (assq :sep params))))))
;; 		  (setq result file))
;; 		;; Possibly perform post process provided its
;; 		;; appropriate.  Dynamically bind "*this*" to the
;; 		;; actual results of the block.
;; 		(let ((post (cdr (assq :post params))))
;; 		  (when post
;; 		    (let ((*this* (if (not file) result
;; 				    (org-babel-result-to-file
;; 				     file
;; 				     (let ((desc (assq :file-desc params)))
;; 				       (and desc (or (cdr desc) result)))))))
;; 		      (setq result (org-babel-ref-resolve post))
;; 		      (when file
;; 			(setq result-params (remove "file" result-params))))))
;; 		(org-babel-insert-result
;; 		 result result-params info new-hash lang)))
;; 	    (run-hooks 'org-babel-after-execute-hook)
;; 	    result)))))))

;; ;; ------ htmlize ------
;; (use-package htmlize)


;; ;; ------ ox-publish ------
;; (require 'ox-publish)

;; (defun org-publish-forced (project)
;;   "Publish PROJECT but forced."
;;   (interactive
;;    (list (assoc (completing-read "Publish project: "
;; 				 org-publish-project-alist nil t)
;; 		org-publish-project-alist)))
;;   (org-publish project t))

;; (setq user-full-name "Héctor Galbis Sanchis")
;; (setq user-mail-address "hectometrocuadrado@gmail.com")
;; (setq org-export-default-language "es")
;; (setq org-html-metadata-timestamp-format "%d-%m-%Y")
;; (setq org-export-html-date-format-string "%d-%m-%Y")
;; (setq org-html-link-up "https://lispylambda.es")
;; (setq org-html-link-home "https://lispylambda.es")

;; (setq org-publish-project-alist
;;       (list
;;        (list "main"
;;              :base-directory "~/lispylambda/"
;;              :base-extension "org"
;;              :publishing-directory "/ssh:root@lispylambda.es:~/blog-site/"
;;              :publishing-function 'org-html-publish-to-html
;;              :html-head "<link rel=\"stylesheet\" href=\"/css/simple.css\" type=\"text/css\"/>"
;;              :section-numbers nil
;;              :with-toc nil
;;              :html-postamble nil)

;;        (list "common-lisp"
;;              :recursive t
;;              :base-directory "~/lispylambda/posts/common-lisp/"
;;              :base-extension "org"
;;              :publishing-directory "/ssh:root@lispylambda.es:~/blog-site/posts/common-lisp/"
;;              :publishing-function 'org-html-publish-to-html
;;              :time-stamp-file t
;;              :html-head "<link rel=\"stylesheet\" href=\"/css/simple.css\" type=\"text/css\"/>"
;;              :html-postamble (format "<p><a href=\"%s\">UP</a> | <a href=\"%s\">HOME</a></p><p></p><p>Autor: %s <%s></p><p>Última edición: %s</p>"
;;                                      org-html-link-up
;;                                      org-html-link-home
;;                                      "%a" "%e" "%C")
;;              :with-toc 2
;;              :section-numbers nil
;;              :auto-sitemap t
;;              :sitemap-filename "common-lisp-sitemap.org"
;;              :sitemap-title ""
;;              :sitemap-sort-files 'chronologically
;;              :sitemap-format-entry (lambda (file style project)
;;                                      (format "(%s) [[file:%s][%s]]"
;;                                              (org-format-time-string org-export-html-date-format-string
;;                                                                      (org-publish-find-date file project))
;;                                              file
;;                                              (org-publish-find-title file project))))
       
;;        (list "UnrealEngine"
;;              :recursive t
;;              :base-directory "~/lispylambda/posts/UnrealEngine/"
;;              :base-extension "org"
;;              :publishing-directory "/ssh:root@lispylambda.es:~/blog-site/posts/UnrealEngine/"
;;              :publishing-function 'org-html-publish-to-html
;;              :time-stamp-file t
;;              :html-head "<link rel=\"stylesheet\" href=\"/css/simple.css\" type=\"text/css\"/>"
;;              :html-postamble (format "<p><a href=\"%s\">UP</a> | <a href=\"%s\">HOME</a></p><p></p><p>Autor: %s <%s></p><p>Última edición: %s</p>"
;;                                      org-html-link-up
;;                                      org-html-link-home
;;                                      "%a" "%e" "%C")
;;              :with-toc 2
;;              :section-numbers nil
;;              :auto-sitemap t
;;              :sitemap-filename "UnrealEngine-sitemap.org"
;;              :sitemap-title ""
;;              :sitemap-sort-files 'chronologically
;;              :sitemap-format-entry (lambda (file style project)
;;                                      (format "(%s) [[file:%s][%s]]"
;;                                              (org-format-time-string org-export-html-date-format-string
;;                                                                      (org-publish-find-date file project))
;;                                              file
;;                                              (org-publish-find-title file project))))

;;        (list "images"
;;              :base-directory "~/lispylambda/images/"
;;              :recursive t
;;              :base-extension "png\\|gif\\|png"
;;              :publishing-directory "/ssh:root@lispylambda.es:~/blog-site/images/"
;;              :publishing-function 'org-publish-attachment)

;;        (list "css"
;;              :base-directory "~/lispylambda/css/"
;;              :base-extension "css\\|el"
;;              :publishing-directory "/ssh:root@lispylambda.es:~/blog-site/css/"
;;              :publishing-function 'org-publish-attachment)

;;        (list "lispylambda"
;;              :components '("css" "images" "common-lisp" ;; "UnrealEngine"
;;                            "main"))))

;; (defun org-publish-update-lispylambda ()
;;   "Update the posts of lispylambda site."
;;   (interactive)
;;   (org-publish-project "images")
;;   (org-publish-project "css")
;;   (org-publish-project "common-lisp")
;;   ;;(org-publish-project "UnrealEngine")
;;   (org-publish-project "main" t))


;; ;; ------ visual-fill-column ------
;; (use-package visual-fill-column
;;   :config
;;   (add-hook 'org-mode-hook 'visual-fill-column-mode)
;;   (setq-default visual-fill-column-center-text t))


;; ;; ------ toc-org ------
;; (use-package toc-org
;;   :config
;;   (add-hook 'org-mode-hook 'toc-org-mode))


;; ;; ------ hunspell ------
;; (setq ispell-program-name "hunspell")


;; ;; ------ langtool ------
;; ;;(setq langtool-language-tool-jar "/snap/languagetool/36/usr/bin/languagetool-commandline.jar")
;; ;;(use-package langtool)


;; ;; ------ flyspell ------
;; (add-hook 'org-mode-hook 'flyspell-mode)


;; ;; ----- Modern C++ font -----
;; (use-package modern-cpp-font-lock)


;; ;; ----- Markdown -----
;; (use-package markdown-mode
;;   :mode ("README\\.md\\'" . gfm-mode)
;;   :init (setq markdown-command "pandoc"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(helpful multiple-cursors nerd-icons-completion line-numbers which-key vertico-directory all-the-icons-completion marginalia emacs-lisp orderless vertico consult electric-pair dired-hide-dotfiles all-the-icons-dired magit all-the-icons doom-modeline vscode-dark-plus-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
