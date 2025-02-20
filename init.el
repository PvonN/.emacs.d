;;; -*- lexical-binding: t -*-

(defun tangle-init ()
  "If the current buffer is init.org the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(use-package gcmh
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 100 1024 1024))  ; 100mb
  (gcmh-mode 1))

(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq native-comp-async-report-warnings-errors 'silent) ;; native-comp warning
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(eval-when-compile
  (dolist (sym '(cl-flet lisp-complete-symbol))
    (setplist sym (use-package-plist-delete
                   (symbol-plist sym) 'byte-obsolete-info))))

(setq which-func-update-delay 1.0)

(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq user-emacs-directory "~/.emacs.d/")
(setq default-directory "~/")

(set-language-environment    "UTF-8")
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

(setq large-file-warning-threshold (* 50 1024 1024))

(require 'package)
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("ORG"          . "https://orgmode.org/elpa/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities
      '(("GNU ELPA"     . 20)
        ("MELPA"        . 15)
        ("ORG"          . 10)
        ("MELPA Stable" . 5)
        ("nongnu"       . 0)))
(package-initialize)

(defvar custom-bindings-map (make-sparse-keymap) 
  "Keymap for custom bindings.")

(use-package magit)

(use-package projectile
  :ensure t
  :init
  (when (file-directory-p "~/Documents/")
    (setq projectile-project-search-path '("~/Documents/Kompositionen"
					   "~/Documents/Code"
					   "~/Documents/DeterministicJitter"
					   "~/Documents/Reaper"
					   "~/Documenqts/Kompositionen-Video"
					   "~/Documents/Projekte"
					   "~/Documents/Utilities")))
  (setq projectile-switch-project-action #'treemacs)
  :bind-keymap
  ("s-p" . projectile-command-map)
  :config
  (projectile-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-startup-banner "/Users/philippneumann/.emacs.d/images/emacs-gnu.png")
  (dashboard-banner-logo-title nil)
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
(dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-footer nil)
  (dashboard-projects-backend 'projectile)
  (dashboard-items '(
		     (recents . 5)
		     (projects . 5)
		     (bookmarks . 5)
		     (agenda . 24))))

(use-package treemacs
  :ensure t
  :bind ("C-x C-t" . treemacs)
  :after doom-themes
  :custom
  (treemacs-is-never-other-window t)
  (setq doom-themes-treemacs-theme "doom-colors")
  :hook
  ((treemacs-mode . treemacs-project-follow-mode)
   (treemacs-mode . (lambda () (display-line-numbers-mode 0)))))

(use-package treemacs-nerd-icons
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

(dolist (mode
         '(tool-bar-mode       ;; Remove toolbar
           scroll-bar-mode     ;; Remove scollbars
           menu-bar-mode    ;; Remove menu bar          
	   tooltip-mode))        ;; Remove tooltips
  (funcall mode 0))

(setq inhibit-startup-message           t       ;; No startup message
      inhibit-startup-echo-area-message t       ;; No startup message in echo area
      inhibit-startup-screen            t       ;; No default startup screen
      initial-major-mode                'fundamental-mode
      ring-bell-function                'ignore ;; No bell
      cursor-type 'box
	  visible-bell                        t                  ;; Visble bell
      display-time-default-load-average nil     ;; Don't show me load time
      scroll-margin                     0       ;; Space between top/bottom
      use-dialog-box                    nil)    ;; Disable dialog

(add-to-list 'default-frame-alist     '(fullscreen . maximized))
;; (add-hook 'window-setup-hook          'toggle-frame-fullscreen t)  ;; F11

(use-package spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode))

(defun solaire-mode-real-buffer-custom-p ()
  "Return t if the current buffer is the dashboard or scratch, or is a real (file-visiting) buffer."
  (cond ((string= (buffer-name (buffer-base-buffer)) "*dashboard*") t)        
        ((buffer-file-name (buffer-base-buffer)) t)
        (t nil)))

(use-package solaire-mode
  :ensure t
  :hook (after-init . solaire-global-mode)
  :init (setq solaire-mode-real-buffer-fn #'solaire-mode-real-buffer-custom-p))

(use-package golden-ratio
  :ensure t
  :hook (after-init . golden-ratio-mode))

(add-to-list 'default-frame-alist '(internal-border-width . 16))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(set-fringe-mode 10)                          ;; Set fringe width to 10

(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil) ;; Otherwise shows a corner icon on the edge
(setq-default indicate-empty-lines nil)       ;; Otherwise there are weird fringes on blank lines

(set-face-attribute 'header-line t :inherit 'default)

(when (eq system-type 'darwin)
  ; no title bar
  (add-to-list 'default-frame-alist '(undecorated-round . t))
  ; don't use proxy icon
  (setq ns-use-proxy-icon nil)
  ; don't show buffer name in title bar
  (setq frame-title-format ""))

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(show-paren-mode t) ;; Highlight matching parentheses

;; Disable line numbers for specific modes
(dolist (mode '(prog-mode-hook
                csound-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(defvar pvn/font-height 180)

(when (eq system-type 'darwin)
  (setq pvn/font-height 180))

(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata" :height pvn/font-height)
  (set-face-attribute 'fixed-pitch nil :family "Inconsolata"))

(when (member "Open Sans" (font-family-list))
  (set-face-attribute 'variable-pitch nil :family "Open Sans"))

(use-package mixed-pitch
  :defer t
  :hook ((org-mode   . mixed-pitch-mode)
         (LaTeX-mode . mixed-pitch-mode)))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  )

(use-package emojify
  :config
  (when (member "Apple Color Emoji" (font-family-list))
    (set-fontset-font
      t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)))

(use-package doom-themes
  :config
  (doom-themes-visual-bell-config)  ;; Enable flashing mode-line on  errors
  ;; Enable custom neotree theme (all-the-icons must be installed!)
					;  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (set-face-background 'font-lock-comment-face nil)
  (setq doom-themes-enable-bold t     ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(setq custom-safe-themes t)

(defvar pvn/default-dark-theme  'doom-vibrant)
(defvar pvn/default-light-theme 'doom-one-light)

(defvar pvn/default-dark-accent-colour  "SkyBlue4")
(defvar pvn/default-light-accent-colour "#CEE4F5")

(load-theme pvn/default-dark-theme t)

(defun disable-custom-themes (theme &optional no-confirm no-enable)
  (mapc 'disable-theme custom-enabled-themes))

(advice-add 'load-theme :before #'disable-custom-themes)

(column-number-mode t) ;; Show current column number in mode line

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (column-number-mode 1)
  :custom
  (doom-modeline-height 20)
  (doom-modeline-window-width-limit nil)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-env-python-executable "python")
  (doom-modeline-vcs-max-length 50))

(use-package olivetti
  :defer t
  :bind (:map custom-bindings-map ("C-c o" . olivetti-mode))
  :config
  (setq olivetti-style t
		;;olivetti-body-width 120
		olivetti-minimum-body-width 72
		olivetti-recall-visual-line-mode-entry-state t))

(add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 0.65)))

(use-package adaptive-wrap
  :defer t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package writeroom-mode
  :defer t)

(use-package focus
  :defer t)

(use-package presentation
  :defer t
  :config
  (setq presentation-default-text-scale 2.5))

(global-visual-line-mode t) ;; Break lines instead of truncating them
(global-auto-revert-mode t) ;; Revert buffers automatically when they change
(recentf-mode            t) ;; Remember recently opened files
(savehist-mode           t) ;; Remember minibuffer prompt history

(setq auto-revert-interval         1         ;; Refresh buffers fast
      auto-revert-verbose          nil       ;; Don't notify me about reverts
      echo-keystrokes              0.1       ;; Show keystrokes fast
      frame-inhibit-implied-resize 1         ;; Don't resize frame implicitly
      sentence-end-double-space    nil       ;; No double spaces
      recentf-max-saved-items      1000      ;; Show more recent files
      use-short-answers            t         ;; 'y'/'n' instead of 'yes'/'no' etc.
      save-interprogram-paste-before-kill t  ;; Save copies between programs
      history-length               25        ;; Only save the last 25 minibuffer prompts
      global-auto-revert-non-file-buffers t) ;; Revert Dired and other buffers

(setq-default tab-width 4)  ;; Smaller tabs

(setq scroll-conservatively            101
      mouse-wheel-follow-mouse         't
      mouse-wheel-progressive-speed    nil
      ;; Scroll 1 line at a time, instead of default 5
      ;; Hold shift to scroll faster and meta to scroll very fast
      mouse-wheel-scroll-amount        '(1 ((shift) . 3) ((meta) . 6)))

;; (Native) smooooooth scrolling
(setq pixel-scroll-precision-mode t)

(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll    nil)

(use-package browse-kill-ring
  :defer t)

(defvar emacs-autosave-directory
  (concat user-emacs-directory ".backupfiles/")
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

(setq-default auto-fill-function 'do-auto-fill)
(setq fill-column 80)

(use-package undo-fu
  :defer t
  :bind (:map custom-bindings-map
              ("C-_" . undo-fu-only-undo)
              ("M-_" . undo-fu-only-redo)))

(use-package move-dup
  :bind (:map custom-bindings-map
              ("C-M-<up>"   . move-dup-move-lines-up)
			  ("C-M-<down>" . move-dup-move-lines-down)
))

(defun pvn/take-me-home ()
  (interactive)
  (if (looking-back "/" nil)
      (progn (call-interactively 'delete-minibuffer-contents) (insert "~/"))
    (call-interactively 'self-insert-command)))

(use-package vertico
  :defer t
  :bind (:map vertico-map ("~" . pvn/take-me-home))
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  (setq read-extended-command-predicate       'command-completion-default-include-p
	vertico-cycle t ; allow cycling the buffer
	vertico-count                         10  ; Show more candidates
        read-file-name-completion-ignore-case t   ; Ignore case of file names
        read-buffer-completion-ignore-case    t   ; Ignore case in buffer completion
        completion-ignore-case                t)) ; Ignore case in completion

(use-package vertico-posframe
    :init
    (setq vertico-posframe-parameters   
		  '((left-fringe  . 8)    ;; Fringes
            (right-fringe . 8))) 
    :config
    (vertico-posframe-mode 1)
    :custom
    (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic partial-completion)
        completion-category-overrides '((file (styles basic partial-completion)))
        orderless-component-separator "[ |]"))

(use-package company
  :config
  (setq company-idle-delay                 0.0
        company-minimum-prefix-length      2
        company-tooltip-align-annotations  t
        company-tooltip-annotation-padding 1
        company-tooltip-margin             1
        company-detect-icons-margin        'company-dot-icons-margin
	company-dabbrev-ignore-case nil  ;; Ignoriere Groß-/Kleinschreibung nicht
	company-dabbrev-downcase nil      ;; Erhalte die Originalschreibweise
	company-dabbrev-code-ignore-case nil ;; Auch für Code-Vervollständigungen case-sensitiv
	company-dabbrev-code-everywhere t)    ;; Suche nach Matches überall im Code
  (global-company-mode t))

(use-package imenu-list
  :defer t
  :bind (:map custom-bindings-map
              ("M-g i" . imenu-list-smart-toggle)))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(require 'iso-transl)

(use-package vterm
  :defer t

  :preface
  (let ((last-vterm ""))
    (defun toggle-vterm ()
      (interactive)
      (cond ((string-match-p "^\\vterm<[1-9][0-9]*>$" (buffer-name))
             (goto-non-vterm-buffer))
            ((get-buffer last-vterm) (switch-to-buffer last-vterm))
            (t (vterm (setq last-vterm "vterm<1>")))))

    (defun goto-non-vterm-buffer ()
      (let* ((r "^\\vterm<[1-9][0-9]*>$")
             (vterm-buffer-p (lambda (b) (string-match-p r (buffer-name b))))
             (non-vterms (cl-remove-if vterm-buffer-p (buffer-list))))
        (when non-vterms
          (switch-to-buffer (car non-vterms)))))

	(defun switch-vterm (n)
      (let ((buffer-name (format "vterm<%d>" n)))
        (setq last-vterm buffer-name)
        (cond ((get-buffer buffer-name)
               (switch-to-buffer buffer-name))
              (t (vterm buffer-name)
                 (rename-buffer buffer-name))))))

  :bind (:map custom-bindings-map
              ("C-z" . toggle-vterm)
              ("M-1" . (lambda () (interactive) (switch-vterm 1)))
              ("M-2" . (lambda () (interactive) (switch-vterm 2)))
              ("M-3" . (lambda () (interactive) (switch-vterm 3)))
              ("M-4" . (lambda () (interactive) (switch-vterm 4)))
              ("M-5" . (lambda () (interactive) (switch-vterm 5)))
              ("M-6" . (lambda () (interactive) (switch-vterm 6)))
              ("M-7" . (lambda () (interactive) (switch-vterm 7)))
              ("M-8" . (lambda () (interactive) (switch-vterm 8)))
              ("M-9" . (lambda () (interactive) (switch-vterm 9))))
  :bind (:map vterm-mode-map
			  ("C-c C-c" . (lambda () (interactive) (vterm-send-key (kbd "C-c")))))

  :config
  ;; Don't query about killing vterm buffers, just kill it
  (defun my-vterm-kill-with-no-query (&rest _)
    "Set process query on exit flag to nil for vterm buffer."
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil))

  (advice-add 'vterm :after #'my-vterm-kill-with-no-query))

(use-package which-key
  :config
  (which-key-mode))

(use-package esup
  :defer t
  :config
  (setq esup-depth 0))

;;(set-exec-path-from-shell-PATH)
(setq httpd-root "/var/www")

(use-package org
  :defer t

:config
  ;; Org-capture templates
  (setq org-capture-templates
        '(("t" "todo" entry (file+headline "~/Documents/Org/todo.org"
           "My ToDo List")
           "** TODO %? [/] %^t")
	  ("j" "journal" entry (file+datetree "~/Documents/Org/journal.org")
           "* %?\n%i\n")
          ("n" "notes" entry (file "~/Documents/Org/notes.org")
           "* %? %U \n%i\n")))

:config
(add-to-list 'org-modules 'org-habit)

:config
  ;; Org-mode custom variables
  (setq org-agenda-start-with-log-mode t
        org-agenda-custom-commands
        '(("c" "Custom Agenda"
           ((agenda "" 
                    ((org-agenda-span 14)
                     (org-deadline-warning-days 7)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'category "habits"))))
            (tags-todo "+CATEGORY=\"habits\""
                       ((org-agenda-overriding-header "Habits")))
            (tags-todo "+CATEGORY=\"ToDos\""
                       ((org-agenda-overriding-header "To-Do Items"))))))
        calendar-week-start-day 1
        org-habit-graph-column 60
        org-agenda-start-with-habit t
        org-habit-show-all-today t
        org-habit-preceding-days 12
        org-habit-following-days 12
        org-agenda-span 14
        org-agenda-show-all-dates nil
        org-log-into-drawer "LOGBOOK"
        org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE"))
        org-agenda-include-diary t
        org-agenda-files '("~/Documents/Org/")
        org-adapt-indentation nil
        org-startup-with-inline-images t
        org-startup-folded t)

  (defun my/org-move-done-to-archive ()
  "Move the current entry to a specific headline when marked as DONE, 
  excluding entries with the property :STYLE: set to 'habit'."
  (when (and (string-equal org-state "DONE")  ; Prüft, ob der Status auf DONE gesetzt wurde
             (not (string-equal (org-entry-get nil "STYLE") "habit"))) ; Überprüft, ob das Property :STYLE: nicht 'habit' ist
    (let ((level (org-current-level))) ; Speichert die aktuelle Ebene der Überschrift
      (org-cut-subtree) ; Schneidet den aktuellen Eintrag aus
      (save-excursion
        (goto-char (point-min)) ; Gehe zum Anfang des Buffers
        (search-forward-regexp "^\\*+ Archive") ; Suche nach der Überschrift "Archive"
        (org-end-of-subtree t) ; Gehe zum Ende dieser Überschrift
        (newline-and-indent) ; Füge eine neue Zeile ein
        (org-paste-subtree level))))) ; Füge den Eintrag mit der korrekten Ebene ein

  (add-hook 'org-after-todo-state-change-hook #'my/org-move-done-to-archive)

:config
  ;; Keybindings for opening org files
  (defun open-journal ()
    (interactive)
    (find-file "~/Documents/Org/journal.org"))
  (global-set-key (kbd "C-c J") #'open-journal)

  (defun open-todo ()
    (interactive)
    (if (not (get-file-buffer "todo.org"))
        (find-file "~/Documents/Org/todo.org")
      (switch-to-buffer "todo.org")))
  (global-set-key (kbd "C-c T") #'open-todo)

  (defun open-notes ()
    (interactive)
    (find-file "~/Documents/Org/notes.org"))
  (global-set-key (kbd "C-c N") #'open-notes)

  ;; Additional Org-mode keybindings
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)

:hook (org-mode . olivetti-mode)

:config 
(setq visual-fill-column-width 100
      visual-fill-column-center-text t)

(unless (package-installed-p 'visual-fill-column)
  (package-install 'visual-fill-column))

:config
;; Resize Org headings
(custom-set-faces
'(org-document-title ((t (:height 1.6))))
'(outline-1          ((t (:height 1.25))))
'(outline-2          ((t (:height 1.2))))
'(outline-3          ((t (:height 1.15))))
'(outline-4          ((t (:height 1.1))))
'(outline-5          ((t (:height 1.1))))
'(outline-6          ((t (:height 1.1))))
'(outline-8          ((t (:height 1.1))))
'(outline-9          ((t (:height 1.1)))))

(setq org-startup-folded 'overview)

(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-ellipsis "  ·")

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

(setq org-log-done                       t
      org-auto-align-tags                t
      org-tags-column                    -80
      org-fold-catch-invisible-edits     'show-and-error
      org-special-ctrl-a/e               t
      org-insert-heading-respect-content t)

)

(use-package org-appear
  :commands (org-appear-mode)
  :hook     (org-mode . org-appear-mode)
  :config 
  (setq org-hide-emphasis-markers t)  ;; Must be activated for org-appear to work
  (setq org-appear-autoemphasis   t   ;; Show bold, italics, verbatim, etc.
        org-appear-autolinks      t   ;; Show links
        org-appear-autosubmarkers t)) ;; Show sub- and superscripts

(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Configure htmlize to be loaded only when org-mode is started
(use-package htmlize
  :ensure t
  :defer t  ;; Do not load htmlize immediately
  :hook (org-mode . (lambda () (require 'htmlize))))

(use-package org-superstar
  :after org
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-headline-bullets-list '("►" "◉" "●" "⚬" "●" "•"))
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO"  . 9744)
                                          ("PROG"  . 9744)
                                          ("NEXT"  . 9744)
                                          ("WAIT"  . 9744)
                                          ("DONE"  . 9745)))
  :hook (org-mode . org-superstar-mode))

(add-to-list 'font-lock-extra-managed-props 'display)
(font-lock-add-keywords 'org-mode
                        `(("^.*?\\( \\)\\(:[[:alnum:]_@#%:]+:\\)$"
                           (1 `(face nil
                                     display (space :align-to (- right ,(org-string-width (match-string 2)) 3)))
                              prepend))) t)

(add-hook 'org-mode-hook #'(lambda () (electric-indent-local-mode -1)))

(setq org-return-follows-link t)

(setq org-blank-before-new-entry '((heading . nil)
                                   (plain-list-item . nil)))

(setq org-export-use-babel       nil
      org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python     . t)
   (lisp . t)
   ))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("cc" . "src c"))
(add-to-list 'org-structure-template-alist '("cl" . "src lisp"))

(use-package ob-python
  :ensure nil
  :after (ob python)
  :config
  (setq org-babel-python-command python-shell-interpreter))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/Org/RoamNotes")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "* Overview\n* Content\n* Footer\n** Links\n%?* Deutsch :noexport:\n** Zusammenfassung\n** Ausführlich\n** Quellen\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+hugo_lastmod: %U\n#+filetags: %^{Tags}\n#+hugo_section: %^{Hugo Section|🌰 Seeds|🌱
Sprouts|🌿 Plants}\n#+hugo_tags: noexport\n")
      :unnarrowed t    
      :use-old-frame t)
	 ("c" "code" plain
      "* Overview\n* Code\n* Footer\n** Links\n%?* Deutsch :noexport:\n** Zusammenfassung\n** Ausführlich\n** Quellen\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+hugo_lastmod:
%U\n#+filetags: %^{Tags}\n#+hugo_section: %^{Hugo Section|🌰 Seeds|🌱
Sprouts|🌿 Plants}\n#+hugo_tags: noexport\n")
      :unnarrowed t    
      :use-old-frame t)
     ("t" "text-review" plain
      "* Links\n%?,* Quelle\nAutor:in: %^{Autor:in}\n
       Titel: ${title}\n
       Erschienen: %^{Jahr}\n
       * Zusammenfassung\n
       * Ausführlich\n
       * Zitate\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+filetags: %^{Tags}\n")
      :unnarrowed t)
     ("p" "project" plain
      "* Links\n%?\n
       * Ziele\n* TODO [/]\n
       * Termine\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+filetags: %^{Tags}\n")
      :unnarrowed t)))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i" . completion-at-point))
  :config
  (org-roam-db-autosync-mode)
  (org-roam-setup)
)

(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %? %U"
                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(global-set-key (kbd "C-c n b") #'my/org-roam-capture-inbox)

(setq org-roam-node-display-template
      (concat "${title:*} "
        (propertize "${tags:10}" 'face 'org-tag)))

(use-package websocket
  :ensure t
  :after org-roam)
(use-package org-roam-ui
  :ensure t
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :hook (org-roam-ui-mode . httpd-start)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
(global-set-key (kbd "C-c n u") (lambda () (interactive)
				  (org-roam-ui-mode) (httpd-start)))

(use-package ox-hugo
  :after org)

(setq time-stamp-active t
      time-stamp-start "#\\+hugo_lastmod:[ \t]*"
      time-stamp-end "$"
      time-stamp-format "\[%Y-%m-%d %a %H:%M\]")
(add-hook 'before-save-hook 'time-stamp)

(setq org-clock-sound "/Users/philippneumann/Documents/Org/Anhang/ClockSound.wav")
(defun my/play-sound (orgin-fn sound)
  (cl-destructuring-bind (_ _ file) sound
    (make-process :name (concat "play-sound-" file)
                  :connection-type 'pipe
                  :command `("afplay" ,file))))
(advice-add 'play-sound :around 'my/play-sound)

(use-package eldoc
  :defer t
  :config
  (global-eldoc-mode))

(electric-pair-mode 1)

(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(use-package lsp-mode
  :defer t
  :init (setq lsp-use-plists t)
  :hook ((c-mode                . lsp)
		 (python-mode           . lsp)
		 (clojure-mode          . lsp)
         (clojurec-mode         . lsp)
         (lsp-mode              . lsp-enable-which-key-integration)
         (typescript-mode       . lsp)
         (typescript-ts-mode    . lsp)
         (web-mode              . lsp))
  :bind (:map lsp-mode-map
              ("M-<return>" . lsp-execute-code-action)
              ("C-M-."      . lsp-find-references)
              ("C-c r"      . lsp-rename))
  :config
  (setq lsp-diagnostics-provider :flycheck)
  ;; Disable visual features
  (setq lsp-lens-enable                  nil  ;; No lenses

        ;; Enable code actions in the mode line
        lsp-modeline-code-actions-enable t
        lsp-modeline-code-action-fallback-icon "✦"

        ;; Limit raising of the echo area to show docs
        lsp-signature-doc-lines 3)
  (setq lsp-file-watch-threshold  1500)
  (setq lsp-format-buffer-on-save nil)

  (with-eval-after-load 'lsp-modeline
    (set-face-attribute 'lsp-modeline-code-actions-preferred-face nil
                        :inherit font-lock-comment-face)
    (set-face-attribute 'lsp-modeline-code-actions-face nil
                        :inherit font-lock-comment-face)))

(use-package lsp-ui
  :commands lsp-ui-mode  ;; Define the command for enabling the LSP UI mode
  :hook
  (python-mode . lsp-deferred)  ;; Start LSP in Python mode, but with a deferred startup (can be more responsive)
  :config
  (setq lsp-ui-doc-enable t  ;; Enable inline documentation popups
        lsp-ui-doc-delay 0.1  ;; Set a short delay (0.1 seconds) before showing documentation after cursor movement
        lsp-ui-doc-position 'at-point))  ;; Position the documentation popup at the point (the cursor location)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(defun pvn/compile-c-file ()
  "compiles the current c file with gcc"
  (interactive)
  (if (buffer-file-name)
      (let* ((file-path (buffer-file-name))
             (file-without-extension (file-name-sans-extension (file-name-nondirectory file-path)))
             (command (format "gcc -Wall -Wextra -o %s %s"
							  file-without-extension file-path)))

        (compile command))
    (message "This buffer has no proper C-file")))

(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x c") 'pvn/compile-c-file)))

(defun pvn/run-compiled-buffer-file ()
  "Runs the compiled version of the current C file."
  (interactive)
  (let* ((file-without-extension (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
         (exec-file (concat default-directory file-without-extension))
         (vterm-buffer-name "*vterm*"))
    (if (file-exists-p exec-file)
        (progn 
          (if (get-buffer vterm-buffer-name)
              (progn 
                (switch-to-buffer vterm-buffer-name)
                (vterm-send-string (concat "cd " (shell-quote-argument default-directory)))
                (vterm-send-return))
            (vterm))
          (vterm-send-string (concat "./" file-without-extension))
          (vterm-send-return))
      (message "Error: File %s does not exist!" exec-file))))

(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x r") 'pvn/run-compiled-buffer-file)))

(use-package python-mode
  :ensure nil  ;; We don't need to install 'python-mode' since it's built-in
  :hook
  (python-mode . lsp-deferred)  ;; Activate LSP deferred for Python files when entering python-mode
  :custom
  (python-shell-interpreter "python3"))  ;; Set the Python interpreter to 'python3' for the shell

(use-package pyvenv
  :config
  (pyvenv-mode 1))  ;; Enable pyvenv-mode to automatically handle virtual environments

(use-package lsp-pyright
  :custom
  (lsp-pyright-langserver-command "pyright")  ;; Set the command for the Pyright language server (can be 'pyright' or 'basedpyright')
  :hook
  (python-mode . (lambda ()  ;; When entering 'python-mode', activate LSP
                   (require 'lsp-pyright)  ;; Ensure 'lsp-pyright' is loaded
                   (lsp))))  ;; Start LSP for Python files, or use lsp-deferred for deferred startup

(setq inferior-lisp-program "/usr/local/bin/sbcl")

(use-package sly
  :defer t
  :hook
  (sly-mrepl-mode . (lambda () (display-line-numbers-mode 0))))

(eval-after-load 'sly
  `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

(setq paren-dont-touch-blink t)

(require 'mic-paren)
(paren-activate)

(setq paren-highlight-offscreen t)

(setq paren-match-face 'bold)

(setq my/face-background-color (face-background 'region))
(set-face-background 'show-paren-match my/face-background-color)

(setq paren-sexp-mode t)

(use-package ansi-color
  :ensure t
  )
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
					;(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(use-package csound-mode
  :ensure nil
  :custom
  (csound-skeleton-default-sr 96000)
  (csound-skeleton-default-ksmps 16)
  (csound-skeleton-default-options "-d -odac -W -3")
  (csound-skeleton-default-additional-header "#include \"/Users/philippneumann/Documents/Code/Csound/UDOs/udo-collection.udo\"")
  :mode (("\\.csd\\'" . csound-mode)
         ("\\.orc\\'" . csound-mode)
         ("\\.sco\\'" . csound-mode)
         ("\\.udo\\'" . csound-mode))
  :load-path "~/.emacs.d/elpa/csound-mode/"
  :config
  (setq compilation-scroll-output t))

(define-key key-translation-map (kbd "M-1") (kbd "@"))
(define-key key-translation-map (kbd "M-2") (kbd "~"))
(define-key key-translation-map (kbd "M-3") (kbd "#"))
(define-key key-translation-map (kbd "M-5") (kbd "%"))
(define-key key-translation-map (kbd "M-7") (kbd "\\"))
(define-key key-translation-map (kbd "C-M-7") (kbd "|"))
(define-key key-translation-map (kbd "M-5") (kbd "["))
(define-key key-translation-map (kbd "M-6") (kbd "]"))
(define-key key-translation-map (kbd "M-8") (kbd "{"))
(define-key key-translation-map (kbd "M-9") (kbd "}"))

(define-minor-mode custom-bindings-mode
  "A mode that activates custom keybindings."
  :init-value t
  :keymap custom-bindings-map)
