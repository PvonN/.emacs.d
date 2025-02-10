;;; Garbage Collector
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
;;; Basic UI Configuration
(setq inhibit-startup-message t)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;;; emacs - org-mode styling
;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)
;; Set reusable font name variables
(defvar my/fixed-width-font "Inconsolata"
  "The font to use for monospaced (fixed width) text.")

(defvar my/variable-width-font "Open Sans"
  "The font to use for variable-pitch (document) text.")

;; NOTE: These settings might not be ideal for your machine, tweak them as needed!
(set-face-attribute 'default nil :font my/fixed-width-font :weight 'light :height 160)
(set-face-attribute 'fixed-pitch nil :font my/fixed-width-font :weight 'light :height 190)
(set-face-attribute 'variable-pitch nil :font my/variable-width-font :weight 'medium :height 1.3)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.35)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font my/variable-width-font :weight 'bold :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font my/variable-width-font :weight 'bold :height 1.8)

(require 'org-indent)
;; Make sure certain org faces use the fixed-pitch face when
;; variable-pitch-mode is on
(set-face-attribute 'org-block nil            :foreground nil :inherit 'fixed-pitch :height 0.85)
(set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch) :height 0.85)
(set-face-attribute 'org-indent nil           :inherit '(org-hide fixed-pitch) :height 0.85)
(set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch) :height 0.85)
(set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil         :inherit 'fixed-pitch)

(add-hook 'org-mode-hook 'variable-pitch-mode)
(plist-put org-format-latex-options :scale 2)


;; Hide emphasis markers on formatted text
(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis "  ·")

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

(add-hook 'org-mode-hook
          (lambda ()
            (visual-fill-column-mode 1)
            (visual-line-mode 1)))

(use-package olivetti
  :defer t
  :config
  (setq olivetti-style t))

(add-hook 'org-mode-hook 'olivetti-mode)


;;; Languages
;;;; LSP-MOde
(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
	 (c-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook
  (python-mode . lsp-deferred)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.1
        lsp-ui-doc-position 'at-point))
;;;; C
;; ;; use c++-mode for better syntax highlighting
;; ;; Verknüpfe .c-Dateien mit c++-mode anstatt c-mode
;; (use-package cc-mode
;;   :ensure nil
;;   :mode ("\\.c\\'" . c-mode)
;;   :bind (:map c-mode-base-map
;;               ("C-c C-c" . compile))
;;   :config
;;   (setq compilation-read-command t))


;;;; Python 
;; outdated
;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable))

(use-package python-mode
  :ensure nil
  :hook
  (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3"))


(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;;;; Lisp 
;;;;; SBCL - Steal Bank Common Lisp
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/usr/local/bin/sbcl")
;;;;; SLY
(use-package sly
  :defer t
  :hook
  (sly-mrepl-mode . (lambda () (display-line-numbers-mode 0))))

(eval-after-load 'sly
  `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))
;;;;; show parenthesis - mic.pare
(setq paren-dont-touch-blink t)
(require 'mic-paren)
(paren-activate)
(setq paren-highlight-offscreen t)
(setq paren-match-face 'bold)

(setq my/face-background-color (face-background 'region))
(set-face-background 'show-paren-match my/face-background-color)

;; (setq my/face-foreground-color (face-foreground 'org-cite))
;; (set-face-foreground 'show-paren-match my/face-foreground-color)


(setq paren-sexp-mode t)

;;; Electric Pair Mode
(electric-pair-mode 1)
;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs
      '(
        (?\" . ?\")
	(?\[ . ?\])
        (?\{ . ?\})))
;;; Emacs Backend 
;;;; use-package - Melpa - GNU Packages
(require 'use-package)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-refresh-contents)
(package-initialize)
;;;; emacs custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;;; backup emacs files
(setq backup-directory-alist `(("." . "~/.emacs.d/.backupfiles")))
;;; Emacs Styling 
;;;; Doom Emacs Themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dark+ t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
;  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (set-face-background 'font-lock-comment-face nil)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;;;; Doom Modeline

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode)
;;   :load-path "~/.emacs.d/elpa/doom-modeline")
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))
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
  ;; needs display-time-mode to be one
  (doom-modeline-time t)
  (doom-modeline-vcs-max-length 50))

;;;; Spacious Padding
(use-package spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode))
;;;; solair - distinguish buffer from files
(defun solaire-mode-real-buffer-custom-p ()
  "Return t if the current buffer is the dashboard or scratch, or is a real (file-visiting) buffer."
  (cond ((string= (buffer-name (buffer-base-buffer)) "*dashboard*") t)        
        ((buffer-file-name (buffer-base-buffer)) t)
        (t nil)))

(use-package solaire-mode
  :ensure t
  :hook (after-init . solaire-global-mode)
  :init (setq solaire-mode-real-buffer-fn #'solaire-mode-real-buffer-custom-p))
;;;; golden-ratio - set sizes of inactive and active buffer
(use-package golden-ratio
  :ensure t
  :hook (after-init . golden-ratio-mode))
;;; Projectile - Project Managing
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
;;; Magit
(use-package magit
  :ensure t)

;;; Mini-Buffer Customization 
;;;; vertico - autocompletion for mini-buffer
(use-package vertico
  :ensure t
  :init 
  (vertico-mode)
  :custom
  (setq vertico-cycle t))

;;;; savehist for remember which files used last in minibuffer
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;;;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;;;; posframe for popping up a window in position
(use-package posframe
  :ensure t
  )
;;;; vertico-posframe for popping up a extra mini-buffer window
(use-package vertico-posframe
  :ensure t
  :custom
  (vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  (vertico-posframe-mode 1)
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center))


;;; outline-minor-mode - for cycling headlines in non-org files
(use-package outline-minor-faces
  :ensure t
  :after outline
  :hook
  (outline-minor-mode . outline-minor-faces-mode)
  :config
  (custom-set-faces
   '(outline-minor-0
     ((((class color) (background light)) (:background unspecified :weight bold :extend t))
      (((class color) (background dark)) (:background unspecified :weight bold :extend t))))))

(use-package outline
  :hook
  (emacs-lisp-mode . outline-minor-mode)
  (sly-mode . outline-minor-mode)
  (elpy-mode . outline-minor-mode)
  :config
  (setq outline-minor-mode-highlight nil) 
  (setq outline-minor-mode-cycle t) 
  (setq outline-minor-mode-use-buttons nil) 
  (setq outline-minor-mode-use-margins t))

;; (use-package bicycle
;;   :ensure t)
;; (defun my-c-mode-outline-setup ()
;;   "Configure `outline-minor-mode` for C/C++ mode to recognize `///`, `////`, and `/////` as outline headings."
;;   (setq-local outline-regexp "\\(///\\{3,5\\}\\)"))


;; (add-hook 'c-mode-hook 'my-c-mode-outline-setup)
;; (add-hook 'c++-mode-hook 'my-c-mode-outline-setup)

;; (add-hook 'c-mode-hook 'outline-minor-mode)
;; (add-hook 'c++-mode-hook 'outline-minor-mode)

;;; quickrun for quickt execution of the editing buffer
(use-package quickrun
  :ensure t)

;;; company - auto completion mode
(add-hook 'after-init-hook 'global-company-mode)
;; Setze Case-Sensitivity für company-dabbrev
(setq company-dabbrev-ignore-case nil)   ;; Ignoriere Groß-/Kleinschreibung nicht
(setq company-dabbrev-downcase nil)      ;; Erhalte die Originalschreibweise

;; Optional: Weitere Anpassungen für company-mode
(setq company-dabbrev-code-ignore-case nil) ;; Auch für Code-Vervollständigungen case-sensitiv
(setq company-dabbrev-code-everywhere t)    ;; Suche nach Matches überall im Code

;;; Line Number
(global-display-line-numbers-mode 1)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))
;;; Keybindings
;TILDE: https://www.emacswiki.org/emacs/DeadKeys
;C-x 8 RET tilde RET
(require 'iso-transl)

; https://emacs.stackexchange.com/questions/16749/how-to-set-emacs-to-recognize-backtick-and-tilde-with-a-colemak-keyboard-layout
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

;;; Auto-Fill-Mode
(setq-default auto-fill-function 'do-auto-fill)
(setq fill-column 80)

;;; Set-Clock Sound for Timer in emacs
(setq org-clock-sound "/Users/philippneumann/Documents/Org/Anhang/ClockSound.wav")
(defun my/play-sound (orgin-fn sound)
  (cl-destructuring-bind (_ _ file) sound
    (make-process :name (concat "play-sound-" file)
                  :connection-type 'pipe
                  :command `("afplay" ,file))))
(advice-add 'play-sound :around 'my/play-sound)
;;; Dashboard
;; use-package with package.el:
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-startup-banner 'logo)
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
;;; Org-Settings
;; Org Mode Configuration

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  ;; Additional Org-mode settings
  (add-to-list 'org-modules 'org-habit)

  ;; Org-capture templates
  (setq org-capture-templates
        '(("t" "todo" entry (file+headline "~/Documents/Org/todo.org"
           "My ToDo List")
           "** TODO %? [/] %^t")
	  ("j" "journal" entry (file+datetree "~/Documents/Org/journal.org")
           "* %?\n%i\n")
          ("n" "notes" entry (file "~/Documents/Org/notes.org")
           "* %? %U \n%i\n")))

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
  (global-set-key (kbd "C-c c") #'org-capture))
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("🔴" "🟠" "🟢")))
;;;; Org-Modern
(use-package org-modern
  :ensure t
  :config
  (setq
   org-auto-align-tags t
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Don't style the following
   org-modern-tag nil
   org-modern-priority nil
   org-modern-todo nil
   org-modern-table nil

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
	 (800 1000 1200 1400 1600 1800 2000)
	 " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  (global-org-modern-mode))
;;;; org-superstart
(use-package org-superstar
  :ensure t
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("DONE" . 9744)
                                          ("READ" . 9744)
                                          ("IDEA" . 9744)
                                          ("WAITING" . 9744)
                                          ("CANCELLED" . 9744)
                                          ("PROJECT" . 9744)
                                          ("POSTPONED" . 9744)))
  )

;;;; ox-hugo
(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)
;; Update last modified date for ox-hugo export
(setq time-stamp-active t
      time-stamp-start "#\\+hugo_lastmod:[ \t]*"
      time-stamp-end "$"
      time-stamp-format "\[%Y-%m-%d\]")
(add-hook 'before-save-hook 'time-stamp)

;;; Org Mode Appearance ------------------------------------
(setq visual-fill-column-width 80
      visual-fill-column-center-text t)














;;; Centering Org Documents --------------------------------

;; Install visual-fill-column
(unless (package-installed-p 'visual-fill-column)
  (package-install 'visual-fill-column))

;; Configure fill width
(setq visual-fill-column-width 110
      visual-fill-column-center-text t)






;;; htmlize
;; Configure htmlize to be loaded only when org-mode is started
(use-package htmlize
  :ensure t
  :defer t  ;; Do not load htmlize immediately
  :hook (org-mode . (lambda () (require 'htmlize))))
;;; Org-Roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/Org/RoamNotes")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "* Links\n%?* Zusammenfassung\n* Ausführlich\n* Quellen\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+date:
%U\n#+hugo_lastmod: %U\n#+filetags: %^{Tags}\n#+hugo_section: seeds\n#+hugo_tags: noexport\n")
      :unnarrowed t
      :immediate-finish t
      :use-old-frame t)
     ("t" "text-review" plain
      "* Links\n%?
       * Quelle\n
       Autor:in: %^{Autor:in}\n
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
 (org-roam-setup))

(defun my/org-roam-capture-inbox- ()
  (interactive)
  (org-roam-capture :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %? %U"
                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(global-set-key (kbd "C-c n b") #'my/org-roam-capture-inbox)
;;; org-roam-ui
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

;; (use-package org-roam-ui
;;   :straight
;;     (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;     :after org-roam
;; ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;; ;;         a hookable mode anymore, you're advised to pick something yourself
;; ;;         if you don't care about startup time, use
;; ;;  :hook (after-init . org-roam-ui-mode)
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))



;;; Shell Befehle in Emacs ausführen über M-!
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;;; simple-httpd

(set-exec-path-from-shell-PATH)
(setq httpd-root "/var/www")
;;(httpd-start)
;;; Csound Config
;; Keybindings
;; C-c C-p csound-play Same as doing csound filename -odac
;;
;; C-c C-r csound-render Same as doing csound filename -o filename.wav
;;
;; C-c C-z csound-repl-start
;;
;; C-M-x/C-c C-c csound-evaluate-region
;;
;; C-x C-e csound-evaluate-line
;;
;; C-c C-l csound-repl-interaction-evaluate-last-expression
;;
;; C-c C-s csound-score-align-block cursor needs to be within a score block
;;
;; M-. csound-score-find-instr-def cursor needs to be within a score block
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
;;; hub-view
;; open hub.org
(defun hub-view ()
  (interactive)
  (find-file "~/Documents/Org/Hub/hub.org"))
    
(global-set-key (kbd "C-c v h") #'hub-view)  

;;; nerd-icons
(use-package nerd-icons
 :ensure t
   :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  )
;;; Treemacs - dir tree
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

;;; term modes
(use-package term
  :ensure t
  :defer t
  :config
  (setq explicit-shell-file-name "zsh") ;; Change this to bash, etc
  (setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;;; auto-load the HUB.org
(find-file "~/Documents/Org/Hub/hub.org")

;;; line highliting
(global-hl-line-mode -1)
(set-face-background 'highlight nil)
;(set-face-foreground 'highlight t)


;;; vTerm
(use-package vterm
  :ensure t)


;;; Org Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lisp . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)
;;; Garbage Collector
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
