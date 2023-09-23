;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;; in Emacs and init.el will be generated automatically!
 (defvar efs/default-font-size 150)
  (defvar efs/default-variable-font-size 150)

;; Make frame transparency overridable
(defvar efs/frame-transparency '(10 . 10))

;; e default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;;(setq user-emacs-directory "~/.emacs.d")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

  ;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Disable line numbers for some modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook
                  doc-view-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil  :font "Fira Code Retina" :height efs/default-font-size)


;;
(set-face-attribute 'variable-pitch nil :font "Fira Code Retina" :height efs/default-variable-font-size :weight 'regular)

;; Using arrow for moving through buffers
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; Create C-c C-m and C-x C-m to run custom commands
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)


(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(use-package doom-themes
  :init (load-theme 'wombat))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 55)
           (doom-modeline-vcs-max-lenght 15)
           ))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))


(use-package counsel
:bind (("C-M-j" . 'counsel-switch-buffer)
       ("M-x" . counsel-M-x)
       ("C-x b" . counsel-ibuffer)
       ("C-x C-f" . counsel-find-file)
       :map minibuffer-local-map
       ("C-r" . 'counsel-minibuffer-history))
:config
(setq ivy-initial0inputs-alist nil))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
   :defer t)

 (defhydra hydra-text-scale (:timeout 4)
   "scale text"
   ("j" text-scale-increase "in")
   ("k" text-scale-decrease "out")
   ("f" nil "finished" :exit t))

; (efs/leader-keys
;   "ts" '(hydra-text-scale/body :which-key "scale text"))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fira Code Retina" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;;(setq org-agenda-files
  ;;      '("~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org"
  ;;        "~/Projects/Code/emacs-from-scratch/OrgFiles/Habits.org"
  ;;       "~/Projects/Code/emacs-from-scratch/OrgFiles/Birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
	((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
	((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
	    ((org-agenda-overriding-header "Waiting on External")
	     (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
	    ((org-agenda-overriding-header "In Review")
	     (org-agenda-files org-agenda-files)))
      (todo "PLAN"
	    ((org-agenda-overriding-header "In Planning")
	     (org-agenda-todo-list-sublevels nil)
	     (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
	    ((org-agenda-overriding-header "Project Backlog")
	     (org-agenda-todo-list-sublevels nil)
	     (org-agenda-files org-agenda-files)))
      (todo "READY"
	    ((org-agenda-overriding-header "Ready for Work")
	     (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
	    ((org-agenda-overriding-header "Active Projects")
	     (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
	    ((org-agenda-overriding-header "Completed Projects")
	     (org-agenda-files org-agenda-files)))
      (todo "CANC"
	    ((org-agenda-overriding-header "Cancelled Projects")
	     (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
	   "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
	   (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
	   "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
	   ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
	   :clock-in :clock-resume
	   :empty-lines 1)
      ("jm" "Meeting" entry
	   (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
	   "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
	   :clock-in :clock-resume
	   :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
	   "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (efs/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 80
        visual-fill-column-center-text nil)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (org . t)
     (ditaa . t)
     (latex . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (screen . nil)
     (shell . t)
     (sql . nil)
     (sqlite . t))))


 ; (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (tempo-define-template "r-block-with-latex"
                         '("#+ATTR_LATEX: :options frame=single\n#+BEGIN_SRC R :results output :exports both :session R-session \n    " r "\n#+END_SRC" >)
                         "<r"
                         "Insert an r code block with prefences")
  (tempo-define-template "r-plot-block-with-latex"
                     '("#+ATTR_LATEX: :options frame=single\n#+BEGIN_SRC R :results graphics file :file FILENAME :exports both :session R-session \n    " r "\n#+END_SRC" >)
                      "<rp"
                      "Insert an r code block with prefences")
  )

;; Setup a plain Latex class as shown https://www.reddit.com/r/orgmode/comments/lzsygs/perfect_org_mode_exports_to_latex_easy_extensible/
 (with-eval-after-load 'ox-latex
   (add-to-list 'org-latex-classes
                '("org-plain-latex"
                  "\\documentclass{article}
              [NO-DEFAULT-PACKAGES]
              [PACKAGES]
              [EXTRA]"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

; (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

 (use-package pdf-tools
    :defer t
    :config
        (pdf-tools-install)
        (setq-default pdf-view-display-size 'fit-page)
    :bind (:map pdf-view-mode-map
          ("\\" . hydra-pdftools/body)
          ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
          ("g"  . pdf-view-first-page)
          ("G"  . pdf-view-last-page)
          ("l"  . image-forward-hscroll)
          ("h"  . image-backward-hscroll)
          ("j"  . pdf-view-next-page)
          ("k"  . pdf-view-previous-page)
          ("e"  . pdf-view-goto-page)
          ("u"  . pdf-view-revert-buffer)
          ("al" . pdf-annot-list-annotations)
          ("ad" . pdf-annot-delete)
          ("aa" . pdf-annot-attachment-dired)
          ("am" . pdf-annot-add-markup-annotation)
          ("at" . pdf-annot-add-text-annotation)
          ("y"  . pdf-view-kill-ring-save)
          ("i"  . pdf-misc-display-metadata)
          ("s"  . pdf-occur)
          ("b"  . pdf-view-set-slice-from-bounding-box)
          ("r"  . pdf-view-reset-slice)))

 (setq revert-without-query '(".pdf"))

 (setq org-latex-listings 'minted
       org-latex-packages-alist '(("" "minted"))
       org-latex-pdf-process
       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
		      (expand-file-name user-emacs-directory)) ; can change to if saved in the user-emacs-directory if this file is saved in  ~/snap/bin/emacs/
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
;new comment

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

(setq ess-r-backend 'lsp)
(add-hook 'ess-r-mode-hook (lambda () (lsp)))



(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("R" "--slave" "-e" "languageserver::run()"))
    :major-modes '(ess-mode inferior-ess-r-mode)
    :server-id 'lsp-R)))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
   :diminish projectile-mode
   :config (projectile-mode)
   :custom ((projectile-completion-system 'ivy))
   :bind-keymap
   ("C-c p" . projectile-command-map)
   :init
   ;; NOTE: Set this to the folder where you keep your Git repos!
   (when (file-directory-p "~/masters")
     (setq projectile-project-search-path '(("~/masters" . 3 ))))
   (setq projectile-switch-project-action #'projectile-dired))

; (use-package counsel-projectile
;   :after projectile
;   :config (counsel-projectile-mode))

(use-package magit
   :commands magit-status
   :custom
   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

 ;; NOTE: Make sure to configure a GitHub token before using this package!
 ;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
 ;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
;; (use-package forge
;;   :after magit)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
