;; -*- lexical-binding: t; -*-

;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;; in Emacs and init.el will be generated automatically!
 (defvar efs/default-font-size 150)
  (defvar efs/default-variable-font-size 150)

;; Make frame transparency overridable
;;(defvar efs/frame-transparency '(10 . 10))

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

(use-package no-littering
  :ensure t
  :init
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  :config
  (no-littering-theme-backups))

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
;;(setq auto-save-file-name-transforms
;;      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


;; Set backups to go to the temporary file directory
;;(setq backup-directory-alist
;;      `((".*" . ,temporary-file-directory)))
;;(setq auto-save-file-name-transforms
;;      `((".*" ,temporary-file-directory t)))

;; Auto delete backups older than a week

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
;;(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
;;(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Disable line numbers for some modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
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

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-j") 'promptless-projectile-test-project)

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

(set-frame-parameter nil 'alpha-background 100)


(defvar previous-alpha-background 90)

(defun toggle-window-transparency ()
  "Toggle transparency between 100% and the previous level."
  (interactive)
  (if (eq (frame-parameter nil 'alpha-background) 100)
      (set-frame-parameter nil 'alpha-background previous-alpha-background)
    (progn
      (setq previous-alpha-background (frame-parameter nil 'alpha-background))
      (set-frame-parameter nil 'alpha-background 100))))

;; (defun toggle-window-transparency ()
;;   "Toggle transparency."
;;   (interactive)
;;   (let ((alpha-transparency 100))
;;     (pcase (frame-parameter nil 'alpha-background)
;;       (alpha-transparency (set-frame-parameter nil 'alpha-background 100))
;;       (t (set-frame-parameter nil 'alpha-background alpha-transparency)))))

(global-set-key (kbd "C-c t") 'toggle-window-transparency)

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

;; Function to filter out a specicific tag from org agenda
(defun my/org-agenda-skip-tag (tag &optional others)
  "Skip all entries that correspond to TAG.

If OTHERS is true, skip all entries that do not correspond to TAG."
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading)))))
    (if others
        (if (not (member tag (org-get-tags-at current-headline)))
            next-headline
          nil)
      (if (member tag (org-get-tags-at current-headline))
          next-headline
        nil))))

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
  ;; This was removed as org-agenda-files are now set using org-roam
  ;; (setq org-agenda-files
  ;;       '("~/org/agenda/agenda.org"
  ;;         "~/org/agenda/habits.org"
  ;;        "~/org/agenda/birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("~/org/agenda/agenda.archive.org" :maxlevel . 1)
      ("~/org/agenda/agenda.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("class" . ?c)
       ("research" . ?r)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))

      (todo "TODO" ((org-agenda-overriding-header "TODO Tasks")
                    (org-agenda-skip-function
                     '(my/org-agenda-skip-tag "habit" nil))
                    ))
      (todo "ACTIVE" ((org-agenda-overriding-header "Active Projects")))
      ))

    ("r" "Thesis Tasks" tags-todo "+thesis")

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
      ("tt" "Task" entry (file+olp "~/org/agenda/agenda.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/org/agenda/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/org/agenda/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/org/agenda/journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Pullups" table-line (file+headline "~/org/agenda/metrics.org" "Pullups")
       "| %U | %^{Pullups} | %^{Notes} |" :kill-buffer t)

      ))

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
     (python .t)
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

  (tempo-define-template "latex equation inline"
                     '(" \\begin{equation} \n    " r "\n \\end{equation}" >)
                      "<eq"
                      "Insert a latex equation block")

  (tempo-define-template "python-block-with-latex"
                         '("#+ATTR_LATEX: :options frame=single\n#+BEGIN_SRC python :results output :exports both :session Python-Session \n    " r "\n#+END_SRC" >)
                         "<p"
                         "Insert an python code block with prefences")
  (tempo-define-template "python-plot-block-with-latex"
                         '("#+ATTR_LATEX: :options frame=single\n#+BEGIN_SRC python :results graphics file :file FILENAME :exports both :session Python-Session \n    " r "\n#+END_SRC" >)
                         "<pp"
                         "Insert an python code block with prefences")

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
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

   )

;; Define an ieeetran-class varaible
(setq ieeetran-class
    '("IEEEtran" "\\documentclass[11pt]{IEEEtran}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; Add it to the org-latex-classes
(with-eval-after-load 'ox-latex (add-to-list 'org-latex-classes ieeetran-class t))

(setq asmeconf-class
      '("asmeconf" "\\documentclass[]{asmeconf}"))

(with-eval-after-load 'ox-latex (add-to-list 'org-latex-classes asmeconf-class t))

(setq sb3c-class
      '("sb3c"
        "\\documentclass{sb3c}                  
              [NO-DEFAULT-PACKAGES]
              [PACKAGES]
              [EXTRA]"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
        )
      )

(with-eval-after-load 'ox-latex (add-to-list 'org-latex-classes sb3c-class t))

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
(setq lsp-keymap-prefix "C-c l") ; Or 'C-l', 's-l'
:config
(lsp-enable-which-key-integration t)
(lsp-register-custom-settings
 '(("pyls.plugins.pycodestyle.enabled" nil)   ; Disable pycodestyle
   ("pyls.plugins.pyflakes.enabled" nil)     ; Disable pyflakes
   ("pyls.plugins.mccabe.enabled" nil)       ; Disable mccabe
   ("pyls.plugins.flake8.enabled" t)         ; Enable Flake8
   ("pyls.plugins.flake8.maxLineLength" 79))) ; Set max line length for Flake8
)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :after lsp)

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (require 'dap-chrome)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed
  (require 'dap-python)

  (setq dap-python-debugger 'debugpy)
  )

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  ;; (general-define-key
  ;;   :keymaps 'lsp-mode-map
  ;;   :prefix lsp-keymap-prefix
  ;;   "d" '(dap-hydra t :wk "debugger")))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred)
  :config
  (setq js-indent-level 2))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3"))

;; (use-package pyvenv
  ;;   :after python-mode
  ;;   :config
  ;;   (pyvenv-mode 1))


(use-package virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place
(setq venv-location "~/.virtualenvs")

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

;; with use-package
(use-package numpydoc
  :ensure t
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))

(setq ess-r-backend 'lsp)
;;(add-hook 'ess-r-mode-hook (lambda () (lsp)))

(use-package ess-r-mode
    :ensure nil
    :hook (ess-r-mode . lsp-deferred))

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


(with-eval-after-load 'projectile
  ;; Override Python Pip project test command
  (projectile-update-project-type
   'python-pip :test "pytest")

  ;; Override Python Setup (pkg) project test command
  (projectile-update-project-type
   'python-pkg :test "pytest")

  ;; Override Python Pipenv project test command
  (projectile-update-project-type
   'python-pipenv :test "pipenv run pytest")

  ;; Override Generic Python project test command
  (projectile-update-project-type
   'python-toml :test "pytest")
  )

(defun promptless-projectile-test-project (&optional prompt)
(interactive "P")
(let ((compilation-read-command
       nil
       ))
  (projectile-test-project prompt)))

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

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  ;;(evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  ;;(evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  ;;(evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(defun rshell (&optional host)
  "Open a remote shell to a host."
  (interactive)
  (with-temp-buffer
    (let ((host (or host (read-string "Host: "))))
      (cd (concat "/scp:" host ":"))
      (shell (concat "*" host "*")))))


(defun rshell-ubuntuResearch18 () (interactive) (rshell "bkm82@192.168.0.37"))
(defun rshell-ubuntuResearch () (interactive) (rshell "bkm82@192.168.0.26"))
(defun rshell-nodeone () (interactive) (rshell "bkm82@nodeone.braymoll.com"))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; Make gc pauses faster by decreasing the threshold.
(use-package citeproc)

(require `ox-md)

(use-package ox-reveal)
(require 'ox-reveal)

(setq Org-Reveal-root "~/slides/reveal.js/js/reveal.js")
(setq Org-Reveal-title-slide nil)

(use-package ssh-agency)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1)
  )

(exec-path-from-shell-initialize)

(gptel-make-privategpt "local_Llama-3.2-3B"
:protocol "http"
:host "192.168.0.8:8001"
:stream t
:context t
:sources nil                        ; Set sources to false
:models '(private-gpt))

(setq gptel-default-mode 'org-mode)

(use-package org-ref)

(setq reftex-default-bibliography '("~/masters/bibliography/references.bib"))

(setq bibtex-completion-bibliography '("~/masters/bibliography/references.bib"
  "~/masters/bibliography/archive.bib")
      bibtex-completion-library-path '("~/masters/bibliography/bibtex-pdfs/")
        bibtex-completion-notes-path "~/masters/bibliography/notes/"
        bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
        bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "open" nil 0 nil fpath)))

(require 'bibtex)

(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)

(require 'org-ref)
(require 'org-ref-ivy)
(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
(setq org-latex-pdf-process
    '("pdflatex -interaction nonstopmode -output-directory %o %f"
      "bibtex %b"
      "pdflatex -interaction nonstopmode -output-directory %o %f"
      "pdflatex -interaction nonstopmode -output-directory %o %f"))

(use-package org-roam
  :ensure t
  :demand t
  :custom
  (org-roam-directory "~/roamnotes")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(
     ("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("p" "project" plain "* Description\n\n** Status\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: :agenda:Project:")
      :unnarrowed t)

     ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n p" . my/org-roam-find-project)
         ("C-c n t" . my/org-roam-capture-task)
         ("C-c n b" . my/org-roam-capture-inbox)
         ("C-c n m" . my/org-roam-capture-metrics)
         ;; :map org-mode-map
         ;; ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))


(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates
         (list
          (append
           (car org-roam-capture-templates)
           '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

 (defun my/org-roam-filter-by-tag (tag-name)
   (lambda (node)
     (member tag-name (org-roam-node-tags node))))

 (defun my/org-roam-list-notes-by-tag (tag-name)
   (mapcar #'org-roam-node-file
           (seq-filter
            (my/org-roam-filter-by-tag tag-name)
            (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "agenda")))

;; Build the agenda list the first time for the session
(my/org-roam-refresh-agenda-list)

(defun my/org-toggle-first-todo-done ()
  "Toggle the first TODO in the buffer to DONE if capture was confirmed."
  (remove-hook 'org-capture-after-finalize-hook #'my/org-toggle-first-todo-done)
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "\\* TODO" nil t)
          (org-todo 'done))))))

(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))



(defun my/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")))


  (defun my/org-roam-capture-metrics ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  (add-hook 'org-capture-after-finalize-hook #'my/org-toggle-first-todo-done)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "metrics"))
                     :templates '(("t" "table" table-line "|%U | %^{${title}} | %^{Notes} |"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: :agenda:metrics:habit:\n\n* Completion Record \n\n** TODO ${title}\nSCHEDULED: %(org-insert-time-stamp nil nil nil nil nil \" .+1d\")\n:PROPERTIES:\n:STYLE:    habit\n:END: \n\n* Table\n| Time Stamp | ${title}    | Notes     |\n"("Table"))))))

  (defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                  :if-new (file+head+olp "inbox.org" "#+title: Inbox\n#+category: Inbox\n#+filetags: :agenda" ("Tasks"))))))

(defun my/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "agenda"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: :agenda:Project:"
                                                          ("Tasks"))))))
;; (defun my/org-roam-copy-todo-to-today ()
;;   (interactive)
;;   (let ((org-refile-keep t) ;; Set this to nil to delete the original!
;;         (org-roam-dailies-capture-templates
;;           '(("t" "tasks" entry "%?"
;;              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
;;         (org-after-refile-insert-hook #'save-buffer)
;;         today-file
;;         pos)
;;     (save-window-excursion
;;       (org-roam-dailies--capture (current-time) t)
;;       (setq today-file (buffer-file-name))
;;       (setq pos (point)))

;;     ;; Only refile if the target file is different than the current file
;;     (unless (equal (file-truename today-file)
;;                    (file-truename (buffer-file-name)))
;;       (org-refile nil nil (list "Tasks" today-file nil pos)))))

;; (add-to-list 'org-after-todo-state-change-hook
;;              (lambda ()
;;                (when (equal org-state "DONE")
;;                  (my/org-roam-copy-todo-to-today))))

(use-package websocket
    :after org-roam)

(use-package org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (
         ("C-M-S-Y" . org-download-screenshot)
         )
  )
  :config
  (defun my-org-download-set-dir ()
    "Set `org-download-image-dir` to the directory of the current 
        buffer's file."
    (when (buffer-file-name)
    (setq-local org-download-image-dir (concat (file-name-directory 
                                                (buffer-file-name)) "images/" (file-name-base buffer-file-name) "/") )))
  :hook
  ((org-mode . my-org-download-set-dir))
  :custom
  (org-download-screenshot-method "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")

  )

(use-package org-fragtog
  :after org
  )
(add-hook 'org-mode-hook 'org-fragtog-mode)
