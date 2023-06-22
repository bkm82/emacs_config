;Inibit startup message
(setq inhibit-startup-message t) 


(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(load-theme 'wombat)

; show the line numbers
(global-display-line-numbers-mode 1)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))



;open every file with the cursor location where it was closed
(save-place-mode 1)

;; dont pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when teh underlying file has changed
(global-auto-revert-mode 1)



(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Use the Ivy package for fuzzy finding files etc
(use-package ivy
  :diminish
;;  :bind (("C-s" . swiper)
         ;; :map ivy-minibuffer-map
         ;; ("TAB" . ivy-alt-done)
         ;; ("C-l" . ivy-alt-done)
         ;; ("C-j" . ivy-next-line)
         ;; ("C-k" . ivy-previous-line)
         ;; :map ivy-switch-buffer-map
         ;; ("C-k" . ivy-previous-line)
         ;; ("C-l" . ivy-done)
         ;; ("C-d" . ivy-switch-buffer-kill)
         ;; :map ivy-reverse-i-search-map
         ;; ("C-k" . ivy-previous-line)
         ;; ("C-d" . ivy-reverse-i-search-kill))
  :config (ivy-mode 1))

;; Use doom mode line for a better mode line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes)

;; Put backups into a directory
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; Start org mode with indent mode and numbered mode enabled
(setq org-startup-indented t)
(setq org-startup-numerated t)

;; Create C-c C-m and C-x C-m to run custom commands
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

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

(use-package evil)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/org-mode-playground/EMACS-Commands.org"))
 '(package-selected-packages
   '(evil doom-themes helpful counsel ivy-rich which-key rainbow-delimiters doom-modeline ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
