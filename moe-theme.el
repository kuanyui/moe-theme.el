;;; moe-theme --- A colorful eye-candy theme. Moe, moe, kyun!

;; This program is not part of GNU Emacs, but it is distributed under GPL v3 :-)
;;
;; This file is established for packaging. If you want to install manually,
;; check README.md
;;
;; Author: kuanyui <azazabc123@gmail.com>
;; Keywords: themes
;; X-URL: https://github.com/kuanyui/moe-theme.el
;; URL: https://github.com/kuanyui/moe-theme.el
;; Version: {{VERSION}}

;;; Commentary:

;; You can take a look at screenshots and acquire more information on:
;;
;;     https://github.com/kuanyui/moe-theme.el
;;
;;
;; = Requirements ==============================================================
;;
;;   - Emacs 24 (or above)
;;   - 256 colors terminal (or higher)
;;
;; = Usage =====================================================================
;;
;;   Add you to your .emacs:
;;
;;      (require 'moe-theme)
;;      (moe-dark)
;;          or
;;      (moe-light)
;;
;;   But if you want to install manually, add this first:
;;
;;      (add-to-list 'custom-theme-load-path "~/path/to/moe-theme")
;;      (add-to-list 'load-path "~/path/to/moe-theme")
;;
;; = Customizations ============================================================
;;
;;    It's impossible to satisfy everyone with one theme, so `moe-theme` provide
;;    some customizations that you may like.
;;
;;    ### Resize Titles ########################################################
;;
;;    You may want to resize the titles in `markdown-mode', `org-mode', or
;;    `rst-mode':
;;
;;      (setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))
;;      (setq moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
;;      (setq moe-theme-resize-rst-title '(2.0 1.7 1.5 1.3 1.1 1.0))
;;
;;    Markdown should have 6 items; org has 9 items; rst has 6 items.
;;
;;    The values should be lists. Larger the values, larger the fonts.
;;    If you don't like this, just leave them nil, and all the titles will be
;;    the same size.
;;
;;    ### Colorful Mode-line and Powerline #####################################
;;
;;    Tired of boring blue mode-line? Set default mode-line color like this:
;;
;;      (setq moe-theme-mode-line-color 'orange)
;;
;;    Available colors: blue, orange, magenta, yellow, purple, red, cyan, w/b.
;;
;;    You can use `moe-theme-select-color' to change color interactively.
;;
;;    Mayby you'll also like `moe-theme-random-color', which gives you a
;;    random mood :D.
;;
;;    ### Powerline ############################################################
;;
;;    Now we supports Powerline (https://github.com/milkypostman/powerline),
;;    which makes mode-line looks fabulous! We recommended installing Powerline
;;    and run `powerline-moe-theme'.
;;
;;    ### Too Yellow Background? ###############################################
;;
;;    With 256-colors, default yellow background of moe-light may be too yellow
;;    and harsh to eyes on some screens.
;;
;;    If you encounter this problem, and want to set background color to #ffffff
;;    in terminal, set the value of `moe-light-pure-white-background-in-terminal'
;;    to t:
;;
;;        (setq moe-light-pure-white-background-in-terminal t)
;;
;;    ### Highlight Buffer-id on Mode-line? ####################################
;;
;;    You may be dislike default highlight on mode-line-buffer-id, now it can be
;;    disable:
;;
;;        (setq moe-theme-highlight-buffer-id nil)
;;
;;
;; = Auto Switching ============================================================
;;
;;   I prefer a terminal with a black-on-white color scheme. I found that in the
;; daytime, sunlight is strong and black-on-white is more readable; However,
;; white-on-black would be less harsh to the eyes at night.

;;   So if you like, you can add the following line to your ~/.emacs to
;; automatically switch between moe-dark and moe-light according to the system
;; time:
;;
;;    (require 'moe-theme-switcher)
;;
;;   By adding the line above, your Emacs will have a light theme in the day
;;  and a dark one at night. =w=+
;;
;;
;; # Live in Antarctica? #######################################################
;;
;;   Daytime is longer in summer but shorter in winter; or you live in a high
;; latitude region which midnight-sun or polar-night may occur such as Finland
;; or Antarctica?
;;
;;   There's a variable `moe-theme-switch-by-sunrise-and-sunset` would solve
;; your problem (default value is `t`)
;;
;;   If this value is `nil`, `moe-theme-switcher` will switch theme at fixed
;; time (06:00 and 18:00).
;;
;;   If this value is `t` and both `calendar-latitude` and `calendar-longitude`
;; are set properly, the switching will be triggered at the sunrise and sunset
;; time of the local calendar.
;;
;;   Take "Keelung, Taiwan" (25N,121E) for example, you can set like this:
;;
;; 	(setq calendar-latitude +25)
;; 	(setq calendar-longitude +121)

;;; Code:

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(defvar moe-theme-highlight-buffer-id t
  "If t, highlight buffer-id on mode-line.
If nil, just bold buffer-id without highlight")

(defvar moe-theme-mode-line-color 'blue
  "Default is 'blue.
If nil, no background color.
Available choices: 'blue, 'green, 'magenta, 'red, 'orange, 'yellow, 'purple, 'b/w")

(defvar moe-light-pure-white-background-in-terminal nil
  "With 256-colors, default yellow background of moe-light may be 'too yellow'
on some screen.

If you encounter this problem, and want to have a background of #ffffff, set
the value of `moe-light-pure-white-background-in-terminal' to t.")

;; The variable `moe-theme-resize-title-fonts`
(defvar moe-theme-resize-markdown-title nil
  "Resize header/title faces of Markdown-mode or not. (default value is nil)
The value should be a list with 6 items of number, which decide title font sizes
of each level.

  (setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))

If the vaule is nil, all the titles will be the same size.")

(defvar moe-theme-resize-org-title nil
  "Resize outline/title faces of Org-mode or not. (default value is nil)

The value should be a list with 9 items of number; which decide title font sizes
of each level. The first item is the font size of `org-document-title', from
second to ninth is outlines of 1-8.
For example, you can set like this:

  (setq moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))

If the vaule is nil, all the outlines will be the same size.")

(defvar moe-theme-resize-rst-title nil
  "Resize title faces of ReStructuredText-mode or not. (default value is nil)
The value should be a list with 6 items of number, which decide title font sizes
of each level.

  (setq moe-theme-resize-rst-title '(2.0 1.7 1.5 1.3 1.1 1.0))

If the value is t, the titles will be resized by its level.
If the vaule is nil, all the outlines will be the same size.")

(defvar moe-theme-revert-theme t
  "When call (moe-light) or (moe-dark), `load-theme' & `set-background-color'
 (they would cause screen flashing) again or not. If doesn't need load-theme,
set this to nil temporarily: (let (moe-theme-revert-theme) ...)
DO NOT CHANGE ITS VALUE.")

(defvar moe-theme-powerline-enable-p nil
  "A variable indicate if `powerline-moe-theme' has been run.
If you want to use powerline powered by moe-theme, please run
`powerline-moe-theme', or `moe-theme-select-color' may act
incorrectly.

Don't setq this manually.")

(defun moe-theme-resize-font-size ()
  (when (and (listp moe-theme-resize-markdown-title)
             (not (null moe-theme-resize-markdown-title))
             (>= (length moe-theme-resize-markdown-title) 6))
    (let* ((s1 (car moe-theme-resize-markdown-title))
           (s2 (cadr moe-theme-resize-markdown-title))
           (s3 (nth 2 moe-theme-resize-markdown-title))
           (s4 (nth 3 moe-theme-resize-markdown-title))
           (s5 (nth 4 moe-theme-resize-markdown-title))
           (s6 (nth 5 moe-theme-resize-markdown-title)))
      (progn (require 'markdown-mode)
             (set-face-attribute 'markdown-header-face-1 nil :height s1)
             (set-face-attribute 'markdown-header-face-2 nil :height s2)
             (set-face-attribute 'markdown-header-face-3 nil :height s3)
             (set-face-attribute 'markdown-header-face-4 nil :height s4)
             (set-face-attribute 'markdown-header-face-5 nil :height s5)
             (set-face-attribute 'markdown-header-face-6 nil :height s6))))

  (when (and (listp moe-theme-resize-org-title)
             (not (null moe-theme-resize-org-title))
             (>= (length moe-theme-resize-org-title) 9))
    (let* ((s1 (car moe-theme-resize-org-title))
           (s2 (cadr moe-theme-resize-org-title))
           (s3 (nth 2 moe-theme-resize-org-title))
           (s4 (nth 3 moe-theme-resize-org-title))
           (s5 (nth 4 moe-theme-resize-org-title))
           (s6 (nth 5 moe-theme-resize-org-title))
           (s7 (nth 6 moe-theme-resize-org-title))
           (s8 (nth 7 moe-theme-resize-org-title))
           (s9 (nth 8 moe-theme-resize-org-title)))
      (progn (require 'org)
             (set-face-attribute 'org-document-title nil :height s1)
             (set-face-attribute 'org-level-1 nil :height s2)
             (set-face-attribute 'org-level-2 nil :height s3)
             (set-face-attribute 'org-level-3 nil :height s4)
             (set-face-attribute 'org-level-4 nil :height s5)
             (set-face-attribute 'org-level-5 nil :height s6)
             (set-face-attribute 'org-level-6 nil :height s7)
             (set-face-attribute 'org-level-7 nil :height s8)
             (set-face-attribute 'org-level-8 nil :height s9))))

  (when (and (listp moe-theme-resize-rst-title)
             (not (null moe-theme-resize-rst-title))
             (>= (length moe-theme-resize-rst-title) 6))
    (let* ((s1 (car moe-theme-resize-rst-title))
           (s2 (cadr moe-theme-resize-rst-title))
           (s3 (nth 2 moe-theme-resize-rst-title))
           (s4 (nth 3 moe-theme-resize-rst-title))
           (s5 (nth 4 moe-theme-resize-rst-title))
           (s6 (nth 5 moe-theme-resize-rst-title)))
      (require 'rst)
      (if (facep 'rst-level-1-face)
	  (progn
	    (set-face-attribute 'rst-level-1-face nil :height s1)
	    (set-face-attribute 'rst-level-2-face nil :height s2)
	    (set-face-attribute 'rst-level-3-face nil :height s3)
	    (set-face-attribute 'rst-level-4-face nil :height s4)
	    (set-face-attribute 'rst-level-5-face nil :height s5)
	    (set-face-attribute 'rst-level-6-face nil :height s6))
	(progn
	  (set-face-attribute 'rst-level-1 nil :height s1)
	  (set-face-attribute 'rst-level-2 nil :height s2)
	  (set-face-attribute 'rst-level-3 nil :height s3)
	  (set-face-attribute 'rst-level-4 nil :height s4)
	  (set-face-attribute 'rst-level-5 nil :height s5)
	  (set-face-attribute 'rst-level-6 nil :height s6))
	)))
  )

(defun moe-light ()
  "Load moe-light-theme with your customizations."
  (interactive)
  (if (not (null moe-theme-revert-theme)) ;Avoid unnecessary flashing screen when using random-color
      (progn (load-theme 'moe-light t)
             (moe-theme-resize-font-size)

             (if (and (not (null moe-light-pure-white-background-in-terminal))
                      (null (window-system)))
                 (set-face-attribute 'default nil :background "#ffffff" :foreground "#5f5f5f"))))
  ;; In Emacs 24.3.50.1 , background-color may cannot be changed under GUI Emacs.
  ;; So do this again.
  (if window-system
      (progn
        (set-background-color "#fdfde7")
        (set-foreground-color "#5f5f5f")))

  (cond ((eq moe-theme-mode-line-color 'blue)
         (set-face-attribute 'mode-line nil :background "#afd7ff" :foreground "#005f87")
         (set-face-attribute 'mode-line-buffer-id nil :background "#afd7ff" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#5fafd7" :background "#3a3a3a"))
        ((eq moe-theme-mode-line-color 'green)
         (set-face-attribute 'mode-line nil :background "#afdf77" :foreground "#005f00")
         (set-face-attribute 'mode-line-buffer-id nil :background "#a1db00" :foreground "#3a3a3a")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#a1db00" :background "#3a3a3a"))
        ((eq moe-theme-mode-line-color 'orange)
         (set-face-attribute 'mode-line nil :background "#ffd787" :foreground "#d75f00")
         (set-face-attribute 'mode-line-buffer-id nil :background "#ffd787" :foreground "#1c1c1c")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#ff8700" :background "#1c1c1c"))
        ((eq moe-theme-mode-line-color 'magenta)
         (set-face-attribute 'mode-line nil :background "#ffafd7" :foreground "#cc1f8b")
         (set-face-attribute 'mode-line-buffer-id nil :background "#ffafd7" :foreground "#3a3a3a")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#ff4ea3" :background "#3a3a3a"))
        ((eq moe-theme-mode-line-color 'yellow)
         (set-face-attribute 'mode-line nil :background "#fce94f" :foreground "#875f00")
         (set-face-attribute 'mode-line-buffer-id nil :background "#fce94f" :foreground "#3a3a3a")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#fce94f" :background "#3a3a3a"))
        ((eq moe-theme-mode-line-color 'purple)
         (set-face-attribute 'mode-line nil :background "#e6a8df" :foreground "#6c0099")
         (set-face-attribute 'mode-line-buffer-id nil :background "#e6a8df" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#af5fd7" :background "#080808"))
        ((eq moe-theme-mode-line-color 'red)
         (set-face-attribute 'mode-line nil :background "#ffafaf" :foreground "#d40000")
         (set-face-attribute 'mode-line-buffer-id nil :background "#ffafaf" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#ff4b4b" :background "#080808"))
        ((eq moe-theme-mode-line-color 'cyan)
         (set-face-attribute 'mode-line nil :background "#87d7af" :foreground "#005f5f")
         (set-face-attribute 'mode-line-buffer-id nil :background "#87d7af" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#5faf87" :background "#3a3a3a"))
        ((eq moe-theme-mode-line-color 'w/b)
         (set-face-attribute 'mode-line nil :background "#9b9b9b" :foreground "#ffffff")
         (set-face-attribute 'mode-line-buffer-id nil :background "#9b9b9b" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#8a8a8a" :background "#080808")))

  (if (null moe-theme-highlight-buffer-id)
      (set-face-attribute 'mode-line-buffer-id nil :background nil))

  (if (eq moe-theme-powerline-enable-p t)
      (powerline-moe-theme)))

(defun moe-dark ()
  "Load moe-light-theme with your customizations."
  (interactive)
  (if (not (null moe-theme-revert-theme))
      (load-theme 'moe-dark t))

  ;; In Emacs 24.3.50.1 , background-color may cannot be changed under GUI Emacs.
  ;; So do this again.
  (if window-system
    (progn
      (set-background-color "#303030")
      (set-foreground-color "#c6c6c6")))

  (moe-theme-resize-font-size)
  (cond ((eq moe-theme-mode-line-color 'blue)
         (set-face-attribute 'mode-line nil :background "#afd7ff" :foreground "#005f87")
         (set-face-attribute 'mode-line-buffer-id nil :background "#afd7ff" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#005f87" :background "#afd7ff"))
        ((eq moe-theme-mode-line-color 'green)
         (set-face-attribute 'mode-line nil :background "#afdf77" :foreground "#005f00")
         (set-face-attribute 'mode-line-buffer-id nil :background "#afdf77" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#005f00" :background "#a1db00"))
        ((eq moe-theme-mode-line-color 'orange)
         (set-face-attribute 'mode-line nil :background "#ffaf5f" :foreground "#b75f00")
         (set-face-attribute 'mode-line-buffer-id nil :background "#ffaf5f" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#080808" :background "#ffaf5f"))
        ((eq moe-theme-mode-line-color 'magenta)
         (set-face-attribute 'mode-line nil :background "#ffafd7" :foreground "#cc1f8b")
         (set-face-attribute 'mode-line-buffer-id nil :background "#ffafd7" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#ff1f8b" :background "#ffffff"))
        ((eq moe-theme-mode-line-color 'yellow)
         (set-face-attribute 'mode-line nil :background "#fce94f" :foreground "#875f00")
         (set-face-attribute 'mode-line-buffer-id nil :background "#fce94f" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#875f00" :background "#ffff87"))
        ((eq moe-theme-mode-line-color 'purple)
         (set-face-attribute 'mode-line nil :background "#e6a8df" :foreground "#6c0099")
         (set-face-attribute 'mode-line-buffer-id nil :background "#e6a8df" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#6c0099" :background "#e6a8df"))
        ((eq moe-theme-mode-line-color 'red)
         (set-face-attribute 'mode-line nil :background "#ef2929" :foreground "#ffffff")
         (set-face-attribute 'mode-line-buffer-id nil :background "#ef2929" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#a40000" :background "#ffafaf"))
        ((eq moe-theme-mode-line-color 'cyan)
         (set-face-attribute 'mode-line nil :background "#87d7af" :foreground "#005f5f")
         (set-face-attribute 'mode-line-buffer-id nil :background "#87d7af" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#005f5f" :background "#87d7af"))
        ((eq moe-theme-mode-line-color 'w/b)
         (set-face-attribute 'mode-line nil :background "#ffffff" :foreground "#3a3a3a")
         (set-face-attribute 'mode-line-buffer-id nil :background "#9e9e9e" :foreground "#080808")
         (set-face-attribute 'minibuffer-prompt nil :foreground "#3e3e3e" :background "#ffffff")))

  (if (null moe-theme-highlight-buffer-id)
      (set-face-attribute 'mode-line-buffer-id nil :background nil))

  (if (eq moe-theme-powerline-enable-p t)
      (powerline-moe-theme)))

;; Powerline

(defun moe-theme-set-color (color)
  "Set the COLOR of mode-line you like. You may also like
`moe-theme-random-color' This should be called
programmly (e.g. in init.el), not interactively."
  (setq moe-theme-mode-line-color
		color)
    (let (moe-theme-revert-theme) ;set to nil to change only mode-line's color
      (if (eq (frame-parameter nil 'background-mode) 'light)
          (moe-light)
        (moe-dark)))
  (if (eq moe-theme-powerline-enable-p t)
      (powerline-moe-theme)))

(defun moe-theme-select-color ()
  "Interactively select the color of mode-line you like and set
it. (Notice: we support Powerline :D) You may also like
`moe-theme-random-color'"
  (interactive)
  (moe-theme-set-color (intern (completing-read
                 "Select a color: "
                 '((blue) (green) (orange) (magenta) (yellow) (purple) (red) (cyan) (w/b))
                 nil t "" nil nil t))))

(defun moe-theme-random-color ()
  "Give me a random mode-line color.=w=+
This function can be called both programmly and interactively."
  (interactive)
  (let* ((n (abs (% (random) 9)))
         (current-color moe-theme-mode-line-color)
         (color-list '(blue green orange magenta yellow purple red cyan w/b)))
    (if (eq (elt color-list n) current-color) ;If gotten color eq current-color, random again.
        (moe-theme-random-color)
      (moe-theme-set-color (elt color-list n)))))

(when (require 'powerline nil :no-error)
  (defadvice powerline-revert (after moe-theme-powerline-revert activate)
    "Auto set `moe-theme-powerline-enable-p' to nil after `powerline-revert'
Because when `powerline-moe-theme' has been run, `moe-theme-select-color'
and `moe-theme-random-color' should call `powerline-moe-theme' again for update."
    (setq moe-theme-powerline-enable-p nil)
    (if (eq (frame-parameter nil 'background-mode) 'light)
          (moe-light)
        (moe-dark)))

  (defun powerline-moe-theme ()
    "Powerline theme powered by moe-theme.el
It's recommended use this with `moe-light' or `moe-dark', but it's ok without them,
as long as setq `moe-theme-mode-line-color' first."
    (interactive)
    (cond ((eq (frame-parameter nil 'background-mode) 'light)
           (set-face-attribute 'mode-line-buffer-id nil :background nil :foreground "#1c1c1c")
             (set-face-attribute 'mode-line-inactive nil :background "#b2b2b2" :foreground "#ffffff")
             (set-face-attribute 'powerline-active2 nil :background "#585858" :foreground "#ffffff")
             (set-face-attribute 'powerline-inactive1 nil :background "#c6c6c6" :foreground "#585858")
             (set-face-attribute 'powerline-inactive2 nil :background "#e4e4e4" :foreground "#585858")
             (cond ((eq moe-theme-mode-line-color 'blue)
                    (set-face-attribute 'mode-line nil :background "#5fafd7" :foreground "#ffffff")
                    (set-face-attribute 'powerline-active1 nil :background "#afd7ff" :foreground "#005faf"))
                   ((eq moe-theme-mode-line-color 'green)
                    (set-face-attribute 'mode-line nil :background "#a1db00" :foreground "#005f00")
                    (set-face-attribute 'powerline-active1 nil :background "#d7ff87" :foreground "#008700"))
                   ((eq moe-theme-mode-line-color 'orange)
                    (set-face-attribute 'mode-line nil :background "#ff8700" :foreground "#ffffff")
                    (set-face-attribute 'powerline-active1 nil :background "#ffd787" :foreground "#d75f00"))
                   ((eq moe-theme-mode-line-color 'magenta)
                    (set-face-attribute 'mode-line nil :background "#ff4ea3" :foreground "#ffffff")
                    (set-face-attribute 'powerline-active1 nil :background "#ffafff" :foreground "#ff1f8b"))
                   ((eq moe-theme-mode-line-color 'yellow)
                    (set-face-attribute 'mode-line nil :background "#fce94f" :foreground "#875f00")
                    (set-face-attribute 'powerline-active1 nil :background "#ffff87" :foreground "#875f00"))
                   ((eq moe-theme-mode-line-color 'purple)
                    (set-face-attribute 'mode-line nil :background "#af5fd7" :foreground "#ffffff")
                    (set-face-attribute 'powerline-active1 nil :background "#e6a8df" :foreground "#6c0099"))
                   ((eq moe-theme-mode-line-color 'red)
                    (set-face-attribute 'mode-line nil :background "#ff4b4b" :foreground "#ffffff")
                    (set-face-attribute 'powerline-active1 nil :background "#ffafaf" :foreground "#cc0000"))
                   ((eq moe-theme-mode-line-color 'cyan)
                    (set-face-attribute 'mode-line nil :background "#5faf87" :foreground "#ffffff")
                    (set-face-attribute 'powerline-active1 nil :background "#87d7af" :foreground "#005f5f"))
                   ((eq moe-theme-mode-line-color 'w/b)
                    (set-face-attribute 'mode-line nil :background "#1c1c1c" :foreground "#ffffff")
                    (set-face-attribute 'powerline-active1 nil :background "#bcbcbc" :foreground "#3a3a3a")
                    (set-face-attribute 'mode-line-buffer-id nil :background nil :foreground "#ffffff"))))
          ((eq (frame-parameter nil 'background-mode) 'dark)
           (set-face-attribute 'mode-line-buffer-id nil :background nil :foreground "#080808")
           (set-face-attribute 'mode-line-inactive nil :background "#4e4e4e" :foreground "#9e9e9e")
           (set-face-attribute 'powerline-active2 nil :background "#ffffff" :foreground "#3a3a3a")
           (set-face-attribute 'powerline-inactive1 nil :background "#626262" :foreground "#eeeeee")
           (set-face-attribute 'powerline-inactive2 nil :background "#767676" :foreground "#e4e4e4")
           (cond ((eq moe-theme-mode-line-color 'blue)
                  (set-face-attribute 'mode-line nil :background "#5fafd7" :foreground "#ffffff")
                  (set-face-attribute 'powerline-active1 nil :background "#afd7ff" :foreground "#005faf"))
                 ((eq moe-theme-mode-line-color 'green)
                  (set-face-attribute 'mode-line nil :background "#a1db00" :foreground "#005f00")
                  (set-face-attribute 'powerline-active1 nil :background "#d7ff87" :foreground "#008700"))
                 ((eq moe-theme-mode-line-color 'orange)
                  (set-face-attribute 'mode-line nil :background "#ff8700" :foreground "#ffffff")
                  (set-face-attribute 'powerline-active1 nil :background "#ffd787" :foreground "#d75f00"))
                 ((eq moe-theme-mode-line-color 'magenta)
                  (set-face-attribute 'mode-line nil :background "#ff4ea3" :foreground "#ffffff")
                  (set-face-attribute 'powerline-active1 nil :background "#ffafff" :foreground "#ff1f8b"))
                 ((eq moe-theme-mode-line-color 'yellow)
                  (set-face-attribute 'mode-line nil :background "#fce94f" :foreground "#875f00")
                  (set-face-attribute 'powerline-active1 nil :background "#ffff87" :foreground "#875f00"))
                 ((eq moe-theme-mode-line-color 'purple)
                  (set-face-attribute 'mode-line nil :background "#af5fd7" :foreground "#ffffff")
                  (set-face-attribute 'powerline-active1 nil :background "#e6a8df" :foreground "#6c0099"))
                 ((eq moe-theme-mode-line-color 'red)
                  (set-face-attribute 'mode-line nil :background "#ff4b4b" :foreground "#ffffff")
                  (set-face-attribute 'powerline-active1 nil :background "#ffafaf" :foreground "#cc0000"))
                 ((eq moe-theme-mode-line-color 'cyan)
                  (set-face-attribute 'mode-line nil :background "#5faf87" :foreground "#ffffff")
                  (set-face-attribute 'powerline-active1 nil :background "#87d7af" :foreground "#005f5f"))
                 ((eq moe-theme-mode-line-color 'w/b)
                  (set-face-attribute 'mode-line nil :background "#ffffff" :foreground "#080808")
                  (set-face-attribute 'powerline-active1 nil :background "#bcbcbc" :foreground "#3a3a3a")
                  (set-face-attribute 'mode-line-buffer-id nil :background nil :foreground "#3a3a3a")))))
    (powerline-default-theme)
    (powerline-reset)
    (setq moe-theme-powerline-enable-p t)))


(provide 'moe-theme)

;; Local Variables:
;; coding: utf-8
;; End:
;;; moe-theme.el ends here
