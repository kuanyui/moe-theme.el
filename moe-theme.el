;;; moe-theme.el --- A colorful eye-candy theme. Moe, moe, kyun!

;; Copyright (C) 2013-2022 kuanyui

;; This file is established for packaging. If you want to install manually,
;; check README.md
;;
;; Author: kuanyui <azazabc123@gmail.com>
;; Keywords: themes
;; X-URL: https://github.com/kuanyui/moe-theme.el
;; URL: https://github.com/kuanyui/moe-theme.el
;; Version: 1.0.2

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; You can take a look at screenshots and acquire more information on:
;;
;;     https://github.com/kuanyui/moe-theme.el
;;

;;; Code:
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(require 'moe-light-theme)
(require 'moe-dark-theme)
(require 'cl-lib)

;; ======================================================
;; Buffer ID
;; ======================================================

(defvar moe-theme-highlight-buffer-id t
  "If t, highlight buffer-id on mode-line.
If nil, just bold buffer-id without highlight")

;; ======================================================
;; Modeline Color
;; ======================================================

(defconst moe-theme-modeline-available-colors-set '(blue green orange magenta yellow purple red cyan w/b))

(defvar moe-theme-mode-line-color 'blue
  "Default is 'blue.
If nil, no background color.
Available choices: 'blue, 'cyan', 'green, 'magenta, 'red, 'orange, 'yellow, 'purple, 'b/w")

;; ======================================================
;; Auto Change Modeline Color By Frame ID
;; ======================================================

(defvar moe-theme-colorize-modeline-by-frame-id nil
  "Auto change mode-line color after switching frame
(If Elscreen is enabled, use it first.)
This feature rely on a checksum function to ensure a predictable order of color.
Function `moe-theme-get-color-by-frame-name' is the implementation.")

(defvar moe-theme-colorize-modeline-by-frame-id-color-set '(cyan green orange blue yellow magenta b/w purple)
  "See `moe-theme-colorize-modeline-by-frame-id'.
`moe-theme-get-color-by-frame-name' will choose a color from this list")

;; ======================================================
;; Background
;; ======================================================

(defvar moe-light-pure-white-background-in-terminal nil
  "With 256-colors, default yellow background of moe-light may be 'too yellow'
on some screen.

If you encounter this problem, and want to have a background of #ffffff, set
the value of `moe-light-pure-white-background-in-terminal' to t.")

;; ======================================================
;; Resize Titles (Markdown / Org / RST)
;; ======================================================
(defvar moe-theme-resize-title-markdown nil
  "Resize header/title faces of Markdown-mode or not. (default value is nil)
The value should be a list with 6 items of number, which decide title font sizes
of each level.

  (setq moe-theme-resize-title-markdown '(2.0 1.7 1.5 1.3 1.0 1.0))

If the vaule is nil, all the titles will be the same size.")

(defvar moe-theme-resize-title-org nil
  "Resize outline/title faces of Org-mode or not. (default value is nil)

The value should be a list with 9 items of number; which decide title font sizes
of each level. The first item is the font size of `org-document-title', from
second to ninth is outlines of 1-8.
For example, you can set like this:

  (setq moe-theme-resize-title-org '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))

If the vaule is nil, all the outlines will be the same size.")

(defvar moe-theme-resize-title-rst nil
  "Resize title faces of ReStructuredText-mode or not. (default value is nil)
The value should be a list with 6 items of number, which decide title font sizes
of each level.

  (setq moe-theme-resize-title-rst '(2.0 1.7 1.5 1.3 1.1 1.0))

If the value is t, the titles will be resized by its level.
If the vaule is nil, all the outlines will be the same size.")

;; ======================================================
;; INTERNAL VARIABLES
;; ======================================================

(defvar moe-theme--need-reload-theme t
  "For internal use. DO NOT CHANGE THIS.
Avoid unnecessary load-theme")

;; ======================================================
;; Resize Titles
;; ======================================================

(defun moe-theme-resize-title--repaire-list (list final-length)
  "Non-destructive"
  (if (not (listp list))
      (setq list '()))
  (setq list (cl-delete-if-not #'numberp list))
  (while (< (length list) final-length)
    (setq list (append list '(1.0))))
  list)

(with-eval-after-load "markdown"
  (defun moe-theme-resize-title-apply-markdown ()
    (setq moe-theme-resize-title-markdown (moe-theme-resize-title--repaire-list moe-theme-resize-title-markdown 6))
    (set-face-attribute 'markdown-header-face-1 nil :height (nth 0 moe-theme-resize-title-markdown))
    (set-face-attribute 'markdown-header-face-2 nil :height (nth 1 moe-theme-resize-title-markdown))
    (set-face-attribute 'markdown-header-face-3 nil :height (nth 2 moe-theme-resize-title-markdown))
    (set-face-attribute 'markdown-header-face-4 nil :height (nth 3 moe-theme-resize-title-markdown))
    (set-face-attribute 'markdown-header-face-5 nil :height (nth 4 moe-theme-resize-title-markdown))
    (set-face-attribute 'markdown-header-face-6 nil :height (nth 5 moe-theme-resize-title-markdown)))
  (moe-theme-resize-title-apply-markdown))

(with-eval-after-load "org"
  (defun moe-theme-resize-title-apply-org ()
    (setq moe-theme-resize-title-org (moe-theme-resize-title--repaire-list moe-theme-resize-title-org 9))
    (set-face-attribute 'org-document-title nil :height (nth 0 moe-theme-resize-title-org))
    (set-face-attribute 'org-level-1        nil :height (nth 1 moe-theme-resize-title-org))
    (set-face-attribute 'org-level-2        nil :height (nth 2 moe-theme-resize-title-org))
    (set-face-attribute 'org-level-3        nil :height (nth 3 moe-theme-resize-title-org))
    (set-face-attribute 'org-level-4        nil :height (nth 4 moe-theme-resize-title-org))
    (set-face-attribute 'org-level-5        nil :height (nth 5 moe-theme-resize-title-org))
    (set-face-attribute 'org-level-6        nil :height (nth 6 moe-theme-resize-title-org))
    (set-face-attribute 'org-level-7        nil :height (nth 7 moe-theme-resize-title-org))
    (set-face-attribute 'org-level-8        nil :height (nth 8 moe-theme-resize-title-org)))
  (moe-theme-resize-title-apply-org))

(with-eval-after-load "rst"
  (defun moe-theme-resize-title-apply-rst ()
    (setq moe-theme-resize-title-rst (moe-theme-resize-title--repaire-list moe-theme-resize-title-rst 6))
    (if (facep 'rst-level-1-face)
        (progn
          (set-face-attribute 'rst-level-1-face nil :height (nth 0 moe-theme-resize-title-rst))
          (set-face-attribute 'rst-level-2-face nil :height (nth 1 moe-theme-resize-title-rst))
          (set-face-attribute 'rst-level-3-face nil :height (nth 2 moe-theme-resize-title-rst))
          (set-face-attribute 'rst-level-4-face nil :height (nth 3 moe-theme-resize-title-rst))
          (set-face-attribute 'rst-level-5-face nil :height (nth 4 moe-theme-resize-title-rst))
          (set-face-attribute 'rst-level-6-face nil :height (nth 5 moe-theme-resize-title-rst)))
      (progn
        (set-face-attribute 'rst-level-1 nil :height (nth 0 moe-theme-resize-title-rst))
        (set-face-attribute 'rst-level-2 nil :height (nth 1 moe-theme-resize-title-rst))
        (set-face-attribute 'rst-level-3 nil :height (nth 2 moe-theme-resize-title-rst))
        (set-face-attribute 'rst-level-4 nil :height (nth 3 moe-theme-resize-title-rst))
        (set-face-attribute 'rst-level-5 nil :height (nth 4 moe-theme-resize-title-rst))
        (set-face-attribute 'rst-level-6 nil :height (nth 5 moe-theme-resize-title-rst)))))
  (moe-theme-resize-title-apply-rst))

(defun moe-theme--common-setup ()
  (if (functionp 'powerline) (powerline-moe-theme))
  (if (functionp 'moe-theme-resize-title-apply-markdown) (moe-theme-resize-title-apply-markdown))
  (if (functionp 'moe-theme-resize-title-apply-org) (moe-theme-resize-title-apply-org))
  (if (functionp 'moe-theme-resize-title-apply-rst) (moe-theme-resize-title-apply-rst))
  )

(defun moe-light ()
  "Load moe-light-theme with additional customizations"
  (interactive)
  (if moe-theme--need-reload-theme ;Avoid unnecessary flashing screen when using random-color
      (progn (load-theme 'moe-light t)
             (if (and (not (null moe-light-pure-white-background-in-terminal))
                      (null (window-system)))
                 (set-face-attribute 'default nil :background "#ffffff" :foreground "#5f5f5f"))))

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
  (moe-theme--common-setup))

(defun moe-dark ()
  "Load moe-light-theme with your additional customization."
  (interactive)
  (if moe-theme--need-reload-theme (load-theme 'moe-dark t))
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
  (moe-theme--common-setup))

;; ======================================================
;; Colorize mode-line (and Powerline)
;; ======================================================

(defun moe-theme-apply-color (color)
  "Set the COLOR of mode-line you like. You may also like
`moe-theme-random-color' This should be called
programmly (e.g. in init.el), not interactively."
  (setq moe-theme-mode-line-color
        color)
  (let (moe-theme--need-reload-theme) ;set to nil to change only mode-line's color
    (if (eq (frame-parameter nil 'background-mode) 'light)
        (moe-light)
      (moe-dark))))


(defun moe-theme-select-color ()
  "Interactively select the color of mode-line you like and set
it. Also see `moe-theme-random-color'"
  (interactive)
  (moe-theme-apply-color (intern (completing-read
                                "Select a color: "
                                (mapcar #'list moe-theme-modeline-available-colors-set)
                                nil t "" nil nil t))))

(defun moe-theme-random-color ()
  "Give me a random mode-line color.
This function can be called both programmly and interactively."
  (interactive)
  (let* ((n (abs (% (random) 9)))
         (current-color moe-theme-mode-line-color))
    (if (eq (elt moe-theme-modeline-available-colors-set n) current-color) ;If gotten color eq current-color, random again.
        (moe-theme-random-color)
      (moe-theme-apply-color (elt moe-theme-modeline-available-colors-set n)))))

;; ======================================================
;; Powerline
;; ======================================================

(with-eval-after-load "powerline"
  (defun moe-theme-powerline ()
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
    (powerline-reset))
  (defalias 'powerline-moe-theme 'moe-theme-powerline))

;; ======================================================
;; Auto Colorize by frame id (Only usable under terminal)
;; ======================================================
(when (null (window-system))
  (defun moe-theme-get-color-by-frame-name ()
    (let* ((obj-name (format "%s" (selected-frame)))
           (name (progn (string-match "#<frame \\(.+?\\) 0x[0-9a-f]+>" obj-name)
                        (match-string-no-properties 1 obj-name)))
           (int (if (string-match "F\\([0-9]+\\)" name)
                    (1- (string-to-number (match-string-no-properties 1 name)))
                  (string-to-number (substring (md5 name) 0 1) 16)))
           (enabled-colors-len (length moe-theme-colorize-modeline-by-frame-id-color-set)))
      (nth (% int enabled-colors-len) moe-theme-colorize-modeline-by-frame-id-color-set)))

  (defadvice other-frame (after change-mode-line-color-by-frame-id activate)
    (if moe-theme-colorize-modeline-by-frame-id
        (moe-theme-apply-color (moe-theme-get-color-by-frame-name))))

  (defadvice delete-frame (after change-mode-line-color-by-frame-id activate)
    (if moe-theme-colorize-modeline-by-frame-id
        (moe-theme-apply-color (moe-theme-get-color-by-frame-name))))

  (defadvice make-frame-command (after change-mode-line-color-by-frame-id activate)
    (if moe-theme-colorize-modeline-by-frame-id
        (moe-theme-apply-color (moe-theme-get-color-by-frame-name)))))

;; support for Elscreen
(with-eval-after-load 'elscreen
  (when (and (window-system))
    (defun moe-theme-get-color-by-frame-name ()
      (let* ((all-screen-indexes (sort (elscreen-get-screen-list) '<))
             (cur-index (elscreen-get-current-screen))
             (enabled-colors-len (length moe-theme-colorize-modeline-by-frame-id-color-set)))
        (nth (% cur-index enabled-colors-len) moe-theme-colorize-modeline-by-frame-id-color-set)))
    (defadvice elscreen-goto (after change-mode-line-color-by-frame-id activate)
      (if moe-theme-colorize-modeline-by-frame-id
          (moe-theme-apply-color (moe-theme-get-color-by-frame-name)))
      )
    ))
;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


(provide 'moe-theme)

;; Local Variables:
;; coding: utf-8
;; End:
;;; moe-theme.el ends here
