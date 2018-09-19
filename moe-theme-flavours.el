;;; moe-theme-flavours.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2018  onohiroko

;; Author: onohiroko <kuanyui.github.io>
;; Keywords: theme

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun moe-theme-reload-by-background-brightness ()
  (if (eq (frame-parameter nil 'background-mode) 'light)
        (load-theme 'moe-light)
      (load-theme 'moe-dark)))

;; ======================================================
;; Default
;; ======================================================
(defun moe-theme-flavour-default ()
  (interactive)
  (mapc (lambda (x)
          (set x (symbol-value (intern (concat "-" (symbol-name x))))))
        '(moe-dark-bg
          moe-dark-fg
          moe-dark-builtin
          moe-dark-comment-delimiter
          moe-dark-comment
          moe-dark-constant
          moe-dark-doc
          moe-dark-doc-string
          moe-dark-function-name
          moe-dark-keyword
          moe-dark-negation-char
          moe-dark-preprocessor
          moe-dark-regexp-grouping-backslash
          moe-dark-regexp-grouping-construct
          moe-dark-string
          moe-dark-type
          moe-dark-variable-name
          moe-dark-warning
          moe-light-bg
          moe-light-fg
          moe-light-builtin
          moe-light-comment-delimiter
          moe-light-comment
          moe-light-constant
          moe-light-doc
          moe-light-doc-string
          moe-light-function-name
          moe-light-keyword
          moe-light-negation-char
          moe-light-preprocessor
          moe-light-regexp-grouping-backslash
          moe-light-regexp-grouping-construct
          moe-light-string
          moe-light-type
          moe-light-variable-name
          moe-light-warning))
  (moe-theme-reload-by-background-brightness)
  )
(default-value 'moe-light-string)

;; ======================================================
;; Tomorrow
;; ======================================================

(defun moe-theme-flavour-tomorrow ()
  (interactive)
  (let ((dark-background "#1d1f21")
        (dark-current-line "#282a2e")
        (dark-selection "#373b41")
        (dark-foreground "#c5c8c6")
        (dark-comment "#969896")
        (dark-red "#cc6666")
        (dark-orange "#de935f")
        (dark-yellow "#f0c674")
        (dark-green "#b5bd68")
        (dark-aqua "#8abeb7")
        (dark-blue "#81a2be")
        (dark-purple "#b294bb")
        (light-background "#ffffff")
        (light-current-line "#efefef")
        (light-selection "#d6d6d6")
        (light-foreground "#4d4d4c")
        (light-comment "#8e908c")
        (light-red "#c82829")
        (light-orange "#f5871f")
        (light-yellow "#eab700")
        (light-green "#718c00")
        (light-aqua "#3e999f")
        (light-blue "#4271ae")
        (light-purple "#8959a8"))
    (setq moe-dark-bg                        dark-background)
    (setq moe-dark-fg                        dark-foreground)
    (setq moe-dark-builtin                   dark-purple)
    (setq moe-dark-comment-delimiter         dark-comment)
    (setq moe-dark-comment                   dark-comment)
    (setq moe-dark-constant                  dark-blue)
    (setq moe-dark-doc                       dark-purple)
    (setq moe-dark-doc-string                dark-yellow)
    (setq moe-dark-function-name             dark-orange)
    (setq moe-dark-keyword                   dark-green)
    (setq moe-dark-negation-char             dark-blue)
    (setq moe-dark-preprocessor              dark-purple)
    (setq moe-dark-regexp-grouping-backslash dark-yellow)
    (setq moe-dark-regexp-grouping-construct dark-purple)
    (setq moe-dark-string                    dark-aqua)
    (setq moe-dark-type                      dark-blue)
    (setq moe-dark-variable-name             dark-yellow)
    (setq moe-dark-warning                   dark-red)

    (setq moe-light-bg                        light-background)
    (setq moe-light-fg                        light-foreground)
    (setq moe-light-builtin                   light-purple)
    (setq moe-light-comment-delimiter         light-comment)
    (setq moe-light-comment                   light-comment)
    (setq moe-light-constant                  light-blue)
    (setq moe-light-doc                       light-purple)
    (setq moe-light-doc-string                light-yellow)
    (setq moe-light-function-name             light-orange)
    (setq moe-light-keyword                   light-green)
    (setq moe-light-negation-char             light-blue)
    (setq moe-light-preprocessor              light-purple)
    (setq moe-light-regexp-grouping-backslash light-yellow)
    (setq moe-light-regexp-grouping-construct light-purple)
    (setq moe-light-string                    light-aqua)
    (setq moe-light-type                      light-blue)
    (setq moe-light-variable-name             light-yellow)
    (setq moe-light-warning                   light-red)
    (moe-theme-reload-by-background-brightness)))

;; ======================================================
;; Monokai
;; ======================================================
(defun moe-theme-flavour-monokai ()
  (interactive)
  (let ((monokai-yellow "#E6DB74")
        (monokai-orange "#FD971F")
        (monokai-red "#F92672")
        (monokai-magenta "#FD5FF0")
        (monokai-blue "#66D9EF")
        (monokai-green "#A6E22E")
        (monokai-cyan "#A1EFE4")
        (monokai-violet "#AE81FF")
        (monokai-gray "#64645E")
        (monokai-foreground "#F8F8F2")
        (monokai-background "#272722")
        (monokai-comments "#757165")
        (monokai-emphasis "#F8F8F0")
        (monokai-line-number "#8F908A")
        (monokai-highlight "#49483E")
        (monokai-highlight-alt "#3E3D31")
        (monokai-highlight-line "#3C3D37"))
    (setq moe-dark-bg monokai-background)
    (setq moe-dark-fg monokai-foreground)
    (setq moe-dark-builtin monokai-red)
    (setq moe-dark-comment-delimiter monokai-comments)
    (setq moe-dark-comment monokai-comments)
    (setq moe-dark-constant monokai-violet)
    (setq moe-dark-doc monokai-yellow)
    (setq moe-dark-doc-string monokai-cyan)
    (setq moe-dark-function-name monokai-green)
    (setq moe-dark-keyword monokai-red)
    (setq moe-dark-negation-char monokai-yellow)
    (setq moe-dark-preprocessor monokai-red)
    (setq moe-dark-regexp-grouping-construct monokai-yellow)
    (setq moe-dark-regexp-grouping-backslash monokai-violet)
    (setq moe-dark-string monokai-yellow)
    (setq moe-dark-type monokai-blue)
    (setq moe-dark-variable-name monokai-orange)
    (setq moe-dark-warning monokai-orange)
    (moe-theme-reload-by-background-brightness)))



(provide 'moe-theme-flavours)
;;; moe-theme-flavours.el ends here
