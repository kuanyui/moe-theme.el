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
    (setq moe-dark-builtin                   dark-aqua)
    (setq moe-dark-comment-delimiter         dark-comment)
    (setq moe-dark-comment                   dark-comment)
    (setq moe-dark-constant                  dark-aqua)
    (setq moe-dark-doc                       dark-comment)
    (setq moe-dark-doc-string                dark-yellow)
    (setq moe-dark-function-name             dark-blue)
    (setq moe-dark-keyword                   dark-purple)
    (setq moe-dark-negation-char             dark-green)
    (setq moe-dark-preprocessor              dark-purple)
    (setq moe-dark-regexp-grouping-backslash dark-yellow)
    (setq moe-dark-regexp-grouping-construct dark-purple)
    (setq moe-dark-string                    dark-green)
    (setq moe-dark-type                      dark-yellow)
    (setq moe-dark-variable-name             dark-orange)
    (setq moe-dark-warning                   dark-red)

    (setq moe-light-bg                        light-background)
    (setq moe-light-fg                        light-foreground)
    (setq moe-light-builtin                   light-aqua)
    (setq moe-light-comment-delimiter         light-comment)
    (setq moe-light-comment                   light-comment)
    (setq moe-light-constant                  light-aqua)
    (setq moe-light-doc                       light-comment)
    (setq moe-light-doc-string                light-yellow)
    (setq moe-light-function-name             light-blue)
    (setq moe-light-keyword                   light-purple)
    (setq moe-light-negation-char             light-green)
    (setq moe-light-preprocessor              light-purple)
    (setq moe-light-regexp-grouping-backslash light-yellow)
    (setq moe-light-regexp-grouping-construct light-purple)
    (setq moe-light-string                    light-green)
    (setq moe-light-type                      light-yellow)
    (setq moe-light-variable-name             light-orange)
    (setq moe-light-warning                   light-red)
    (if (eq (frame-parameter nil 'background-mode) 'light)
        (load-theme 'moe-light)
      (load-theme 'moe-dark)
      )))


(provide 'moe-theme-flavours)
;;; moe-theme-flavours.el ends here
