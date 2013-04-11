;;; moe-light-theme.el --- Tango-based custom theme for faces

;; Author: kuanyui<azazabc123@gmail.com>
;; Based on "tango-dark-theme" , a part of GNU Emacs 24
;;	    Chong Yidong <cyd@stupidchicken>
;;          Jan Moringen <jan.moringen@uni-bielefeld.de>


;;; Code:

(deftheme moe-light
  "Face colors for 256 colors terminal (dark background).
Moe, moe, chew!")

(let ((class '((class color) (min-colors 89)))
      ;; Palette colors.
      (yellow-1 "#fce94f") (yellow-2 "#ffd700") (yellow-3 "#c4a000")
      (orange-1 "#ff9742") (orange-2 "#ff8700") (orange-3 "#ff5d17")
      (magenta-1 "#ff7bbb") (magenta-2 "#ff4ea3") (magenta-3 "#ff1f8b")
      (green-1 "#afff00") (green-2 "#a1db00") (green-3 "#5faf00") (green-4 "#005f00")
      (blue-1 "#729fcf") (blue-2 "#1f5bff") (blue-3 "#005f87") (blue-4 "#0000af")
      (cyan-1 "#87ffff") (cyan-2 "#00ffff") (cyan-3 "#00d7af") (cyan-4 "#5f87af")
      (purple-1 "#d18aff") (purple-2 "#9a08ff") (purple-3 "#6c0099")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (white-1 "#ffffff") (white-2 "#d3d3d3") (white-3 "#b2b2b2") (LIGHT_BG "#ffffd7")
      (black-1 "#8a8a8a") (black-2 "#585858") (black-3 "#303030")
      ;; Not in palette; used for better contrast.
      (green-01 "#d7ff00")
      (green-0 "#d7ff5f") (blue-0 "#afd7ff") (purple-0 "#e6a8df") (yellow-0 "#ffff87")
      (red-0 "#ff4b4b")  (black-4 "#41423f") (black-5 "#1c1c1c")
      (green-00 "#d7ff87") (yellow-00 "#ffffaf") (blue-00 "#d7d7ff"))

  (custom-theme-set-faces
   'moe-light
   ;; Ensure sufficient contrast on low-color terminals.
   `(default ((((class color) (min-colors 4096))
	       (:foreground ,black-5 :background ,LIGHT_BG))
	      (((class color) (min-colors 256))
	       (:foreground ,black-5 :background ,LIGHT_BG))
	      (,class
	       (:foreground ,black-5 :background "black"))))
   `(cursor ((,class (:background ,white-1))))

   ;; Highlighting faces
   `(fringe ((,class (:foreground ,black-1 :background ,white-2))))
   `(linum ((,class (:foreground ,white-1 :background ,white-3))))
   `(highlight ((,class (:foreground ,black-5 :background ,green-0))))
   `(region ((,class (:foreground ,white-1 :background ,black-1))))
   `(secondary-selection ((,class (:background ,blue-3))))
   `(isearch ((,class (:foreground ,white-1 :background ,orange-3))))
   `(lazy-highlight ((,class (:background ,magenta-3 :foreground ,white-1))))
   `(trailing-whitespace ((,class (:background ,red-3))))

   ;; Mode line & frames' faces
   `(mode-line ((,class
		 (:box (:line-width -1 :style released-button)
		  :background ,blue-1 :foreground ,white-1))))
   `(mode-line-inactive ((,class
			  (:box (:line-width -1 :style released-button)
			   :background ,blue-00 :foreground ,black-1))))
   `(mode-line-buffer-id ((,class (:foreground ,black-3 :background ,blue-1))))
   `(vertical-border ((,class (:foreground ,black-2 :background ,black-2))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,black-3 :background ,green-2))))
   `(escape-glyph ((,class (:foreground ,yellow-3))))
   `(error ((,class (:foreground ,red-0))))
   `(warning ((,class (:foreground ,orange-1))))
   `(success ((,class (:foreground ,green-2))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,purple-2))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,red-2 :slant italic))))
   `(font-lock-comment-face ((,class (:foreground ,red-2))))
   `(font-lock-constant-face ((,class (:foreground ,blue-2))))
   `(font-lock-doc-face ((,class (:foreground ,red-2))))
   `(font-lock-doc-string-face ((,class (:foreground ,yellow-2))))
   `(font-lock-function-name-face ((,class (:foreground ,yellow-2))))
   `(font-lock-keyword-face ((,class (:foreground ,green-2))))
   `(font-lock-negation-char-face ((,class (:foreground ,green-2))))
   `(font-lock-preprocessor-face ((,class (:foreground ,purple-2))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow-2))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple-2))))
   `(font-lock-string-face ((,class (:foreground ,magenta-3))))
   `(font-lock-type-face ((,class (:foreground ,blue-2))))
   `(font-lock-variable-name-face ((,class (:foreground ,orange-2))))
   `(font-lock-warning-face ((,class (:weight bold :foreground ,red-2))))

   ;; org-mode
   `(org-document-title ((,class (:foreground ,blue-1 :background ,LIGHT_BG :weight bold :height 1.5))))
   `(org-document-info ((,class (:foreground ,blue-3 :background ,LIGHT_BG :weight bold))))
   `(org-document-info-keyword ((,class (:foreground ,black-5 :background ,LIGHT_BG))))
   `(org-agenda-date-today
     ((,class (:foreground ,orange-2 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:slant italic))))
   `(org-checkbox ((,class (:background ,white-2 :foreground ,black-3
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,blue-2 :underline t))))
   `(org-done ((,class (:bold t :weight bold :foreground ,green-4 :background ,green-1
                              :box (:line-width 1 :style none)))))
   `(org-todo ((,class (:bold t :weight bold :foreground ,white-1 :background ,red-2
                              :box (:line-width 1 :style none)))))
   `(org-level-1 ((,class (:bold t :foreground ,blue-2 :height 1.3))))
   `(org-level-2 ((,class (:bold t :foreground ,green-2 :height 1.2))))
   `(org-level-3 ((,class (:bold t :foreground ,orange-2 :height 1.1))))
   `(org-level-4 ((,class (:bold t :foreground ,cyan-3))))
   `(org-level-5 ((,class (:bold t :foreground ,red-2))))
   `(org-level-6 ((,class (:bold t :foreground ,magenta-2))))
   `(org-level-7 ((,class (:bold t :foreground ,purple-2))))
   `(org-level-8 ((,class (:bold t :foreground ,yellow-2))))
   `(org-link ((,class (:foreground ,blue-1 :underline t))))
   `(org-tag ((,class (:background ,black-1 :foreground ,white-1 :bold t :weight bold))))
   `(org-column ((,class (:background ,white-3 :foreground ,black-5))))
   `(org-column-title ((,class (:background ,green-2 :foreground ,black-5 :underline t :weight bold))))
   `(org-deadline-announce ((,class (:foreground ,red-3))))
   `(org-scheduled ((,class (:foreground ,green-2))))
   `(org-scheduled-previously ((,class (:foreground ,red-1))))
   `(org-scheduled-today ((,class (:foreground ,blue-2))))
   `(org-special-keyword ((,class (:foreground ,yellow-2))))
   `(org-table ((,class (:background ,yellow-00 :foreground ,black-3))))
   `(org-time-grid ((,class (:foreground ,orange-2))))
   `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   `(org-warning ((,class (:bold t :foreground ,red-3 :weight bold :underline nil))))
   `(org-formula ((,class (:foreground ,yellow-2))))
   `(org-headline-done ((,class (:foreground ,green-2))))
   `(org-hide ((,class (:foreground ,black-2))))
   `(org-code ((,class (:foreground ,yellow-2))))
   `(org-link ((,class (:foreground ,blue-1))))
   `(org-footnote ((,class (:foreground ,magenta-3))))
   `(org-ellipsis ((,class (:foreground ,red-2))))
   `(org-agenda-date ((,class (:foreground ,blue-2 :underline nil))))
   `(org-agenda-todo ((,class (:foreground ,black-3 :background ,red-2))))
   `(org-agenda-done ((,class (:foreground ,black-3 :background ,green-2))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,white-1 :background ,red-2))))
   `(org-block ((,class (:foreground ,orange-1))))
   `(org-quote ((,class (:foreground ,orange-1))))
   `(org-block-begin-line ((,class (:foreground ,orange-2))))
   `(org-block-end-line ((,class (:foreground ,orange-2))))
   `(org-mode-line-clock ((,class (:foreground ,blue-3 :background ,black-5 :bold t))))
   `(org-mode-line-clock-overrun ((,class (:foreground ,black-3 :background ,red-1 :bold t))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((,class (:foreground ,black-5))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,green-2 :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red-2))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow-2))))

   ;; Markdown faces
   `(markdown-blockquote-face ((,class (:foreground ,green-3 :background ,LIGHT_BG :italic t))))
   `(markdown-bold-face ((,class (:foreground ,black-5 :bold t))))
   `(markdown-comment-face ((,class (:foreground ,black-2 :italic t))))
   `(markdown-header-face-1 ((,class (:foreground ,orange-2 :bold t))))
   `(markdown-header-face-2 ((,class (:foreground ,orange-2 :bold t))))
   `(markdown-header-face-3 ((,class (:foreground ,orange-2 :bold t))))
   `(markdown-header-face-4 ((,class (:foreground ,orange-2 :bold t))))
   `(markdown-header-face-5 ((,class (:foreground ,orange-2 :bold t))))
   `(markdown-header-face-6 ((,class (:foreground ,orange-2 :bold t))))
   `(markdown-link-face ((,class (:foreground ,magenta-1 :underline t))))
   `(markdown-inline-code-face ((,class (:foreground ,blue-3 :background ,blue-00))))
   `(markdown-italic-face ((,class (:foreground ,black-5 :italic t))))
   `(markdown-list-face ((,class (:background ,red-2 :foreground ,white-1 :bold t))))
   `(markdown-math-face ((,class (:foreground ,magenta-1))))
   `(markdown-missing-link-face ((,class (:foreground ,red-1 :bold t))))
   `(markdown-pre-face ((,class (:foreground ,blue-2))))
   `(markdown-reference-face ((,class (:foreground ,orange-2 :italic t))))
   `(markdown-url-face ((,class (:foreground ,magenta-3 :underline t))))

   ;; Twittering-mode
   `(twittering-username-face ((,class (:foreground ,blue-1 :bold t))))
   `(twittering-uri-face ((,class (:foreground ,blue-1 :bold t))))
   `(twittering-timeline-footer-face ((,class (:foreground ,black-1 :bold t))))
   `(twittering-timeline-header-face ((,class (:foreground ,black-1 :bold t))))

   ;; popup
   `(popup-face ((,class (:background ,white-2 :foreground ,black-3))))
   `(popup-isearch-match ((,class (:background ,cyan-1 :foreground ,black-3))))
   `(popup-menu-face ((,class (:background ,white-2 :foreground ,black-3))))
;;   `(popup-menu-mouse-face ((,class (:background ,white-2 :foreground ,black-3))))
;;   `(popup-menu-selection-face ((,class (:background ,white-2 :foreground ,black-3))))
;;   `(popup-menu-summary-face ((,class (:background ,white-2 :foreground ,black-3))))
   `(popup-scroll-bar-background-face ((,class (:background ,white-3 :foreground ,white-3))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,black-2 :foreground ,black-2))))
;;   `(popup-summary-face ((,class (:background ,white-2 :foreground ,black-3))))
   `(popup-tip-face ((,class (:background ,yellow-0 :foreground ,black-3))))
   
   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue-1))))
   `(link-visited ((,class (:underline t :foreground ,blue-2))))

   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:foreground ,purple-2))))
   `(gnus-group-news-1-low ((,class (:foreground ,purple-2))))
   `(gnus-group-news-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-news-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-news-3 ((,class (:foreground ,green-2))))
   `(gnus-group-news-3-low ((,class (:foreground ,green-2))))
   `(gnus-group-news-4 ((,class (:foreground ,purple-2))))
   `(gnus-group-news-4-low ((,class (:foreground ,magenta-2))))
   `(gnus-group-news-5 ((,class (:foreground ,orange-1))))
   `(gnus-group-news-5-low ((,class (:foreground ,orange-2))))
   `(gnus-group-news-low ((,class (:foreground ,yellow-2))))
   `(gnus-group-mail-1 ((,class (:foreground ,purple-2))))
   `(gnus-group-mail-1-low ((,class (:foreground ,purple-2))))
   `(gnus-group-mail-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-mail-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-mail-3 ((,class (:foreground ,green-2))))
   `(gnus-group-mail-3-low ((,class (:foreground ,green-2))))
   `(gnus-group-mail-low ((,class (:foreground ,yellow-2))))
   `(gnus-header-content ((,class (:weight normal :foreground ,yellow-3))))
   `(gnus-header-from ((,class (:foreground ,yellow-2))))
   `(gnus-header-subject ((,class (:foreground ,green-2))))
   `(gnus-header-name ((,class (:foreground ,blue-1))))
   `(gnus-header-newsgroups ((,class (:foreground ,magenta-2))))

   ;; Helm
   `(helm-selection ((,class (:foreground ,black-3 :background ,green-01))))
   `(helm-bookmark-directory ((,class (:foreground ,blue-1 :background ,LIGHT_BG :bold t))))
   `(helm-bookmark-file ((,class (:foreground ,black-5 :background ,LIGHT_BG))))
   `(helm-ff-directory ((,class (:foreground ,blue-1 :background ,LIGHT_BG :bold t))))
   `(helm-ff-file ((,class (:foreground ,black-5 :background ,LIGHT_BG))))
   `(helm-ff-excutable ((,class (:foreground ,green-2 :background ,LIGHT_BG :bold t))))
   `(helm-source-header ((,class (:foreground ,black-3 :background ,blue-1 :bold t))))
   `(helm-header ((,class (:foreground ,blue-3 :background ,white-1))))
   `(helm-candidate-number ((,class (:foreground ,blue-3 :background ,white-1))))
   `(helm-match ((,class (:foreground ,green-2 :background ,LIGHT_BG :bold t))))

   ;; Message faces
   `(message-header-name ((,class (:foreground ,blue-3))))
   `(message-header-cc ((,class (:foreground ,yellow-3))))
   `(message-header-other ((,class (:foreground ,magenta-2))))
   `(message-header-subject ((,class (:foreground ,green-2))))
   `(message-header-to ((,class (:foreground ,yellow-2))))
   `(message-cited-text ((,class (:foreground ,green-2))))
   `(message-separator ((,class (:foreground ,purple-2))))

   ;; SMerge faces
   `(smerge-refined-change ((,class (:background ,blue-3))))

   ;; Grep
   `(grep-context-face ((,class (:foreground ,red-2))))
   `(grep-error-face ((,class (:foreground ,red-1 :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,green-2))))
   `(grep-match-face ((,class (:foreground nil :background nil :inherit match))))

   ;; Ediff faces
   `(ediff-current-diff-A ((,class (:background ,black-2))))
   `(ediff-fine-diff-A ((,class (:background ,blue-3))))
   `(ediff-even-diff-A ((,class (:background ,black-4))))
   `(ediff-odd-diff-A ((,class (:background ,black-4))))
   `(ediff-current-diff-B ((,class (:background ,black-2))))
   `(ediff-fine-diff-B ((,class (:background ,magenta-3))))
   `(ediff-even-diff-B ((,class (:background ,black-4))))
   `(ediff-odd-diff-B ((,class (:background ,black-4))))

   ;; Flyspell faces
   `(flyspell-duplicate ((,class (:underline t foreground-color,orange-1))))
   `(flyspell-incorrect ((,class (:background ,red-1 :foreground ,black-5 :bold t))))

   ;; Semantic faces
   `(semantic-decoration-on-includes ((,class (:underline ,black-1))))
   `(semantic-decoration-on-private-members-face
     ((,class (:background ,purple-3))))
   `(semantic-decoration-on-protected-members-face
     ((,class (:background ,magenta-3))))
   `(semantic-decoration-on-unknown-includes
     ((,class (:background ,red-3))))
   `(semantic-decoration-on-unparsed-includes
     ((,class (:background ,black-4))))
   `(semantic-tag-boundary-face ((,class (:overline ,blue-1))))
   `(semantic-unmatched-syntax-face ((,class (:underline ,red-1))))

   ;; Flymake
   `(flymake-warnline ((,class (:underline ,orange-1))))
   `(flymake-errline ((,class (:underline ,red-2)))))

  (custom-theme-set-variables
   'moe-light
   `(ansi-color-names-vector [,black-5 ,red-0 ,green-0 ,yellow-1
			      ,blue-1 ,purple-1 ,blue-0 ,white-1])))


(provide-theme 'moe-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; moe-light-theme.el ends here


