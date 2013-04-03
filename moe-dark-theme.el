;;; moe-dark-theme.el --- Tango-based custom theme for faces

;; Author: kuanyui<azazabc123@gmail.com>
;; Based on "tango-dark-theme" , a part of GNU Emacs 24
;;	    Chong Yidong <cyd@stupidchicken>
;;          Jan Moringen <jan.moringen@uni-bielefeld.de>


;;; Code:

(deftheme moe-dark
  "Face colors for 256 colors terminal (dark background).
Moe, moe, chew!")

(let ((class '((class color) (min-colors 89)))
      ;; Palette colors.
      (yellow-1 "#fce94f") (yellow-2 "#ffd700") (yellow-3 "#c4a000")
      (orange-1 "#ff9742") (orange-2 "#ff5d17") (orange-3 "#e52900")
      (magenta-1 "#ff7bbb") (magenta-2 "#ff4ea3") (magenta-3 "#ff1f8b")
      (green-1 "#b9ff31") (green-2 "#a1db00") (green-3 "#4e9a06")
      (blue-1 "#729fcf") (blue-2 "#1f5bff") (blue-3 "#204a87")
      (cyan-1 "#87ffff") (cyan-2 "#00ffff") (cyan-3 "#00d7af")
      (purple-1 "#d18aff") (purple-2 "#9a08ff") (purple-3 "#6c0099")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (white-1 "#ffffff") (white-2 "#d3d3d3") (white-3 "#bcbcbc")
      (black-1 "#999999") (black-2 "#555555") (black-3 "#303030")
      ;; Not in palette; used for better contrast.
      (green-0 "#c1ff57") (blue-0 "#8cc4ff") (purple-0 "#e6a8df")
      (red-0 "#ff4b4b")  (black-4 "#41423f") (black-5 "#1c1c1c"))

  (custom-theme-set-faces
   'moe-dark
   ;; Ensure sufficient contrast on low-color terminals.
   `(default ((((class color) (min-colors 4096))
	       (:foreground ,white-1 :background ,black-5))
	      (((class color) (min-colors 256))
	       (:foreground ,white-1 :background ,black-5))
	      (,class
	       (:foreground ,white-1 :background "black"))))
   `(cursor ((,class (:background ,white-1))))
   ;; Highlighting faces
   `(fringe ((,class (:foreground ,black-3 :background ,white-3))))
   `(linum ((,class (:foreground ,white-1 :background ,black-2))))
   `(highlight ((,class (:foreground ,white-1 :background ,black-3))))
   `(region ((,class (:foreground ,black-3 :background ,green-2))))
   `(secondary-selection ((,class (:background ,blue-3))))
   `(isearch ((,class (:foreground ,white-1 :background ,orange-3))))
   `(lazy-highlight ((,class (:background ,magenta-3))))
   `(trailing-whitespace ((,class (:background ,red-3))))
   ;; Mode line & frames' faces
   `(mode-line ((,class
		 (:box (:line-width -1 :style released-button)
		  :background ,white-1 :foreground ,blue-3))))
   `(mode-line-inactive ((,class
			  (:box (:line-width -1 :style released-button)
			   :background ,black-3 :foreground ,white-1))))
   `(mode-line-buffer-id ((,class (:foreground ,black-3 :background ,blue-1))))
   `(vertical-border ((,class (:foreground ,white-1 :background ,white-1))))
;;   `(mode-line-highlight ((,class (:foreground ,blue-2 :background ,white-1))))
;;org-mode
   `(org-document-title ((,class (:foreground ,blue-1 :background ,black-5 :weight bold :height 1.5))))
   `(org-document-info ((,class (:foreground ,blue-3 :background ,black-5 :weight bold))))
   `(org-document-info-keyword ((,class (:foreground ,white-1 :background ,black-5))))
   `(org-agenda-date-today
     ((,class (:foreground ,orange-2 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:slant italic))))
   `(org-checkbox ((,class (:background ,white-2 :foreground ,black-3
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,blue-2 :underline t))))
   `(org-done ((,class (:bold t :weight bold :foreground ,black-3 :background ,green-2
                              :box (:line-width 1 :style none)))))
   `(org-todo ((,class (:bold t :weight bold :foreground ,black-3 :background ,red-1
                              :box (:line-width 1 :style none)))))
   `(org-level-1 ((,class (:foreground ,blue-1 :height 1.3))))
   `(org-level-2 ((,class (:foreground ,green-2 :height 1.2))))
   `(org-level-3 ((,class (:foreground ,orange-2 :height 1.1))))
   `(org-level-4 ((,class (:foreground ,yellow-2))))
   `(org-level-5 ((,class (:foreground ,red-2))))
   `(org-level-6 ((,class (:foreground ,magenta-2))))
   `(org-level-7 ((,class (:foreground ,purple-2))))
   `(org-level-8 ((,class (:foreground ,black-2))))
   `(org-link ((,class (:foreground ,blue-1 :underline t))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-column ((,class (:background ,white-3 :foreground ,black-5))))
   `(org-column-title ((,class (:background ,green-2 :foreground ,black-5 :underline t :weight bold))))
   `(org-deadline-announce ((,class (:foreground ,red-3))))
   `(org-scheduled ((,class (:foreground ,green-2))))
   `(org-scheduled-previously ((,class (:foreground ,red-1))))
   `(org-scheduled-today ((,class (:foreground ,blue-1))))
   `(org-special-keyword ((,class (:foreground ,yellow-2))))
   `(org-table ((,class (:foreground ,green-2))))
   `(org-time-grid ((,class (:foreground ,orange-2))))
   `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   `(org-warning ((,class (:bold t :foreground ,red-3 :weight bold :underline nil))))
   `(org-formula ((,class (:foreground ,yellow-2))))
   `(org-headline-done ((,class (:foreground ,green-2))))
   `(org-hide ((,class (:foreground ,black-2))))
   `(org-code ((,class (:foreground ,yellow-1))))
   `(org-link ((,class (:foreground ,blue-1))))
   `(org-footnote ((,class (:foreground ,magenta-3))))
   `(org-ellipsis ((,class (:foreground ,red-2))))
   `(org-agenda-date ((,class (:foreground ,blue-2 :underline nil))))
   `(org-agenda-todo ((,class (:foreground ,black-3 :background ,red-2))))
   `(org-agenda-done ((,class (:foreground ,black-3 :background ,green-2))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,black-3 :background ,red-3))))
   `(org-block ((,class (:foreground ,orange-1))))


   ;; undo-tree
     `(undo-tree-visualizer-default-face ((,class (:foreground ,black-5))))
     `(undo-tree-visualizer-current-face ((,class (:foreground ,green-2 :weight bold))))
     `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red-2))))
     `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow-2))))


   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,black-3 :background ,green-2))))
   `(escape-glyph ((,class (:foreground ,yellow-3))))
   `(error ((,class (:foreground ,red-0))))
   `(warning ((,class (:foreground ,orange-1))))
   `(success ((,class (:foreground ,green-1))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,purple-1))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,red-2 :slant italic))))
   `(font-lock-comment-face ((,class (:foreground ,red-2))))
   `(font-lock-constant-face ((,class (:foreground ,blue-1))))
   `(font-lock-doc-face ((,class (:foreground ,red-2))))
   `(font-lock-doc-string-face ((,class (:foreground ,yellow-1))))
   `(font-lock-function-name-face ((,class (:foreground ,yellow-1))))
   `(font-lock-keyword-face ((,class (:foreground ,green-1))))
   `(font-lock-negation-char-face ((,class (:foreground ,green-2))))
   `(font-lock-preprocessor-face ((,class (:foreground ,purple-1))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow-1))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple-1))))
   `(font-lock-string-face ((,class (:foreground ,magenta-1))))
   `(font-lock-type-face ((,class (:foreground ,blue-1))))
   `(font-lock-variable-name-face ((,class (:foreground ,orange-2))))
   `(font-lock-warning-face ((,class (:weight bold :foreground ,red-2))))

   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue-1))))
   `(link-visited ((,class (:underline t :foreground ,blue-2))))

   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:foreground ,purple-1))))
   `(gnus-group-news-1-low ((,class (:foreground ,purple-2))))
   `(gnus-group-news-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-news-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-news-3 ((,class (:foreground ,green-1))))
   `(gnus-group-news-3-low ((,class (:foreground ,green-2))))
   `(gnus-group-news-4 ((,class (:foreground ,purple-0))))
   `(gnus-group-news-4-low ((,class (:foreground ,magenta-2))))
   `(gnus-group-news-5 ((,class (:foreground ,orange-1))))
   `(gnus-group-news-5-low ((,class (:foreground ,orange-2))))
   `(gnus-group-news-low ((,class (:foreground ,yellow-2))))
   `(gnus-group-mail-1 ((,class (:foreground ,purple-1))))
   `(gnus-group-mail-1-low ((,class (:foreground ,purple-2))))
   `(gnus-group-mail-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-mail-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-mail-3 ((,class (:foreground ,green-1))))
   `(gnus-group-mail-3-low ((,class (:foreground ,green-2))))
   `(gnus-group-mail-low ((,class (:foreground ,yellow-2))))
   `(gnus-header-content ((,class (:weight normal :foreground ,yellow-3))))
   `(gnus-header-from ((,class (:foreground ,yellow-2))))
   `(gnus-header-subject ((,class (:foreground ,green-1))))
   `(gnus-header-name ((,class (:foreground ,blue-1))))
   `(gnus-header-newsgroups ((,class (:foreground ,magenta-2))))
   ;; Message faces
   `(message-header-name ((,class (:foreground ,blue-1))))
   `(message-header-cc ((,class (:foreground ,yellow-3))))
   `(message-header-other ((,class (:foreground ,magenta-2))))
   `(message-header-subject ((,class (:foreground ,green-1))))
   `(message-header-to ((,class (:foreground ,yellow-2))))
   `(message-cited-text ((,class (:foreground ,green-1))))
   `(message-separator ((,class (:foreground ,purple-1))))
   ;; SMerge faces
   `(smerge-refined-change ((,class (:background ,blue-3))))
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
   `(flyspell-duplicate ((,class (:underline ,orange-1))))
   `(flyspell-incorrect ((,class (:background ,red-1 :foreground ,white-1))))
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
   `(flymake-errline ((,class (:underline ,red-2))))
   ;; Clojure errors
   `(clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
   `(clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
   `(clojure-test-success-face ((,class (:background nil :foreground nil :underline ,green-2)))))

  (custom-theme-set-variables
   'moe-dark
   `(ansi-color-names-vector [,black-5 ,red-0 ,green-0 ,yellow-1
			      ,blue-1 ,purple-1 ,blue-0 ,white-1])))



(provide-theme 'moe-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; moe-dark-theme.el ends here


