;;; moe-light-theme.el --- An eye-candy theme for Emacser

;; Author: kuanyui<azazabc123@gmail.com>
;; Based on "tango-dark-theme" , a part of GNU Emacs 24

;;; Code:

(deftheme moe-light
  "Face colors for 256 colors terminal (dark background).
Moe, moe, kyun!")

(let ((class '((class color) (min-colors 89)))
      ;; Palette colors.
      (yellow-1 "#fce94f") (yellow-2 "#ffd700") (yellow-3 "#c4a000") (yellow-4 "#875f00")
      (orange-1 "#ffaf5f") (orange-2 "#ff8700") (orange-3 "#ff5d17") (orange-4 "#d75f00")
      (magenta-1 "#ff7bbb") (magenta-2 "#ff4ea3") (magenta-3 "#ff1f8b")
      (green-1 "#afff00") (green-2 "#a1db00") (green-3 "#00af00") (green-4 "#008700") (green-5 "#005f00")
      (blue-1 "#729fcf") (blue-2 "#1f5bff") (blue-3 "#005f87") (blue-4 "#005faf") (blue-5 "#0000af") 
      (cyan-1 "#87ffff") (cyan-2 "#00ffff") (cyan-3 "#00d7af") (cyan-4 "#5f87af")
      (purple-1 "#d18aff") (purple-2 "#9a08ff") (purple-3 "#6c0099")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (white-1 "#eeeeee") (white-2 "#dadada") (white-3 "#c6c6c6") (white-4 "#b2b2b2") (LIGHT_BG "#ffffd7")
      (black-1 "#9e9e9e") (black-2 "#8a8a8a") (black-3 "#767676")

      (green-01 "#d7ff00")
      (green-0 "#d7ff5f") (blue-0 "#afd7ff") (purple-0 "#e6a8df") (yellow-0 "#ffff87") (white-0 "#ffffff")
      (red-0 "#ff4b4b")  (black-4 "#626262") (black-5 "#5f5f5f") (black-6 "#3a3a3a") (orange-0 "#ffaf87")
      (green-00 "#d7ff87") (yellow-00 "#ffffaf") (blue-00 "#d7d7ff"))

  (custom-theme-set-faces
   'moe-light
   ;; Ensure sufficient contrast on low-color terminals.
   `(default ((((class color) (min-colors 4096))
	       (:foreground ,black-5 :background ,LIGHT_BG))
	      (((class color) (min-colors 256))
	       (:foreground ,black-5 :background ,LIGHT_BG))
	      (,class
	       (:foreground ,black-5 :background ,LIGHT_BG))))
   `(cursor ((,class (:background ,black-5))))

   ;; Highlighting faces
   `(fringe ((,class (:foreground ,black-1 :background "#d7d7af"))))
   `(linum ((,class (:foreground "#87875f" :background "#d7d7af"))))
   `(highlight ((,class (:background ,green-0))))
   `(hl-line ((,class (:background ,green-00)))) ; Embedded strings of IM (e.g. fcitx) would be very unreadable... orz
   `(region ((,class (:foreground ,white-0 :background ,blue-1))))
   `(secondary-selection ((,class (:background ,blue-3 :foreground ,white-0))))
   `(isearch ((,class (:foreground ,white-0 :background ,orange-3))))
   `(lazy-highlight ((,class (:background ,magenta-3 :foreground ,white-0))))
   `(trailing-whitespace ((,class (:background ,red-3))))
   `(show-paren-match ((,class (:background ,blue-00))))
   ;; Mode line & frames' faces
   `(mode-line ((,class
		 (:box (:line-width -1 :style nil)
		  :background ,blue-1 :foreground ,white-0))))
   `(mode-line-inactive ((,class
			  (:box (:line-width -1 :style nil)
			   :background ,white-2 :foreground ,black-1))))
   `(mode-line-buffer-id ((,class (:foreground ,black-6 :background ,blue-1 :bold t))))
   `(vertical-border ((,class (:foreground "#d7d7af" :background "#d7d7af"))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,black-6 :background ,green-2))))
   `(escape-glyph ((,class (:foreground ,yellow-3))))
   `(error ((,class (:foreground ,red-0))))
   `(warning ((,class (:foreground ,orange-1))))
   `(success ((,class (:foreground ,green-2))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,purple-2))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,red-2 :slant italic))))
   `(font-lock-comment-face ((,class (:foreground ,red-2 :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,blue-2))))
   `(font-lock-doc-face ((,class (:foreground ,red-2))))
   `(font-lock-doc-string-face ((,class (:foreground ,yellow-3))))
   `(font-lock-function-name-face ((,class (:foreground ,orange-2))))
   `(font-lock-keyword-face ((,class (:foreground ,green-3))))
   `(font-lock-negation-char-face ((,class (:foreground ,green-3))))
   `(font-lock-preprocessor-face ((,class (:foreground ,purple-2))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,orange-2))))
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
   `(org-todo ((,class (:bold t :weight bold :foreground ,red-3 :background ,orange-0
                              :box (:line-width 1 :style none)))))
   `(org-level-1 ((,class (:bold t :foreground ,blue-1 :height 1.3))))
   `(org-level-2 ((,class (:bold t :foreground ,green-2 :height 1.2))))
   `(org-level-3 ((,class (:bold t :foreground ,orange-2 :height 1.1))))
   `(org-level-4 ((,class (:bold t :foreground ,cyan-3))))
   `(org-level-5 ((,class (:bold t :foreground ,red-2))))
   `(org-level-6 ((,class (:bold t :foreground ,magenta-2))))
   `(org-level-7 ((,class (:bold t :foreground ,purple-2))))
   `(org-level-8 ((,class (:bold t :foreground ,yellow-2))))
   `(org-tag ((,class (:background ,black-1 :foreground ,white-0 :bold t :weight bold))))
   `(org-column ((,class (:background ,white-4 :foreground ,black-5))))
   `(org-column-title ((,class (:background ,green-2 :foreground ,black-5 :underline t :weight bold))))
   `(org-deadline-announce ((,class (:foreground ,red-3))))
   `(org-scheduled ((,class (:foreground ,green-3))))
   `(org-scheduled-previously ((,class (:foreground ,red-1))))
   `(org-scheduled-today ((,class (:foreground ,blue-2))))
   `(org-special-keyword ((,class (:background ,yellow-2 :foreground ,yellow-4))))
   `(org-table ((,class (:background ,yellow-0 :foreground ,black-3))))
   `(org-time-grid ((,class (:foreground ,orange-2))))
   `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   `(org-warning ((,class (:bold t :foreground ,red-3 :weight bold))))
   `(org-formula ((,class (:foreground ,purple-2))))
   `(org-headline-done ((,class (:foreground ,green-2))))
   `(org-hide ((,class (:foreground ,black-2))))
   `(org-code ((,class (:foreground ,blue-2))))
   `(org-link ((,class (:foreground ,blue-2 :underline ,blue-2))))
   `(org-footnote ((,class (:foreground ,magenta-3))))
   `(org-ellipsis ((,class (:foreground ,red-2))))
   `(org-agenda-clocking ((,class (:foreground ,blue-3 :background ,blue-0 :bold t))))
   `(org-agenda-date ((,class (:foreground ,blue-2 :underline nil))))
   `(org-agenda-todo ((,class (:foreground ,white-0 :background ,red-2))))
   `(org-agenda-done ((,class (:foreground ,black-3 :background ,green-2))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,white-0 :background ,red-2))))
   `(org-block ((,class (:foreground ,orange-1))))
   `(org-quote ((,class (:foreground ,orange-1))))
   `(org-block-begin-line ((,class (:foreground ,orange-2))))
   `(org-block-end-line ((,class (:foreground ,orange-2))))
   `(org-mode-line-clock ((,class (:foreground ,blue-3 :background ,blue-0 :bold t))))
   `(org-mode-line-clock-overrun ((,class (:foreground ,white-0 :background ,red-1 :bold t))))


   ;; undo-tree
   `(undo-tree-visualizer-default-face ((,class (:foreground ,black-5))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,green-2 :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red-2))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow-2))))

   ;; Markdown-mode
   `(markdown-blockquote-face ((,class (:foreground ,green-3 :background ,green-00 :italic t))))
   `(markdown-bold-face ((,class (:foreground ,black-5 :bold t))))
   `(markdown-comment-face ((,class (:foreground ,white-4 :italic t))))
   `(markdown-header-delimiter-face ((,class (:foreground ,orange-3 :bold t))))
   `(markdown-header-face ((,class (:foreground ,orange-2 :bold t))))
   `(markdown-header-rule-face ((,class (:foreground ,orange-2 :bold t))))
   `(markdown-header-face-1 ((,class (:foreground ,orange-2 :bold t :height 2.0))))
   `(markdown-header-face-2 ((,class (:foreground ,orange-2 :bold t :height 1.5))))
   `(markdown-header-face-3 ((,class (:foreground ,orange-2 :bold t :height 1.2))))
   `(markdown-header-face-4 ((,class (:foreground ,orange-2 :bold t))))
   `(markdown-header-face-5 ((,class (:foreground ,orange-2 :bold t))))
   `(markdown-header-face-6 ((,class (:foreground ,orange-2 :bold t))))
   `(markdown-link-face ((,class (:foreground ,magenta-1 :bold t :underline ,magenta-1))))
   `(markdown-inline-code-face ((,class (:foreground ,blue-3 :background ,blue-00))))
   `(markdown-italic-face ((,class (:foreground ,black-5 :italic t :underline ,black-5))))
   `(markdown-list-face ((,class (:foreground ,orange-2 :background ,LIGHT_BG :bold t))))
   `(markdown-math-face ((,class (:foreground ,purple-2))))
   `(markdown-missing-link-face ((,class (:foreground ,red-1 :bold t))))
   `(markdown-pre-face ((,class (:foreground ,blue-2))))
   `(markdown-reference-face ((,class (:foreground ,orange-2 :italic t))))
   `(markdown-url-face ((,class (:foreground ,magenta-3 :underline ,magenta-3))))

   ;; Twittering-mode
   `(twittering-username-face ((,class (:foreground ,blue-3 :background ,blue-00))))
   `(twittering-uri-face ((,class (:foreground ,blue-2 :underline ,blue-2))))
   `(twittering-timeline-footer-face ((,class (:foreground ,black-1 :bold t))))
   `(twittering-timeline-header-face ((,class (:foreground ,black-1 :bold t))))

   ;; popup
   `(popup-face ((,class (:background ,blue-00 :foreground ,blue-3))))
   `(popup-isearch-match ((,class (:background ,cyan-1 :foreground ,black-3))))
   `(popup-menu-face ((,class (:background ,blue-00 :foreground ,blue-3))))
   `(popup-menu-selection-face ((,class (:background ,blue-1 :foreground ,white-0))))
   `(popup-menu-summary-face ((,class (:background ,blue-00 :foreground ,blue-3))))
   `(popup-scroll-bar-background-face ((,class (:background ,blue-0 :foreground ,blue-0))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,blue-3 :foreground ,blue-3))))
   `(popup-summary-face ((,class (:background ,blue-00 :foreground ,blue-3))))
   `(popup-tip-face ((,class (:background ,yellow-00 :foreground ,black-3))))

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
   `(helm-bookmark-directory ((,class (:foreground ,blue-1 :background ,black-5 :bold t))))
   `(helm-bookmark-file ((,class (:foreground ,yellow-4 :background ,yellow-0))))
   `(helm-bookmark-info ((,class (:foreground ,green-4 :background ,green-0))))
   `(helm-ff-directory ((,class (:foreground ,blue-1 :background ,LIGHT_BG :bold t))))
   `(helm-ff-file ((,class (:foreground ,black-5 :background ,LIGHT_BG))))
   `(helm-ff-excutable ((,class (:foreground ,green-2 :background ,LIGHT_BG :bold t))))
   `(helm-source-header ((,class (:foreground ,black-4 :background ,blue-1 :bold t))))
   `(helm-header ((,class (:foreground ,blue-3 :background ,white-0))))
   `(helm-candidate-number ((,class (:foreground ,blue-3 :background ,white-0))))
   `(helm-match ((,class (:foreground ,green-2 :background ,LIGHT_BG :bold t))))
   `(helm-buffer-saved-out ((,class (:foreground ,red-2 :background ,LIGHT_BG))))
   `(helm-ff-invalid-symlink ((,class (:foreground ,white-0 :background ,red-2))))
   `(helm-ff-prefix ((,class (:foreground ,white-0 :background ,orange-2))))
   `(helm-buffer-size ((,class (:foreground ,orange-2))))

   ;; Dired+
   `(dired-directory ((,class (:foreground ,blue-1 :bold t))))
   `(dired-flagged ((,class (:foreground ,red-1))))
   `(dired-header ((,class (:foreground ,blue-1))))
   `(dired-ignored ((,class (:foreground ,white-4))))
   `(dired-mark ((,class (:foreground ,green-1))))
   `(dired-marked ((,class (:foreground ,green-2))))
   `(dired-perm-write ((,class (:foreground ,red-2))))
   `(dired-symlink ((,class (:foreground ,magenta-2))))
   `(dired-warning ((,class (:foreground ,white-0 :background ,red-1 :bold t))))
   `(diredp-compressed-file-suffix ((,class (:foreground ,purple-2))))
   `(diredp-date-time ((,class (:foreground ,blue-3 :background ,blue-00))))
   `(diredp-deletion ((,class (:foreground ,white-0, :background ,red-3))))
   `(diredp-deletion-file-name ((,class (:foreground ,red-2))))
   `(diredp-dir-heading ((,class (:foreground ,black-5 :background ,green-01 :bold t))))
   `(diredp-dir-priv ((,class (:foreground ,blue-1 :background ,LIGHT_BG :bold t))))
   `(diredp-display-msg ((,class (:foreground ,orange-2))))
   `(diredp-executable-tag ((,class (:foreground ,green-2))))
   `(diredp-file-name ((,class (:foreground ,black-5))))
   `(diredp-file-suffix ((,class (:foreground ,orange-3))))
   `(diredp-flag-mark ((,class (:foreground ,white-0 :background ,magenta-3 :bold t))))
   `(diredp-flag-mark-line ((,class (:foreground ,black-5 :background ,magenta-1))))
   `(diredp-ignored-file-name ((,class (:foreground ,white-4))))
   `(diredp-link-priv ((,class (:foreground ,magenta-3))))
   `(diredp-mode-line-flagged ((,class (:foreground ,black-5 :background ,green-2))))
   `(diredp-mode-line-marked ((,class (:foreground ,white-0 :background ,magenta-3 bold t))))
   `(diredp-no-priv ((,class (:foreground ,yellow-3 :background ,yellow-00))))
   `(diredp-number ((,class (:foreground ,green-3))))
   `(diredp-other-priv ((,class (:foreground ,white-0 :background ,blue-3))))
   `(diredp-rare-priv ((,class (:foreground ,white-0 :background ,purple-2))))
   `(diredp-symlink ((,class (:foreground ,magenta-3))))
   `(diredp-read-priv ((,class (:foreground ,green-4 :background ,green-0))))
   `(diredp-write-priv ((,class (:foreground ,blue-5 :background ,blue-0))))
   `(diredp-exec-priv ((,class (:foreground ,red-3 :background ,orange-0))))

   ;; Magit
   `(magit-branch ((,class (:foreground ,blue-2 :background ,blue-0 :bold t))))
   `(magit-diff-add ((,class (:foreground ,green-3 :background ,LIGHT_BG :bold t))))
   `(magit-diff-del ((,class (:foreground ,red-2 :background ,LIGHT_BG :bold t))))
   `(magit-diff-file-header ((,class (:foreground ,black-4 :background ,white-3 :bold t))))
   `(magit-diff-hunk-header ((,class (:foreground ,black-2 :background ,white-1 :bold t))))
   `(magit-diff-merge-current ((,class (:foreground ,purple-1))))
   `(magit-diff-merge-diff3-separator ((,class (:foreground ,purple-1))))
   `(magit-diff-merge-proposed ((,class (:foreground ,purple-1))))
   `(magit-diff-merge-separator ((,class (:foreground ,purple-1))))
   `(magit-diff-none ((,class (:foreground ,white-4))))
   `(magit-header ((,class (:foreground ,blue-2 :background ,white-0 :underline ,blue-2))))
   `(magit-item-highlight ((,class (:background ,yellow-00))))
   `(magit-item-mark ((,class (:foreground ,white-0 :background ,magenta-2))))
   `(magit-log-author ((,class (:foreground ,green-3 :background ,LIGHT_BG))))
   `(magit-log-author-date-cutoff ((,class (:foreground ,red-1 :bold t))))
   `(magit-log-date ((,class (:foreground ,black-5 :background ,green-0))))
   `(magit-log-graph ((,class (:foreground ,black-2 :background ,LIGHT_BG))))
   `(magit-log-head-label-bisect-bad ((,class (:foreground ,white-1 :background ,red-0))))
   `(magit-log-head-label-bisect-good ((,class (:foreground ,black-3 :background ,green-0))))
   `(magit-log-head-label-default ((,class (:foreground ,white-0 :background ,black-3 :bold t))))
   `(magit-log-head-label-local ((,class (:foreground ,white-0 :background ,black-3 :bold t))))
   `(magit-log-head-label-patches ((,class (:foreground ,red-3 :background ,orange-0 :bold t))))
   `(magit-log-head-label-remote ((,class (:foreground ,green-3 :background ,green-0 :bold t))))
   `(magit-log-head-label-tags ((,class (:foreground ,yellow-4 :background ,yellow-0 :bold t))))
   `(magit-log-message ((,class (:foreground ,black-5 :background ,LIGHT_BG))))
   `(magit-log-sha1 ((,class (:foreground ,orange-2 :background ,LIGHT_BG))))
   `(magit-section-title ((,class (:foreground ,orange-3 :background ,LIGHT_BG :underline ,orange-3 :bold ,t))))
   `(magit-valid-signature ((,class (:foreground ,cyan-3 :background ,LIGHT_BG :bold t))))
   `(magit-whitespace-warning-face ((,class (:foreground ,white-0 :background ,red-2 :bold t))))
   
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
   `(ediff-current-diff-A ((,class (:background ,magenta-1))))
   `(ediff-fine-diff-A ((,class (:background ,green-1))))
   `(ediff-even-diff-A ((,class (:background ,yellow-0))))
   `(ediff-odd-diff-A ((,class (:background ,orange-0))))
   `(ediff-current-diff-B ((,class (:background ,magenta-1))))
   `(ediff-fine-diff-B ((,class (:background ,green-1))))
   `(ediff-even-diff-B ((,class (:background ,yellow-0))))
   `(ediff-odd-diff-B ((,class (:background ,orange-0))))

   ;; smerge
   `(smerge-refined-change ((,class (:background ,blue-3 :foreground ,white-0))))
   
   ;; Flyspell faces
   `(flyspell-duplicate ((,class (:underline t foreground-color,orange-1))))
   `(flyspell-incorrect ((,class (:background ,red-1 :foreground ,white-0 :bold t))))

   ;; Semantic faces
   `(semantic-decoration-on-includes ((,class (:underline ,black-1))))
   `(semantic-decoration-on-private-members-face
     ((,class (:background ,purple-0))))
   `(semantic-decoration-on-protected-members-face
     ((,class (:background ,magenta-1))))
   `(semantic-decoration-on-unknown-includes
     ((,class (:background ,red-0))))
   `(semantic-decoration-on-unparsed-includes
     ((,class (:background ,yellow-1))))
   `(semantic-tag-boundary-face ((,class (:overline ,blue-1))))
   `(semantic-unmatched-syntax-face ((,class (:underline ,red-1))))

   ;; CUA
   `(cua-rectangle ((,class (:background ,magenta-3 :foreground ,white-0))))

   ;; Ace-jump
   `(ace-jump-face-background ((,class (:foreground ,white-4))))
   `(ace-jump-face-foreground ((,class (:foreground ,red-2 :bold t))))

   ;; Rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:bold t :foreground ,red-2))))
   `(rainbow-delimiters-depth-2-face ((,class (:bold t :foreground ,blue-2))))
   `(rainbow-delimiters-depth-3-face ((,class (:bold t :foreground ,green-3))))
   `(rainbow-delimiters-depth-4-face ((,class (:bold t :foreground ,magenta-2))))
   `(rainbow-delimiters-depth-5-face ((,class (:bold t :foreground ,cyan-3))))
   `(rainbow-delimiters-depth-6-face ((,class (:bold t :foreground ,orange-2))))
   `(rainbow-delimiters-depth-7-face ((,class (:bold t :foreground ,purple-2))))
   `(rainbow-delimiters-depth-8-face ((,class (:bold t :foreground ,yellow-2))))
   `(rainbow-delimiters-depth-9-face ((,class (:bold t :foreground ,black-5))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,white-0 :background ,red-3 :bold t))))

   ;; which-function-mode
   `(which-func ((,class (:foreground ,white-0 :background ,orange-2))))

   ;; Flymake
   `(flymake-warnline ((,class (:underline ,orange-2))))
   `(flymake-errline ((,class (:underline ,red-2)))))

  (custom-theme-set-variables
   'moe-light
   `(ansi-color-names-vector [,black-5 ,red-0 ,green-0 ,yellow-1
			      ,blue-1 ,purple-1 ,blue-0 ,white-0])))
;; fix wrong default face
(if window-system
    (progn 
      (set-background-color "#ffffd7")
      (set-foreground-color "#5f5f5f")))

(provide-theme 'moe-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; moe-light-theme.el ends here


