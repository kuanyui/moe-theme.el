;;; moe-light-theme.el --- An eye-candy theme for Emacser

;; Author: kuanyui<azazabc123@gmail.com>
;; Based on "tango-dark-theme"

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
      (purple-1 "#d18aff") (purple-2 "#9a08ff") (purple-3 "#6c0099") (purple-4 "#6c0099")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (white-1 "#eeeeee") (white-2 "#dadada") (white-3 "#c6c6c6") (white-4 "#b2b2b2") (LIGHT_BG "#fdfde7")
      (black-1 "#9e9e9e") (black-2 "#8a8a8a") (black-3 "#767676")

      (green-01 "#d7ff00") (green-02 "#87ff87") (white-0 "#ffffff")
      (green-0 "#d7ff5f") (blue-0 "#afd7ff") (purple-0 "#af87ff") (yellow-0 "#ffff87") (white-0 "#ffffff")
      (red-0 "#ff4b4b") (red-00 "#ffafaf") (black-4 "#626262") (black-5 "#5f5f5f") (black-6 "#3a3a3a") (orange-0 "#ffaf87") (magenta-0 "#ffafd7") (magenta-00 "#ffd7ff")
      (green-00 "#d7ff87") (yellow-00 "#ffffaf") (orange-00 "#ffd787") (orange-000 "#ffd7af") (blue-00 "#d7d7ff") (purple-00 "#e6a8df"))

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
   `(linum-highlight-face ((,class (:background "#87875f" :foreground "#d7d7af"))))
   `(highlight ((,class (:background ,green-0))))
   `(hl-line ((,class (:background ,green-00)))) ; Embedded strings of IM (e.g. fcitx) would be very unreadable... orz
   `(region ((,class (:foreground ,white-0 :background ,blue-1))))
   `(secondary-selection ((,class (:background ,blue-3 :foreground ,white-0))))
   `(isearch ((,class (:foreground ,white-0 :background ,orange-3))))
   `(lazy-highlight ((,class (:background ,magenta-3 :foreground ,white-0))))
   `(trailing-whitespace ((,class (:background ,red-3))))
   `(show-paren-match ((,class (:background ,blue-00))))
   `(header-line ((,class (:background ,blue-1 :foreground ,white-0 :underline t))))

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
   `(minibuffer-prompt ((,class (:foreground ,black-5 :background ,green-2 :bold nil))))
   `(escape-glyph ((,class (:foreground ,yellow-3))))
   `(error ((,class (:foreground ,red-0))))
   `(warning ((,class (:foreground ,orange-1))))
   `(success ((,class (:foreground ,green-2))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,purple-2))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,white-4 :slant italic))))
   `(font-lock-comment-face ((,class (:foreground ,white-4 :slant italic))))
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
   `(org-archived ((,class (:slant italic))))
   `(org-checkbox ((,class (:background ,white-2 :foreground ,black-3
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,blue-2 :underline t))))
   `(org-done ((,class (:bold t :weight bold :foreground ,green-4 :background ,green-0
                              :box (:line-width 1 :style none)))))
   `(org-todo ((,class (:bold t :weight bold :foreground ,red-3 :background ,orange-0
                              :box (:line-width 1 :style none)))))
   `(org-level-1 ((,class (:bold t :foreground ,blue-1))))
   `(org-level-2 ((,class (:bold t :foreground ,green-2))))
   `(org-level-3 ((,class (:bold t :foreground ,orange-2))))
   `(org-level-4 ((,class (:bold t :foreground ,cyan-3))))
   `(org-level-5 ((,class (:bold t :foreground ,red-2))))
   `(org-level-6 ((,class (:bold t :foreground ,magenta-2))))
   `(org-level-7 ((,class (:bold t :foreground ,purple-2))))
   `(org-level-8 ((,class (:bold t :foreground ,yellow-2))))
   `(org-tag ((,class (:background ,black-1 :foreground ,white-0 :bold t :weight bold))))
   `(org-column ((,class (:background ,yellow-00 :foreground ,black-3))))
   `(org-column-title ((,class (:background ,blue-0 :foreground ,black-5 :underline t :weight bold))))
   `(org-agenda-structure
     ((,class (:foreground ,black-4 :background ,green-0))))
   `(org-deadline-announce ((,class (:foreground ,red-3))))
   `(org-scheduled ((,class (:foreground ,black-2))))
   `(org-scheduled-previously ((,class (:foreground ,red-1))))
   `(org-scheduled-today ((,class (:foreground ,blue-2))))
   `(org-special-keyword ((,class (:background ,yellow-1 :foreground ,yellow-4))))
   `(org-table ((,class (:background ,yellow-0 :foreground ,black-3))))
   `(org-time-grid ((,class (:foreground ,orange-2))))
   `(org-upcoming-deadline ((,class (:foreground ,red-3))))
   `(org-warning ((,class (:bold t :foreground ,white-0 :background ,red-3))))
   `(org-formula ((,class (:foreground ,purple-2))))
   `(org-headline-done ((,class (:foreground ,green-2))))
   `(org-hide ((,class (:foreground ,LIGHT_BG))))
   `(org-code ((,class (:foreground ,blue-2))))
   `(org-link ((,class (:foreground ,blue-2 :underline ,blue-2))))
   `(org-footnote ((,class (:foreground ,magenta-3))))
   `(org-ellipsis ((,class (:foreground ,red-2))))
   `(org-agenda-clocking ((,class (:foreground ,blue-3 :background ,blue-0 :bold t))))
   `(org-agenda-date ((,class (:foreground ,orange-2 :background ,LIGHT_BG :underline nil))))
   `(org-agenda-date-weekend ((,class (:foreground ,yellow-3 :slant italic :bold nil))))
   `(org-agenda-date-today ((,class (:foreground ,orange-3 :weight bold :underline t))))
   `(org-agenda-column-dateline ((,class (:foreground ,white-0 :background ,yellow-0 :underline t))))
   `(org-agenda-todo ((,class (:foreground ,white-0 :background ,red-2))))
   `(org-agenda-done ((,class (:foreground ,green-3 :background ,nil))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,white-0 :background ,red-2))))
   `(org-priority ((,class (:foreground ,red-3 :background ,nil))))
   `(org-block ((,class (:foreground ,orange-1))))
   `(org-quote ((,class (:foreground ,orange-1))))
   `(org-block-begin-line ((,class (:foreground ,orange-2))))
   `(org-block-end-line ((,class (:foreground ,orange-2))))
   `(org-mode-line-clock ((,class (:foreground ,blue-3 :background ,blue-0 :bold t))))
   `(org-mode-line-clock-overrun ((,class (:foreground ,white-0 :background ,red-1 :bold t))))

   ;; outline
   `(outline-1 ((,class (:bold t :foreground ,blue-1))))
   `(outline-2 ((,class (:bold t :foreground ,green-2))))
   `(outline-3 ((,class (:bold t :foreground ,orange-2))))
   `(outline-4 ((,class (:bold t :foreground ,cyan-3))))
   `(outline-5 ((,class (:bold t :foreground ,red-2))))
   `(outline-6 ((,class (:bold t :foreground ,magenta-2))))
   `(outline-7 ((,class (:bold t :foreground ,purple-2))))
   `(outline-8 ((,class (:bold t :foreground ,yellow-2))))

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
   `(twittering-username-face ((,class (:foreground ,blue-3 :background ,blue-00 :bold t :underline t))))
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
   `(gnus-group-news-1-low ((,class (:foreground ,purple-1))))
   `(gnus-group-news-2 ((,class (:foreground ,blue-2))))
   `(gnus-group-news-2-low ((,class (:foreground ,blue-1))))
   `(gnus-group-news-3 ((,class (:foreground ,green-3))))
   `(gnus-group-news-3-low ((,class (:foreground ,green-2))))
   `(gnus-group-news-4 ((,class (:foreground ,magenta-3))))
   `(gnus-group-news-4-low ((,class (:foreground ,magenta-2))))
   `(gnus-group-news-5 ((,class (:foreground ,orange-3))))
   `(gnus-group-news-5-low ((,class (:foreground ,orange-2))))
   `(gnus-group-news-low ((,class (:foreground ,yellow-3))))
   `(gnus-group-mail-1 ((,class (:foreground ,purple-2))))
   `(gnus-group-mail-1-low ((,class (:foreground ,purple-1))))
   `(gnus-group-mail-2 ((,class (:foreground ,blue-2))))
   `(gnus-group-mail-2-low ((,class (:foreground ,blue-1))))
   `(gnus-group-mail-3 ((,class (:foreground ,green-3))))
   `(gnus-group-mail-3-low ((,class (:foreground ,green-2))))
   `(gnus-group-mail-low ((,class (:foreground ,yellow-3))))
   `(gnus-header-content ((,class (:weight normal :foreground ,yellow-3))))
   `(gnus-header-from ((,class (:foreground ,yellow-2))))
   `(gnus-header-subject ((,class (:foreground ,green-2))))
   `(gnus-header-name ((,class (:foreground ,blue-1))))
   `(gnus-header-newsgroups ((,class (:foreground ,magenta-2))))
   `(gnus-signature ((,class (:italic t :foreground ,black-1))))
   `(gnus-summary-cancelled ((,class (:foreground ,black-3)))) ;
   `(gnus-summary-high-ancient ((,class (:bold t :foreground ,yellow-3))))
   `(gnus-summary-high-read ((,class (:bold t :foreground ,green-3))))
   `(gnus-summary-high-ticked ((,class (:bold t :foreground ,red-2))))
   `(gnus-summary-high-unread ((,class (:bold t :foreground ,black-5))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,yellow-3))))
   `(gnus-summary-normal-read ((,class (:foreground ,green-2))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,red-2))))
   `(gnus-summary-normal-unread ((,class (:foreground ,white-0))))
   `(gnus-summary-low-ancient ((,class (:italic t :foreground ,green-3))))
   `(gnus-summary-low-read ((,class (:italic t :foreground ,yellow-3))))
   `(gnus-summary-low-ticked ((,class (:italic t :foreground ,red-3))))
   `(gnus-summary-low-unread ((,class (:italic t :foreground ,black-1))))
   `(gnus-summary-selected  ((,class (:background ,magenta-3 :foreground ,white-0))))

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

   ;; Dired/Dired+
   `(dired-directory ((,class (:foreground ,blue-1 :bold t))))
   `(dired-flagged ((,class (:foreground ,red-1))))
   `(dired-header ((,class (:foreground ,black-5  :background ,green-01 :bold t))))
   `(dired-ignored ((,class (:foreground ,white-4))))
   `(dired-mark ((,class (:foreground ,green-1))))
   `(dired-marked ((,class (:foreground ,green-2))))
   `(dired-perm-write ((,class (:foreground ,red-2 :bold t))))
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
   `(magit-branch ((,class (:foreground ,blue-4 :background ,blue-0 :bold t))))
   `(magit-diff-add ((,class (:foreground ,green-3 :background nil :bold t))))
   `(magit-diff-del ((,class (:foreground ,red-2 :background nil :bold t))))
   `(magit-diff-file-header ((,class (:foreground ,black-4 :background ,white-3 :bold t))))
   `(magit-diff-hunk-header ((,class (:foreground ,black-2 :background ,white-1 :bold t))))
   `(magit-diff-merge-current ((,class (:foreground ,purple-1))))
   `(magit-diff-merge-diff3-separator ((,class (:foreground ,purple-1))))
   `(magit-diff-merge-proposed ((,class (:foreground ,purple-1))))
   `(magit-diff-merge-separator ((,class (:foreground ,purple-1))))
   `(magit-diff-none ((,class (:foreground ,white-4))))
   `(magit-header ((,class (:foreground ,blue-2 :background ,white-0 :underline ,blue-2))))
   `(magit-item-highlight ((,class (:background ,yellow-00 :foreground ,black-5))))
   `(magit-item-mark ((,class (:foreground ,white-0 :background ,magenta-2))))
   `(magit-log-author ((,class (:foreground ,green-3 :background ,LIGHT_BG))))
   `(magit-log-author-date-cutoff ((,class (:foreground ,red-1 :bold t))))
   `(magit-log-date ((,class (:foreground ,black-5 :background ,green-0))))
   `(magit-log-graph ((,class (:foreground ,black-2 :background ,LIGHT_BG))))
   `(magit-log-head-label-bisect-bad ((,class (:foreground ,white-0 :background ,red-3))))
   `(magit-log-head-label-bisect-good ((,class (:foreground ,black-3 :background ,green-0))))
   `(magit-log-head-label-bisect-skip ((,class (:foreground ,yellow-3 :background ,orange-00))))
   `(magit-log-head-label-default ((,class (:foreground ,green-4 :background ,green-2 :bold t :underline t))))
   `(magit-log-head-label-head ((,class (:foreground ,green-4 :background ,green-01 :bold t :underline t))))
   `(magit-log-head-label-local ((,class (:foreground ,green-4 :background ,green-00 :bold t :underline t))))
   `(magit-log-head-label-patches ((,class (:foreground ,black-5 :background ,orange-1 :bold t :underline t))))
   `(magit-log-head-label-remote ((,class (:foreground ,blue-3 :background ,blue-0 :bold t :underline t))))
   `(magit-log-head-label-tags ((,class (:foreground ,yellow-4 :background ,yellow-00 :bold t :underline t))))
   `(magit-log-head-label-wip ((,class (:foreground ,black-1 :background ,white-2 :bold t :underline t))))
   `(magit-log-message ((,class (:foreground ,black-5 :background ,LIGHT_BG))))
   `(magit-log-reflog-label-amend ((,class (:foreground ,magenta-3 :background ,magenta-0))))
   `(magit-log-reflog-label-checkout ((,class (:foreground ,green-3 :background ,green-00))))
   `(magit-log-reflog-label-cherry-pick ((,class (:foreground ,orange-4 :background ,orange-00))))
   `(magit-log-reflog-label-commit ((,class (:foreground ,yellow-4 :background ,yellow-0))))
   `(magit-log-reflog-label-merge ((,class (:foreground ,purple-4 :background ,purple-00))))
   `(magit-log-reflog-label-other ((,class (:foreground ,white-0 :background ,black-3))))
   `(magit-log-reflog-label-rebase ((,class (:foreground ,green-4 :background ,yellow-00))))
   `(magit-log-reflog-label-remote ((,class (:foreground ,blue-4 :background ,blue-0))))
   `(magit-log-reflog-label-reset ((,class (:foreground ,black-5 :background ,red-0))))
   `(magit-log-sha1 ((,class (:foreground ,orange-2 :background ,LIGHT_BG))))
   `(magit-section-title ((,class (:foreground ,orange-3 :background ,LIGHT_BG :underline ,orange-3 :bold ,t))))
   `(magit-valid-signature ((,class (:foreground ,cyan-3 :background ,LIGHT_BG :bold t))))
   `(magit-whitespace-warning-face ((,class (:foreground ,white-0 :background ,red-2 :bold t))))

   ;; git-commit-mode
   `(git-commit-branch-face ((,class (:foreground ,blue-4 :background ,blue-0 :underline t))))
   `(git-commit-comment-action-face ((,class (:foreground ,orange-2))))
   `(git-commit-comment-file-face ((,class (:foreground ,magenta-2))))
   `(git-commit-comment-heading-face ((,class (:foreground ,green-4 :background ,green-00))))
   `(git-commit-known-pseudo-header-face ((,class (:foreground ,green-2))))
   `(git-commit-no-branch-face ((,class (:foreground ,orange-3))))
   `(git-commit-nonempty-second-line-face ((,class (:foreground ,red-2))))
   `(git-commit-note-face ((,class (:foreground ,cyan-3))))
   `(git-commit-overlong-summary-face ((,class (:foreground ,red-2))))
   `(git-commit-pseudo-header-face ((,class (:foreground ,magenta-3))))
   `(git-commit-summary-face ((,class (:foreground ,blue-2))))
   `(git-rebase-description-face ((,class (:foreground ,white-3))))
   `(git-rebase-killed-action-face ((,class (:foreground ,white-3))))

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

   ;; Diff
   `(diff-added ((,class (:foreground ,green-4 :background ,green-00 :bold t))))
   `(diff-changed ((,class (:foreground ,yellow-4 :background ,yellow-00 :bold t))))
   `(diff-context ((,class (:foreground ,white-4))))
   `(diff-file-header ((,class (:foreground ,blue-3 :background ,blue-0 :bold t :underline t))))
   `(diff-function ((,class (:foreground ,black-2 :background ,white-2))))
   `(diff-header ((,class (:foreground ,blue-3 :background ,blue-0))))
   `(diff-hunk-header ((,class (:foreground ,blue-3 :background ,blue-00 :overline t))))
   `(diff-index ((,class (:foreground ,black-4 :background ,white-3 :bold t))))
   `(diff-indicator-added ((,class (:foreground ,white-0 :background ,green-3 :bold t))))
   `(diff-indicator-changed ((,class (:foreground ,white-0 :background ,yellow-3 :bold t))))
   `(diff-indicator-removed ((,class (:foreground ,white-0 :background ,red-3 :bold t))))
   `(diff-nonexistent ((,class (:foreground ,white-0 :background ,red-3 :bold t))))
   `(diff-refine-added ((,class (:foreground ,green-3))))
   `(diff-refine-change ((,class (:foreground ,yellow-3 :bold t))))
   `(diff-refine-removed ((,class (:foreground ,red-3 :bold t))))
   `(diff-removed ((,class (:foreground ,red-3 :background ,red-00 :bold t))))

   ;; Ediff
   `(ediff-current-diff-A ((,class (:background ,yellow-0))))
   `(ediff-current-diff-Ancestor ((,class (:background ,blue-0))))
   `(ediff-current-diff-B ((,class (:background ,green-00))))
   `(ediff-current-diff-C ((,class (:background ,orange-00))))
   `(ediff-even-diff-A ((,class (:background ,white-3))))
   `(ediff-even-diff-Ancestor ((,class (:background ,white-3))))
   `(ediff-even-diff-B ((,class (:background ,white-3))))
   `(ediff-even-diff-C ((,class (:background ,white-3))))
   `(ediff-fine-diff-A ((,class (:foreground ,white-0 :background ,magenta-2 :bold t))))
   `(ediff-fine-diff-Ancestor ((,class (:foreground ,white-0 :background ,magenta-2 :bold t))))
   `(ediff-fine-diff-B ((,class (:foreground ,white-0 :background ,magenta-2 :bold t))))
   `(ediff-fine-diff-C ((,class (:foreground ,white-0 :background ,magenta-2 :bold t))))
   `(ediff-odd-diff-A ((,class (:background ,white-3))))
   `(ediff-odd-diff-Ancestor ((,class (:background ,white-3))))
   `(ediff-odd-diff-B ((,class (:background ,white-3))))
   `(ediff-odd-diff-C ((,class (:background ,white-3))))

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

   ;; EShell
   `(eshell-ls-archive ((,class (:foreground ,purple-2))))
   `(eshell-ls-backup ((,class (:foreground ,white-3))))
   `(eshell-ls-clutter ((,class (:foreground ,black-1))))
   `(eshell-ls-directory ((,class (:foreground ,blue-1 :bold t))))
   `(eshell-ls-executable ((,class (:foreground ,green-3))))
   `(eshell-ls-missing ((,class (:foreground ,white-0 :background ,red-3))))
   `(eshell-ls-product ((,class (:foreground ,white-0 :background ,green-2))))
   `(eshell-ls-readonly ((,class (:foreground ,orange-3))))
   `(eshell-ls-special ((,class (:foreground ,yellow-3))))
   `(eshell-ls-symlink  ((,class (:foreground ,magenta-2))))
   `(eshell-ls-unreadable ((,class (:foreground ,black-2))))
   `(eshell-prompt ((,class (:foreground ,black-3 :background ,yellow-0 :bold t))))

   ;; Comint prompt
   `(comint-highlight-prompt ((,class (:foreground ,black-3 :background ,yellow-0 :bold t))))

   ;; which-function-mode
   `(which-func ((,class (:foreground ,white-0 :background ,orange-2))))

   ;; Flymake
   `(flymake-warnline ((,class (:underline ,orange-2))))
   `(flymake-errline ((,class (:underline ,red-2))))

   ;; MMM-Mode
   `(mmm-cleanup-submode-face ((,class (:background ,orange-00))))
   `(mmm-code-submode-face ((,class (:background ,blue-00))))
   `(mmm-comment-submode-face ((,class (:background ,blue-0))))
   `(mmm-declaration-submode-face ((,class (:background ,cyan-1))))
   `(mmm-default-submode-face ((,class (:background ,yellow-00))))
   `(mmm-delimiter-face ((,class (:background nil :foreground ,black-5))))
   `(mmm-init-submode-face ((,class (:background ,magenta-0))))
   `(mmm-output-submode-face ((,class (:background ,purple-00))))
   `(mmm-special-submode-face ((,class (:background ,green-00))))

   ;; Clojure
   `(clojure-test-failure-face ((,class (:underline ,orange-2))))
   `(clojure-test-error-face ((,class (:underline ,red-2))))
   `(clojure-test-success-face ((,class (:underline ,green-3))))

   ;; Javascript
   `(js2-function-param-face ((,class (:foreground ,green-3))))

   ;; rcirc
   `(rcirc-bright-nick ((,class (:foreground ,blue-1 :bold t))))
   `(rcirc-dim-nick ((,class (:foreground ,black-2 :bold t))))
   `(rcirc-keyword ((,class (:foreground ,magenta-2 :bold t))))
   `(rcirc-my-nick ((,class (:foreground ,green-2 :bold t))))
   `(rcirc-nick-in-message ((,class (:foreground ,green-3))))
   `(rcirc-nick-in-message-full-line ((,class (:foreground ,blue-3))))
   `(rcirc-other-nick ((,class (:foreground ,blue-4 :bold t))))
   `(rcirc-prompt ((,class (:foreground ,black-3 :background ,green-01 :bold t))))
   `(rcirc-server ((,class (:foreground ,black-1))))
   `(rcirc-server-prefix ((,class (:foreground ,black-1))))
   `(rcirc-timestamp ((,class (:foreground ,orange-2))))
   `(rcirc-track-keyword ((,class (:foreground ,magenta-2))))
   `(rcirc-track-nick ((,class (:foreground ,blue-1))))
   `(rcirc-url ((,class (:foreground ,blue-2 :bold nil :underline ,blue-2))))

   ;; ERC
   `(erc-button ((,class (:foreground ,blue-2 :underline ,blue-2 :bold nil))))
   `(erc-current-nick-face ((,class (:foreground ,green-3))))
   `(erc-dangerous-hosts ((,class (:foreground ,red-3 :bold t))))
   `(erc-direct-msg-face ((,class (:foreground ,orange-2))))
   `(erc-error-face ((,class (:foreground ,red-2))))
   `(erc-header-face ((,class (:background ,blue-1))))
   `(erc-input-face ((,class (:foreground ,black-3))))
   `(erc-keyword-face ((,class (:foreground ,magenta-2 :bold t))))
   `(erc-my-nick-face ((,class (:foreground ,green-2 :bold t))))
   `(erc-nick-default-face ((,class (:bold t :foreground ,blue-4))))
   `(erc-nick-msg-face ((,class (:weight normal :foreground ,orange-2))))
   `(erc-notice-face ((,class (:foreground ,black-1))))
   `(erc-pal-face ((,class (:foreground ,purple-1))))
   `(erc-prompt-face ((,class (:bold t :foreground ,black-3 :background ,green-01))))
   `(erc-timestamp-face ((,class (:foreground ,orange-2))))

   ;; ReStructuredText
   `(rst-level-1-face ((,class (:foreground ,green-3 :background ,green-00 :bold t))))
   `(rst-level-2-face ((,class (:foreground ,green-3 :background ,green-00 :bold t))))
   `(rst-level-3-face ((,class (:foreground ,green-3 :background ,green-00 :bold t))))
   `(rst-level-4-face ((,class (:foreground ,green-3 :background ,green-00 :bold t))))
   `(rst-level-5-face ((,class (:foreground ,green-3 :background ,green-00 :bold t))))
   `(rst-level-6-face ((,class (:foreground ,green-3 :background ,green-00 :bold t))))
   `(rst-block ((,class (:foreground ,orange-2 :background ,LIGHT_BG :bold t))))
   `(rst-reference ((,class (:foreground ,magenta-3 :background ,LIGHT_BG))))
   `(rst-definition ((,class (:foreground ,yellow-3 :background ,LIGHT_BG))))
   `(rst-external ((,class (:foreground ,blue-2 :background ,LIGHT_BG))))

   ;; yalinum
   `(yalinum-bar-face ((,class (:foreground "#d7d7af" :background "#87875f"))))
   `(yalinum-face ((,class (:foreground "#87875f" :background "#d7d7af"))))
   `(yalinum-track-face ((,class (:foreground "#87875f" :background "#d7d7af"))))

   ;; Ruby
   `(enh-ruby-heredoc-delimiter-face ((,class (:foreground ,purple-2))))
   `(enh-ruby-op-face ((,class (:foreground ,orange-3 :bold t))))
   `(enh-ruby-regexp-delimiter-face ((,class (:foreground ,purple-2))))
   `(enh-ruby-string-delimiter-face ((,class (:foreground ,purple-3))))
   `(erm-syn-errline ((,class (:foreground ,white-0 :background ,red-3 :bold t))))
   `(erm-syn-warnline ((,class (:foreground ,white-0 :background ,orange-2 :bold t))))

   ;; ansi-term
   `(term-color-black ((,class (:background ,black-5 :foreground ,black-5))))
   `(term-color-blue ((,class (:background ,blue-1 :foreground ,blue-1))))
   `(term-color-cyan ((,class (:background ,cyan-3 :foreground ,cyan-3))))
   `(term-color-green ((,class (:background ,green-2 :foreground ,green-2))))
   `(term-color-magenta ((,class (:background ,magenta-3 :foreground ,magenta-3))))
   `(term-color-red ((,class (:background ,red-2 :foreground ,red-2))))
   `(term-color-white ((,class (:background ,white-0 :foreground ,white-0))))
   `(term-color-yellow ((,class (:background ,orange-2 :foreground ,orange-2))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,orange-2 :bold t))))
   `(ido-incomplete-regexp ((,class (:foreground ,red-2 :bold t))))
   `(ido-indicator ((,class (:foreground ,yellow-4 :background ,orange-00))))
   `(ido-only-match ((,class (:foreground ,green-4 :background ,green-00 :bold t))))
   `(ido-subdir ((,class (:foreground ,blue-1 :bold t))))
   `(ido-virtual ((,class (:foreground ,magenta-3))))
)

  (custom-theme-set-variables
   'moe-light
   `(ansi-color-names-vector [,black-5 ,red-0 ,green-2 ,yellow-1
			      ,blue-1 ,purple-1 ,blue-0 ,white-0])))
;; fix wrong default face
(if window-system
    (progn
      (set-background-color "#fdfde7")
      (set-foreground-color "#5f5f5f")))

(provide-theme 'moe-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; moe-light-theme.el ends here
