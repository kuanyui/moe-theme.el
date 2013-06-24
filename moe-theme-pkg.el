;;; moe-theme --- A colorful eye-candy theme. Moe, moe, chu!

;; This program is not part of GNU Emacs. 
;; But it is distributed under GPL v3 :-)
;; Author: kuanyui <azazabc123@gmail.com>

;;; Commentary:

;; You can take a look at screenshots and more details on:
;;     https://github.com/kuanyui/moe-theme.el

;; Usage:

;; Add you to your .emacs:

;;
;;	(load-theme 'moe-dark t)
;; 


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


(define-package "moe-theme" "SNAPSHOT" "A colorful eye-candy theme. Moe, moe, chu!" nil)
