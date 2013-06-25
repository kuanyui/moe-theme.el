;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(define-package "moe-theme" "SNAPSHOT" "A colorful eye-candy theme. Moe, moe, chu!" nil)
