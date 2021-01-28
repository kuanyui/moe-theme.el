;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode
  (eval . (progn
            (when (fboundp 'aggressive-indent-mode)
              (aggressive-indent-mode -1))))))
