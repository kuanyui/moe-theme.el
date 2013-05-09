<link href="markdown.css" rel="stylesheet"></link>
# moe-theme
Just another Emacs theme.
![Screenshot](https://github.com/kuanyui/moe-theme.el/raw/master/pics/screenshot.png)
This theme supports Emacs 24 native theme.
# Support
`moe-theme.el` provide good-looking[tm] and quite fully-supported font-faces include:
* Dired/Dired+
* Helm
* Org-mode
* Magit
* Markdown-mode
* popup/Auto-complete-mode
* Rainbow-delimiters
* Twittering-mode
* undo-tree
* ......and More!



## Requirements
* Emacs 24 (or above)
* 256-colors terminal.(or of course, GUI version Emacs.)

## Install
`moe-light` and `moe-dark` are independent from each other, so you can just download one of them.

Download the one you prefer to `~/.emacs.d/themes`.Then, add these to your init file (take `moe-dark-theme.el` as example):

	;;customize theme
	(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
	(load-theme 'moe-dark t)
    
Or you can load theme just by adding:    

    (load-file "~/.emacs.d/themes/moe-dark-theme.el"))

### Have A Good Mood Today?
You can also add these to your `.emacs` to automatically switch `moe-dark` and `moe-light` according to day or night:

    (defun auto-switch-moe-theme ()
       (let ((now (string-to-int (format-time-string "%H"))))
          (if (and (>= now 06) (<= now 17))
                  (load-theme 'moe-light) (load-theme 'moe-dark))
                      nil))
    (run-with-timer 0 (* 1 60) 'auto-switch-moe-theme)

From now on, your Emacs will have a light theme within day and have a dark one in night. =w=+

## Note
### No 256-Color Output?
If your terminal emulator doesn't render 256-color output correctly, set its
environment variable `TERM` to `xterm-256color`. For example, if you are using
`Konsole`, navigate to `Edit Current Profile > General > Environment > Edit` and
add the following line:

    TERM=xterm-256color

If you also use `tmux`, add this to `~/.tmux.conf`, too:
	
    set -g default-terminal "screen-256color"

### Paren
If you use Emacs build-in `show-paren-mode`, I recommend set the value of `show-paren-style` to `expression` for optimized visual experience:

    (show-paren-mode t)
    (setq show-paren-style 'expression) ;;另一種突顯方式(突顯整個括號範圍)

### Not supported the mode(s) you're using?
The mode you're using has an ugly looking? `Moe-theme` doesn't support the mode you like? It's welcome to report wishlist or issue on github, I'll try to add related settings as soon as possible. Or of course, you can push request, too.


## Known Bugs
* When type characters with IM (e.g. fcitx), and run Emacs under terminal emulator (e.g. Konsole) with `moe-light-theme`; when you type words in IM, the string embedded in Emacs may be very insignificant (But as you output the word from IM, it turns normal).

## License
`moe-theme.el` (include images) is released under GPL v3.
