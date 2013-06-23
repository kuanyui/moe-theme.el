# moe-theme
<a href="https://raw.github.com/kuanyui/moe-theme.el/master/pics/moe-theme.png"><img src="https://raw.github.com/kuanyui/moe-theme.el/master/pics/moe-theme.png" width="720" height="401"/></a>
## Screenshot
<a href="https://raw.github.com/kuanyui/moe-theme.el/master/pics/dark01.png"><img src="pics/dark01.png" width="355" height="192"/></a>
<a href="https://raw.github.com/kuanyui/moe-theme.el/master/pics/light01.png"><img src="pics/light01.png" width="355" height="192"/></a>
<a href="https://raw.github.com/kuanyui/moe-theme.el/master/pics/dark02.png"><img src="pics/dark02.png" width="355" height="192"/></a>
<a href="https://raw.github.com/kuanyui/moe-theme.el/master/pics/light02.png"><img src="pics/light02.png" width="355" height="192"/></a>
<a href="https://raw.github.com/kuanyui/moe-theme.el/master/pics/dark03.png"><img src="pics/dark03.png" width="355" height="192"/></a>
<a href="https://raw.github.com/kuanyui/moe-theme.el/master/pics/light03.png"><img src="pics/light03.png" width="355" height="192"/></a>
<a href="https://raw.github.com/kuanyui/moe-theme.el/master/pics/dark04.png"><img src="pics/dark04.png" width="355" height="192"/></a>
<a href="https://raw.github.com/kuanyui/moe-theme.el/master/pics/light04.png"><img src="pics/light04.png" width="355" height="192"/></a>

## What Special?
* Optimized for terminal.
* Black-on-white & white-on-black.
* Delightful and cheerful color-platte.
* Relatively (...maybe?) completed font-faces.
* Automatically switch between dark and light moe-theme according local time! (optional)

## Support
`moe-theme.el` provide good-looking[tm] and quite fully-supported font-faces for various modes, include:
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
* 256-colors (or higher) terminal.

## Install
`moe-light` and `moe-dark` are independent from each other, so you can just download one of them.

Download the one you prefer to `~/.emacs.d/themes`.Then, add these to your init file (take `moe-dark-theme.el` as example):

```lisp
	;;customize theme
	(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
	(load-theme 'moe-dark t)
```
    
Or you can load theme just by adding:    

```lisp
    (load-file "~/.emacs.d/themes/moe-dark-theme.el")
```

### Have A Good Mood Today?
I prefer a terminal with a black-on-white color scheme. I found that in the daytime, sunlight is strong and black-on-white is more readable; However, white-on-black would be less harsh to the eyes at night.

So if you like, you can add the following line to your `~/.emacs` to automatically switch between `moe-dark` and `moe-light` according to the system time:

```lisp
	(require 'moe-theme-switcher)
```

By adding the line above, your Emacs will have a light theme in the day and a dark one at night. =w=+

## Note
### No 256-Color Output?
If your terminal emulator doesn't render 256-color output correctly, set its environment variable `TERM` to `xterm-256color`. For example, if you are using `Konsole`, navigate to `Edit Current Profile > General > Environment > Edit` and add the following line:

    TERM=xterm-256color

If you also use `tmux`, add this to `~/.tmux.conf`, too:
	
    set -g default-terminal "screen-256color"

### Paren
If you use Emacs build-in `show-paren-mode`, I recommend set the value of `show-paren-style` to `expression` for optimized visual experience:

```lisp
    (show-paren-mode t)
    (setq show-paren-style 'expression)
```

### Not supported the mode(s) you're using?
The mode you're using has an ugly looking? `Moe-theme` doesn't support the mode you like? It's welcome to report wishlist or issue on github, I'll try to add related settings as soon as possible. Or of course, you can push request, too.

## Known Bugs
* When type characters with IM (e.g. fcitx), and run Emacs under terminal emulator (e.g. Konsole) with `moe-light-theme`; when you type words in IM, the string embedded in Emacs may be very insignificant (But as you output the word from IM, it turns normal).
* moe-light under GUI version's Emacs seems to have a wrong background color.

## License
`moe-theme.el` (include images) is released under GPL v3. Copyleft is so cute!
