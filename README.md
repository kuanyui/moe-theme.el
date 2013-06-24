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
<a href="https://raw.github.com/kuanyui/moe-theme.el/master/pics/dark05.png"><img src="pics/dark05.png" width="355" height="192"/></a>
<a href="https://raw.github.com/kuanyui/moe-theme.el/master/pics/light05.png"><img src="pics/light05.png" width="355" height="192"/></a>

## What Special?
* Optimized for terminal color palettes.
* Black-on-white & white-on-black.
* Delightful and cheerful color-palettes.
* Quite completed font faces for each mode.
* Automatically switch between dark and light `moe-theme` by local time! (optional)

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
* Emacs 24 or above (CLI, that is `emacs -nw`)
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

## Have A Good Mood Today?
I prefer a terminal with a black-on-white color scheme. I found that in the daytime, sunlight is strong and black-on-white is more readable; However, white-on-black would be less harsh to the eyes at night.

So if you like, you can add the following line to your `~/.emacs` to automatically switch between `moe-dark` and `moe-light` according to the system time:

```lisp
	(require 'moe-theme-switcher)
```

By adding the line above, your Emacs will have a light theme in the day and a dark one at night. =w=+

### Live in Antarctica?
Daytime is longer in summer but shorter in winter; or you live in a high latitude region which midnight-sun or polar-night may occur such as Finland or Antarctica?

There's a variable `moe-theme-switch-by-sunrise-and-sunset` would solve your problem (default value is `t`)

If this value is `nil`, `moe-theme-switcher` will switch theme at fixed time (06:00 and 18:00).

If this value is `t` and both `calendar-latitude` and `calendar-longitude` are set properly, the switching will be triggered at the sunrise and sunset time of the local calendar.

Take "Keelung, Taiwan" (25N,121E) for example, you can set like this:

```lisp
	(setq calendar-latitude +25)
	(setq calendar-longitude +121)
```

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
Can't bear a mode with an ugly looking? `moe-theme` doesn't support the mode you like? It's welcome to report wishlist or issue on github, I'll try to add related settings as soon as possible. Or of course, you can push request, too.

## Known Issues
* When using `moe-light` and typing characters under terminal emulator (e.g. Konsole) with IM (e.g. fcitx), the string embedded in Emacs may be very insignificant (But as you output the word from IM, it turns normal).
* The background colors seems to be wrong under a GUI version's Emacs.

## License
`moe-theme.el` (include images) is released under GPL v3. Copyleft is so cute!
