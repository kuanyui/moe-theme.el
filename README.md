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
<a href="https://raw.github.com/kuanyui/moe-theme.el/master/pics/mode-line-preview.png"><img src="pics/mode-line-preview.png" width="710" height="182"/></a>

## What Special?
* Optimized for terminal's 256 color palettes.
* Black-on-white & white-on-black.
* Delightful and cheerful color palettes.
* Quite completed (and reasonable) font faces for each mode.
* **Easy to customize!**
    * Colorful Mode-line / Powerline.
    * Enlarge titles font sizes or not.
    * Automatically switch between dark and light `moe-theme` by local time! (optional)

## Support
`moe-theme.el` provide good-looking™ and quite fully-supported font-faces for various modes, include:
* Diff / EDiff
* Dired / Dired+
* ERC / rcirc
* Eshell / Ansi-term
* Gnus / Message
* Helm / ido
* Org-mode
* Magit / Git-commit / Git-gutter
* Markdown-mode
* popup / Auto-complete-mode
* Rainbow-delimiters
* Twittering-mode
* undo-tree
* ......and More!

## Requirements
* Emacs 24 or above.
* 256-colors (or higher) terminal.

## Download
### Via package.el
`Moe-theme` is available in [MELPA](https://github.com/milkypostman/melpa) repository now, so you can install `moe-theme` easily with `M-x` `list-packages`.

### Manually
Download the archive of `moe-theme` to `~/.emacs.d/themes` and extract it. Then, add these to your init file:

```lisp
	;;customize theme
	(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
```

## Customizations
It's impossible to satisfy everyone with one fixed theme, but `moe-theme` provide some customizations that you may like.

Select a theme you like and let's go on:

```lisp
    (require 'moe-theme)
    ;; Choose the one you like, (moe-light) or (moe-dark)
    (moe-light)
```

>#### Load Theme Itself Only?
>If you just want to use `load-theme` to apply **ONLY** `moe-theme` itself and **without customizations**, you can skip "Customizations" chapter and just use this:
>
>```lisp
>    (load-theme 'moe-dark t)
>    ;;or
>    (load-theme 'moe-light t)
>```

### Resize Titles
You may want to resize titles in `markdown-mode`, `org-mode`, or `ReStructuredText-mode`:

```lisp
  ;; Resize titles
  (setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))
  (setq moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
  (setq moe-theme-resize-rst-title '(2.0 1.7 1.5 1.3 1.1 1.0))
```

>Markdown should have 6 items; org has 9 items; rst has 6 items.

The values should be lists. Larger the values, larger the fonts.
If you don't like this, just leave them nil, and all the titles will be the same size.

### Colorful Mode-line and Powerline
Tired of boring blue mode-line? Set default mode-line color like this:
```lisp
  (setq moe-theme-mode-line-color 'orange)
  ;; (Available colors: blue, orange, magenta, yellow, purple, red, cyan, w/b.)
```

You can use `moe-theme-select-color` to change color interactively.

Mayby you'll also like `moe-theme-random-color`, which gives you a random mood :D.

#### Powerline
Now `moe-theme` supports [Powerline](https://github.com/milkypostman/powerline), which makes mode-line looks fabulous! We recommended installing `powerline` and run `powerline-moe-theme`.

### Too Yellow Background?
With 256-colors terminal, default yellow background of moe-light may be too yellow and harsh to eyes on some screens.

If you encounter this problem, and want to set background color to `#ffffff` in terminal, set the value of `moe-light-pure-white-background-in-terminal` to t:

```lisp
    (setq moe-light-pure-white-background-in-terminal t)
```

### Highlight Buffer-id on Mode-line?
You may be dislike default highlight on mode-line-buffer-id, now it can be disable:

```lisp
(setq moe-theme-highlight-buffer-id nil)
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

## Notes
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
Can't bear a mode with an ugly looking? `moe-theme` doesn't support the mode you like? It's welcome to report wishlist or issue on github, I'll try to add related settings as soon as possible. Or of course, you can push request, too. We need your feedback!

## Known Issues
* When using `moe-light` and typing characters under terminal emulator (e.g. Konsole) with IM (e.g. fcitx), the string embedded in Emacs may be very insignificant (But as you output the word from IM, it turns normal).

## Todo
* Minor mode for `moe-theme-switcher`.
* (Seems impossible...?) A variable controlling if enlarge title font size, e.g. org-mode.

## License
`moe-theme.el` (include images) is released under GPL v3. Copyleft is so cute!

