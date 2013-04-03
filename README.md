# moe-theme
Just another Emacs theme.
![Screenshot](https://github.com/kuanyui/moe-theme.el/raw/master/screenshot)
This theme supports Emacs 24 native theme.
# 
> I am a comment
## Warranty and License
This theme is made by an Emacs user who don't understand programming at all (truly); so, 
*I WILL NOT PROVIDE ANY WARRANTY FOR THIS FREE SOFTWARE.*
**bold**
*italic*
This theme is released under LGPL

## Requirements
* Emacs 24 (or above)
* 256-colors terminal.(or of course, GUI version Emacs.)

## Install
- `moe-light` and `moe-dark` are dependent to each other, so you can just download one of them.
1. Download the one you prefer to `~/.emacs.d/themes`.
2. Add these to your init file (take `moe-dark-theme.el` as example):
    `;;customize theme
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") ;;Emacs24之後的theme路徑指定
    (load-theme 'moe-dark t)
    (enable-theme 'moe-dark)`
    
Or you can load theme just by adding:    
    `(load-file "~/.emacs.d/themes/moe-dark-theme.el"))  ;;都不行就直接用這招`
3. Restart Emacs, press `y` twice to accept.
## Note
### Doesn't have a 256-colors terminal?
If you use `Konsole`, it can be customized in `Edit Current Profile>General>Environment>Edit`
    `TERM=xterm-256color`
If you also use `tmux`, add this to `~/.tmux.conf`, too:
    `set -g default-terminal "screen-256color"`

## Nonsense
1. Is this theme safe?
    => I don't understand programming at all, so don't worry that I would put some dangerous elisp in it.

一開始是為了解決Emacs24裡預設的theme會導致completions選單裡的字看不見等等怪異問題，才開始想辦法看要如何自訂Emacs 24的native theme。如果我知道有tomorrow-theme.el，我或許當初就不會做這個了。然而因為自己龜毛，一旦了解Emacs的theme該如何寫後，就開始強迫症發作地想把一切都弄得更好看（其實後來也發現，tomorrow-theme寫得也不見得完整，所以我其實應該還是會自己做），只要是自己有在使用的mode，所有不滿意的地方都全部寫上去。

啊，於是就有了moe-theme.el

我不會寫程式，然而還使用Emacs應該是件有點反常的事：我連寫theme寫錯了都不太會除錯...現在真的變成不會寫程式只會吐theme的笨蛋...orz

希望您會喜歡這個moe-theme.el
