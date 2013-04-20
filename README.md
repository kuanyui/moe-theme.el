<link href="markdown.css" rel="stylesheet"></link>
# moe-theme
Just another Emacs theme.
![Screenshot](https://github.com/kuanyui/moe-theme.el/raw/master/screenshot)
This theme supports Emacs 24 native theme.
# 
> I am a blockquote
## Warranty and License
This theme is made by an Emacs user who don't understand programming at all (truly); so, 
*I WILL NOT PROVIDE ANY WARRANTY FOR THIS FREE SOFTWARE.*
**bold**
*italic*
This theme (include pictures) is released under GPL v3.

## Requirements
* Emacs 24 (or above)
* 256-colors terminal.(or of course, GUI version Emacs.)

## Install
`moe-light` and `moe-dark` are independent from each other, so you can just download one of them.

Download the one you prefer to `~/.emacs.d/themes`.Then, add these to your init file (take `moe-dark-theme.el` as example):

	;;customize theme
	(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") ;;Emacs24之後的theme路徑指定
	(load-theme 'moe-dark t)
	(enable-theme 'moe-dark)

    
Or you can load theme just by adding:    

    (load-file "~/.emacs.d/themes/moe-dark-theme.el"))  ;;都不行就直接用這招
### Paren
I recommend set the value of `show-paren-style` to `expression` for optimized visual experience:

    (show-paren-mode t)
    (setq show-paren-style 'expression) ;;另一種突顯方式(突顯整個括號範圍)
    
## Note
### Doesn't have a 256-colors terminal?
If you use `Konsole`, it can be customized in `Edit Current Profile>General>Environment>Edit`

    TERM=xterm-256color
    
If you also use `tmux`, add this to `~/.tmux.conf`, too:
	
    set -g default-terminal "screen-256color"

## Nonsense
1. Is this theme safe?
    => I don't understand programming at all, so don't worry that I would put some dangerous elisp in it.

沒想到我竟然也有開github repository的一天啊。

當然，這次依舊奉行我那一貫不會寫程式(tm)的原則，這只是個Emacs theme，以後大概也不會做類似的事了，頂多也只會做這種程度的東西而已。

我自己都覺得這程式碼寫得真是醜爆了，如果您願意協助精簡這個theme，非常的歡迎push commit。

一開始是為了解決Emacs24裡預設的theme會導致completions選單裡的字看不見等等怪異問題，才開始想辦法看要如何自訂Emacs 24的native theme。如果我知道有tomorrow-theme.el，我或許當初就不會做這個了。然而因為自己龜毛，一旦了解Emacs的theme該如何寫後，就開始強迫症發作地想把一切都弄得更好看（其實後來也發現，tomorrow-theme寫得也不見得完整，所以我其實應該還是會自己做），只要是自己有在使用的mode，所有不滿意的地方都全部寫上去。

啊，於是就有了moe-theme.el

我不會寫程式，然而還使用Emacs應該是件有點反常的事：我連寫theme寫錯了都不太會除錯...

我盡量兼顧配色美感與可讀性，所以moe-dark跟moe-light是互相獨立、分開寫的，例如黃色在淡色背景根本看不到，基本上不能用。另外moe-light真的比較難做，畢竟字的顏色普遍都不容易在淡色背景中顯現出來，所以辨識度一定會比較差。如果你很重視辨識度，那請選擇moe-dark。

希望您會喜歡這個moe-theme.el
