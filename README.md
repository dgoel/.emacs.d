# My emacs settings

My emacs configuration initially forked from magnars but heavily personalized
thereafter with bits and pieces accumulated from the internet over many years.

## Structure

The repository is organized based on the directory structure used by Unix-style
operating systems:

* `init.el`   Main entry point for loading the configuration
* `custom.el` File to store customizations automatically generated by emacs
* `bin `      Place to store binaries generated from emacs' packages like irony-server
* `etc `      Place to store package (core or third party) configurations
* `var `      Cache and other runtime state for various packages, which can be
  cleaned up at any time, is stored here


## Note about keybindings

I try to follow the general emacs keybindings convention as much as possible:

1. `C-x`: reserved for Emacs native essential keybindings like buffer, frame,
   file, etc...

2. `C-g`, `C-h` and `<ESC>`: reserved for keyboard-quit, help and alternate for
   `M-x` respectively.

3. `C-c <letter>` and `<f5>`-`<f12>`: reserved for the user.

4. `C-c C-<letter>`: reserved for major mode.


## Tips for using these emacs settings

If you want to use my settings straight out of the box, here are some things to note:

 * I recommend starting with a blank emacs +
   [Technomancy's better-defaults package](https://github.com/technomancy/better-defaults),
   and then dig through this repo for useful nuggets, instead of forking it directly.

 * You quit emacs with `C-x R Q`, mnemonic *Really Quit*.

 * Find file in dir with `C-x C-f`, recent with `C-x f`

 * Autocomplete with `C-.`

 * expand-region is your friend. Find its bound key by doing `C-=`

 * Undo with `C-_` and redo with `M-_`. Watch the undo-tree with `C-x u`

 * I recommend rebinding Caps Lock to Ctrl and use that instead of the often badly placed Ctrl-key.

## Survival guide for the first week of emacs

When you start using emacs for the first time, your habits fight you every inch
of the way. Your fingers long for the good old familiar keybindings. Here's an
overview of the most commonly used shortcuts to get you through this pain:

* `C      ` Shorthand for the ctrl-key
* `M      ` Shorthand for the meta-key (bound to cmd on my mac settings)
* `S      ` Shorthand for the shift-key

### Files

* `C-x C-f` Open a file. Starts in the current directory
* `C-x f  ` Open a recently visited file
* `C-x o  ` Open a file in the current project (based on .git ++)
* `C-x C-s` Save this file
* `C-x C-w` Save as ...
* `C-x C-j` Jump to this files' current directory
* `C-x b  ` Switch to another open file (buffer)
* `C-x C-b` List all open files (buffers)

### Cut copy and paste

* `C-space` Start marking stuff. C-g to cancel.
* `C-w    ` Cut (aka kill)
* `C-k    ` Cut till end of line
* `M-w    ` Copy region or whole line if no region is active
* `C-y    ` Paste (aka yank)
* `M-y    ` Cycle last paste through previous kills
* `C-x C-y` Choose what to paste from previous kills
* `C-@    ` Mark stuff quickly. Press multiple times

### General

* `C-g    ` Quit out of whatever mess you've gotten yourself into
* `M-x    ` Run a command by name
* `C-.    ` Autocomplete
* `C-_    ` Undo
* `M-_    ` Redo
* `C-x u  ` Show the undo-tree
* `C-x m  ` Open magit. It's a magical git interface for emacs

### Navigation

* `C-arrow` Move past words/paragraphs
* `C-a    ` Go to start of line
* `C-e    ` Go to end of line
* `M-g M-g` Go to line number
* `C-x C-i` Go to symbol
* `M-i    ` Imenu
* `C-s    ` Search forward. Press `C-s` again to go further.
* `C-r    ` Search backward. Press `C-r` again to go further.

### Window management

* `C-x 0  ` Close this window
* `C-x 1  ` Close other windows
* `C-x 2  ` Split window horizontally
* `C-x 3  ` Split window vertically

### Help

* `F1 t   ` Basic tutorial
* `F1 k   ` Help for a keybinding
* `F1 r   ` Emacs' extensive documentation
