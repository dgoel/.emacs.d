# My emacs settings

My emacs (>= 30.1) configuration initially forked from magnars but heavily
personalized thereafter with bits and pieces accumulated from the internet over
many years.


## Structure

* `early-init.el`       Pre-GUI startup tweaks (GC, `package-quickstart`, `package-directory-list nil`, frame defaults)
* `init.el`             Main configuration, organized into `;;;` sections (appearance, defaults, navigation, editing, modes, VCS/projects)
* `package-manifest.el` Declarative list of third-party packages, loaded standalone by `make up` and `make audit`
* `defuns/`             Personal commands, loaded by `init.el`
* `site-lisp/`          Third-party elisp not available from an archive
* `themes/`             Local colour theme (`dgoel-dark`, a self-contained port of `doom-dark+`)
* `packages.el`         Batch installer/updater driven by `make up`
* `native.el`           Ahead-of-time native compilation driven by `make precompile`
* `audit.el`            Manifest drift checker driven by `make audit`
* `grammars.el`         Optional tree-sitter grammar installer driven by `make grammars` (tree-sitter is off by default; set `dgoel/enable-treesit` to `t` to enable)
* `bin/`                Place to store binaries generated from emacs' packages
* `var/`                Cache and runtime state (`package-quickstart`, `recentf`, `history`, `transient/`, `custom.el`); safe to delete

Run `make help` for the available maintenance targets (`up`, `precompile`,
`audit`, `grammars`, `compile`, `clean`).


## Note about keybindings

I try to follow the general emacs keybindings convention as much as possible:

1. `C-x`: reserved for Emacs native essential keybindings like buffer, frame,
   file, etc.

2. `C-g`, `C-h` and `<ESC>`: reserved for keyboard-quit, help and alternate for
   `M-x` respectively.

3. `C-c <letter>` and `<f5>`-`<f9>`: reserved for the user.

4. `C-c C-[anything]`, `C-c [<digit>, {, }, <, >, :, ;]`: reserved for major mode.


## Tips for using these emacs settings

If you want to use my settings straight out of the box, here are some things to note:

 * I recommend starting with a blank emacs +
   [Technomancy's better-defaults package](https://github.com/technomancy/better-defaults),
   and then dig through this repo for useful nuggets, instead of forking it directly.

 * You quit emacs with `C-x C-c`.

 * Find file in dir with `C-x C-f`, recent with `C-x f`, project file with `C-c p f`

 * Tree-sitter is disabled by default for faster startup (classic modes are used
   for Go, TypeScript, YAML, Dockerfile, etc.). To enable it, set
   `(setq dgoel/enable-treesit t)` in `early-init.el` (uses simplified
   `treesit-enabled-modes` on Emacs 31+).

 * Inline completion is shown as you type (built-in completion-preview-mode);
   `C-<tab>` accepts it, `C-M-i` opens the full completion list

 * Contextual actions on the thing at point with `C-.` (embark)

 * expand-region is your friend. Find its bound key by doing `C-=`

 * The mode line does not list minor modes (it is the slowest thing a mode line
   can do); `C-h m` shows which ones are active

 * Undo with `C-_` and redo with `C-M-_` (built-in `undo-redo`)

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
* `C-c p f` Open a file in the current project (`project.el`)
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
* `C-<tab>` Accept the inline completion, `C-M-i` full completion list
* `C-.    ` Embark: act on the thing at point
* `C-_    ` Undo
* `C-M-_  ` Redo
* `C-x g  ` Open magit. It's a magical git interface for emacs

### Navigation

* `C-arrow` Move past words/paragraphs
* `C-a    ` Go to start of line
* `C-e    ` Go to end of line
* `M-g M-g` Go to line number
* `C-x TAB` Imenu: jump to a symbol in this file
* `M-s l  ` Search lines in this buffer (consult-line)
* `M-s r  ` Ripgrep the project (consult-ripgrep)
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
