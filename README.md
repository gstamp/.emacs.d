# .emacs.d

This is my personal setup for emacs 24 and as such reflects my preferences.
Despite this however is should be still generally applicable.  It uses
the Emacs Starter Kit as it's base and then builds from there.

This was originally migrated from my Emacs 23 version however it's structure
has changed somewhat since the starter kit has change significantly since
that time.

# Features



# Using

## Keys Quick Reference

### Files

* `C-x C-f` Open a file. Starts in the current directory
* `C-x f  ` Open a recently visited file
* `C-x o  ` Open a file in the current project (based on .git ++)
* `C-x C-s` Save this file
* `C-x C-j` Jump to this files' current directory
* `C-x M-j` Jump to this files' current directory in another window
* `C-x b  ` (or `f1`) Switch to another open file (buffer)
* `C-x C-b` List all open files (buffers)

### Cut copy and paste

* `C-space` Start marking stuff. C-g to cancel.
* `C-w    ` Cut (aka kill)
* `C-k    ` Cut till end of line
* `M-w    ` Copy
* `C-y    ` Paste (aka yank)
* `M-y    ` Cycle last paste through previous kills
* `C-@    ` Mark stuff quickly. Press multiple times
* `C-.    ` Retype the last thing you typed

### General

* `C-g    ` Quit out of whatever mess you've gotten yourself into
* `M-x    ` Run a command by name
* `C-.    ` Autocomplete
* `C-_    ` Undo
* `M-_    ` Redo
* `C-x u  ` Show the undo-tree (q to quit out)

### Navigation

* `C-arrow` Move past words/paragraphs
* `C-a    ` Go to start of line
* `C-e    ` Go to end of line
* `M-i    ` Go to indentation
* `M-g M-g` Go to line number
* `C-x C-i` Go to symbol
* `C-s    ` Regexp search forward. Press `C-s` again to go further.
* `C-r    ` Regexp search backward. Press `C-r` again to go further.

### Bookmarks

* `S-F2   ` Toggle bookmark
* `S-F3   ` Go to next bookmark
* `S-F4   ` Go to previous bookmark

### Window management

* `C-x 0  ` Close this window
* `C-x 1  ` Close other windows
* `C-x 2  ` Split window horizontally
* `C-x 3  ` Split window vertically
* `S-arrow` Jump to window to the left/right/up/down

## Clojure/SLIME Support

TBA

## To Be Completed

 - auto-complete
 - snippets

## Fix broken plugins
 - Durendal
