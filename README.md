# .emacs.d

This is my personal setup for emacs 24 and as such reflects my preferences.
Despite this however is should be still generally applicable.  It uses
the Emacs Starter Kit as it's base and then builds from there.

This was originally migrated from my Emacs 23 version however it's structure
has changed somewhat since the starter kit has change significantly since
that time.

# Credits

This file contains helpful shortcuts pulled from many sources.  As
such it's somewhat difficult to provide full credit.  Feel free to
take what you need for your own Emacs configuration.

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
* `F8     ` Find file at point

### Cut copy and paste

* `C-space` Start marking stuff. C-g to cancel.
* `C-w    ` Cut (aka kill)
* `C-k    ` Cut till end of line
* `M-w    ` Copy
* `C-y    ` Paste (aka yank)
* `M-y    ` Cycle last paste through previous kills
* `C-@    ` Mark stuff quickly. Press multiple times
* `C-.    ` Retype the last thing you typed (dot-mode)

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

### Searching

* `C-s    ` Regexp search forward. Press `C-s` again to go further.
  Press `c-o` while searching to run an occur search on the buffer.
* `C-r    ` Regexp search backward. Press `C-r` again to go further.
* `F2     ` Grep for string
* `C-F2   ` Find occurances in this buffer (opens in new window)
* `C-F3   ` Highlight symbol at point (currently broken).

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
* `C-`    ` Toggle window split
* `C-F4   ` Kill current buffer
* `C-c C-l` Kill opposite buffer
* `C-c left ` Restore previous window layout
* `C-c right` Undo restore previous window layout
* `C-x r w a` Save window configuration into register a
* `C-x r j a` Restore window configuration that was saved into
  register a.

### Editing

* `C-S-j  ` Join with next line
* `S-ret  ` Open new line below
* `C-S-o  ` Open new line above

### Clojure/SLIME Support

* `F6     ` Save and compile clojure
* `C-F6   ` Restart swank (using elein)
* `S-F6   ` Start swank (using elein)
* `C-c C-v` Slime eval and print last expression
* `C-c C-a` Align clojure statements (let, hashes etc)
* `C-c C-,` Send form to SLIME repl
* `C-c C-.` Send form to SLIME repl and evalute

### Other

* `F10    ` Open/create shell
* `M-x    ` (or `C-x C-m` or `C-c C-m`) execute command

### Dired

* `C-x d  ` Open dired
* `q      ` Bury dired buffer
* `space  ` Navigate to next dired listing
* `^      ` Navigate to parent
* `d      ` Flag file for deletion
* `D      ` Delete immediately
* `u      ` Unmark file
* `m      ` Mark file
* `x      ` Go ahead and actually delete flagged files
* `enter  ` Visit file
* `C-o    ` Open the file in another window (but stay in dired)
* `v      ` View file (q to quit out, e start editing)
* `M-up   ` Go to top of dired list
* `M-down ` Go to bottom of dired list
* `C      ` Copy file
* `R      ` Rename file
* `+      ` Create new directory
* `Z      ` Compress file
* `% m    ` Mark using regex
* `g      ` Refresh file listing
* `C-x C-q` Start editing dired buffer, `C-c C-c` to finish and then
  `C-c esc` to abort.

### Org Mode

* `M-up   ` Move section up
* `M-down ` Move section down
* `C-x n s` Narrow to a single section
* `C-x n w` Widen to the whole file
* `C-c C-t` Toggle todo state (acts on sections).
* `S-right` (and S-left) change list type (eg bullet or numbered)
* `M-ret  ` New headline (or list item)
* `C-ret  ` New headline
* `M-left ` Promote headline or list
* `M-right` Demote headline or list
* `M-S-left` Demote headline or list including subtree

### Magit

* `C-c g  ` Magit status (magit-status)
* `s      ` Stage file
* `S      ` Stage all files
* `u      ` Unstage file
* `c      ` Commit staged files. `C-c C-c` after writing commit
  message or `C-c C-k` to abort. `C-c C-a` sdlkfjlkdfj
* `b b    ` To switch to a branch
* `b m    ` Rename branch
* `b d    ` Delete branch
* `b v    ` List branches (can checkout from resultant screen using RET)
* `P P    ` Git push
* `f f    ` Git fetch
* `F F    ` Git pull
* `TAB    ` Shows diff of file in the list or expand collapse section.
  Stage and unstage actually work on bits of the diff as well.
* `i      ` Ignore file (adds to .gitignore)
* `k      ` Delete. Deletes untracked file and stashes (on section header
  it deletes all untracked files). If you're positioned in a diff
  for an uncommited file you can also delete just the hunk.
* `l l    ` Show history
* `l L    ` Show history in verbose format
* `t t    ` Make lightweight tag
* `t a    ` Make annotated tag
* `x      ` Revert commit history to entered revision
* `z z    ` Create a stash
* `a      ` Apply a stash
* `A      ` Apply the stash and pop it off the stash list
* `z s    ` Creates a snapshot (the stash gets created but the working
  tree is not deleted.
* `w      ` Show how other branches related to the current one
* `m m    ` Start merging.  In the event of conflicts resolve changes
  using e then stage with s.
* `R      ` Starts a rebase `R c` will continue a rebase.  Stage
  resolved conflicts before continuing.


## To Be Completed

 - auto-complete
 - snippets
 - mark multiple not working.

## Fix broken plugins
 - Durendal
