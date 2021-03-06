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

General emacs reference sheet:

http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley

Custom changes follow:

### Files

* `C-x C-f` Open a file. Starts in the current directory
* `C-x f  ` Open a recently visited file
* `C-x o  ` Open a file in the current project (based on .git ++)
* `C-x C-s` Save this file
* `C-x C-r` Rename buffers file
* `C-x C-k` Delete the buffers file
* `C-x C-j` Jump to this files' current directory
* `C-x M-j` Jump to this files' current directory in another window
* `C-x b  ` (or `f1`) Switch to another open file (buffer)
* `C-x C-b` List all open files (buffers)
* `F8     ` Find file at point

### Cut copy and paste

* `C-space` Start marking stuff. C-g to cancel.
* `C-;    ` Mark current line or extend by one line
* `C-w    ` Cut (aka kill)
* `C-k    ` Cut till end of line
* `M-w    ` Copy
* `C-y    ` Paste (aka yank)
* `M-y    ` Cycle last paste through previous kills
* `C-=    ` Expand mark. Press multiple times

### General

* `C-g    ` Quit out of whatever mess you've gotten yourself into
* `M-x    ` Run a command by name
* `C-_    ` Undo
* `M-_    ` Redo
* `C-x u  ` Show the undo-tree (q to quit out)
* `C-x z  ` Repeat last command (keep pressing z to keep repeating)

### Navigation

* `C-arrow` Move past words/paragraphs
* `C-a    ` Go to start of line
* `C-e    ` Go to end of line
* `M-i    ` Go to indentation
* `M-g M-g` Go to line number
* `C-x C-i` Go to symbol
* `C-S-f,b,n,p` Jump around multiple lines/characters at a time.

### Searching

* `C-s    ` Regexp search forward. Press `C-s` again to go further.
  Press `C-o` while searching to run an occur search on the buffer.
  Press `C-w` to search for word under the point.
* `C-r    ` Regexp search backward. Press `C-r` again to go further.
* `F2     ` ACK for string
* `C-F2   ` Find occurances in this buffer (opens in new window)
* `C-F3   ` Highlight symbol at point.
* `C-=    ` Expand highlighted region (by semantic units)
* `C->    ` Mark next match
* `C-<    ` Mark previous match

#### Editable grep buffers with wgrep

* `C-c C-p` Toggle read-only area.
* `C-c C-e` Apply the highlighting changes to file buffers.
* `C-c C-u` All changes are unmarked and ignored.
* `C-c C-d` Delete current line include new line. Command result immediately reflect to file buffer.
* `C-c C-r` Remove the highlight in the region (The Changes doesn't
            apply to files. Of course, if you type C-c C-e, the remained
            highlight changes are applied to files.)
* `C-c C-k` Discard all changes and exit.
* `C-x C-q` Exit wgrep mode.

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
* `C-|    ` Toggle window split
* `C-F4   ` Kill current buffer
* `C-c left ` Restore previous window layout
* `C-c right` Undo restore previous window layout
* `C-x r w a` Save window configuration into register a
* `C-x r j a` Restore window configuration that was saved into
  register a.
* `S-F9     ` Toggle between vertical and horizontal split windows.
* `S-F10    ` Rotate spit windows

### Editing

* `C-S-j  ` Join with next line
* `S-ret  ` Open new line below
* `C-S-o  ` Open new line above

### HTML

* `C-j    ` Zenmode completion (https://github.com/rooney/zencoding)
* `C-c C-a` Add attributes
* `C-c C-e` Close tag
* `C-c C-d` Delete tag (but not contents)

### Paredit keys

* `C-S-d  ` Duplicate the current s-expression on the next line.
* `C-M-f  ` Paredit forward
* `C-M-b  ` Paredit backward
* `M-s    ` Splice sexp into parent
* `M-S-s  ` Split sexp
* `M-S-j  ` Join two sibling sexps.
* `C-)    ` Grab sexp from the right
* `M-)    ` Surround previous sexp
* `C-}    ` Split out last expression from the right
* `M-q    ` Reindent sexp

### Clojure/SLIME Support

* `F6     ` Save and compile clojure
* `C-F6   ` Restart swank (using elein)
* `S-F6   ` Start swank (using elein)
* `C-c C-v` Slime eval and print last expression
* `C-c C-a` Align clojure statements (let, hashes etc)
* `C-c C--` Send form to SLIME repl
* `C-c C-=` Send form to SLIME repl and evalute

### Shell/Command Execution

* `F10    ` Open/create shell
* `M-g M-l` Popup a shell at the bottom or hide if already open
* `M-!    ` Execute command and output results into new buffer
* `C-u M-!` Insert shell output into buffer
* `M-|    ` Pipe region to shell command
* `C-u M-|` Pipe region to shell command and replace region with results

### Other

* `M-x    ` (or `C-x C-m` or `C-c C-m`) execute command
* `C-x C-g` Toggle git-gutter diff display

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

### Ruby/Rails

Interactive Ruby

* `M-x inf-ruby` Start IRB
* `M-x rinari-console` Start rails console
* `M-x bundle-console` Start bundle console
* `M-x robe-start` Start robe for quick navigation with `M-.` and `M-,` (need to start IRB/console session first)

* `C-c C-l  ` Load current buffer into IRB/console session
* `C-c C-b  ` Send the current block to the IRB/console session
* `C-c C-x  ` Send the current definition to the IRB/console session
* `C-c C-z  ` Goto IRB/console session
* `C-c C-d  ` See ruby docs for symbol at point (needs robe to be started)
* `C-c C-k  ` Refresh rails environment for robe

Other

* `C-c {    ` Toggle between { } and do end style blocks
* `C-c C-e  ` Insert end and reindent
* `M-C-F1   ` Run xmp to evaluate buffer
* `M-S-C-F1 ` Eval entire buffer
* `C-c ; f c` Find controller
* `C-c ; f e` Find environment
* `C-c ; f f` Find file-in-project
* `C-c ; f h` Find helper
* `C-c ; f i` Find migration
* `C-c ; f j` Find javascript
* `C-c ; f l` Find plugin
* `C-c ; f m` Find model
* `C-c ; f n` Find configuration
* `C-c ; f o` Find log
* `C-c ; f p` Find public
* `C-c ; f s` Find script
* `C-c ; f t` Find test
* `C-c ; f v` Find view
* `C-c ; f w` Find worker
* `C-c ; f x` Find fixture
* `C-c ; f y` Find stylesheet
* `M-x rinari-test` Run tests
* `M-x rinari-rake` Run rake
* `M-x rinari-sql` Browse the applications database.
* `M-x rinari-web-server` Browse the applications web server

RSpec

* `C-c , t  ` Toggle between code and test
* `C-c , v  ` Verify current spec
* `C-c , a  ` Verify all specs
* `C-c , s  ` Verify the spec at the cursor

### Org Mode

* `M-up   ` Move section up
* `M-down ` Move section down
* `C-x n s` Narrow to a single section
* `C-x n w` Widen to the whole file
* `C-c C-t` Toggle todo state (acts on sections).
* `C-c .  ` Add a timestamp (run twice to do a date range)
* `M-+    ` (and M--) change the todo-date or list type (eg bullet or numbered)
* `M-S-+  ` (and M-S--) change the priority
* `M-ret  ` New headline (or list item)
* `C-ret  ` New headline
* `M-left ` Promote headline or list
* `M-right` Demote headline or list
* `M-S-left` Demote headline or list including subtree
* `C-c C-d` Insert deadline time
* `C-c C-s` Insert scheduled time
* `C-c C-o` Open link
* `C-c C-l` Create link
* `C-c l  ` Store link
* `C-c C-c` Set tags
* `C-c C-t` Toggle task state (TODO, DONE)
* `C-u C-c C-c` Align tags
* `F9     ` Agenda view (ie, `org-agenda`)

### In the Agenda View

* `L      ` Display agenda item
* `<RET>  ` Go to agenda item (replacing agenda view window)
* `F      ` Follow mode
* `q      ` Quit out of agenda view (closes window). `x` will close
  windows opened through agend view.
* `o      ` Delete the other (non-agenda) window
* `d      ` Day view
* `w      ` Week view
* `v m    ` Month view
* `v y    ` Year view
* `f      ` Forward in time 1 day
* `b      ` Back in time 1 day
* `.      ` Go to today
* `l      ` Toggle show closed/done items
* `r      ` Refresh agenda view
* `\      ` Narrow agenda view (see prompts for more info)

### Magit

* `C-c g  ` Magit status (magit-status)
* `s      ` Stage file
* `S      ` Stage all files
* `u      ` Unstage file
* `c      ` Commit staged files. `C-c C-c` after writing commit
  message or `C-c C-k` to abort. `C-c C-a` ammend commit.
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


