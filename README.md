dot-emacs
=========

This project is a semi-systematic attempt to modernize and clean up portions of my emacs startup
files that I have accumulated over the years.

They are extremely subjective. Take what you find useful, leave what you don't.
To learn more on the individual settings, just read the files.
They should be commented reasonably well.

The fundamental philosophy of my choices are as follows.


Where to start
--------------

If you just checked out this project and only see README.md... you are right.
The meat of the project is in "hidden" files `.emacs` and `.emacs.d`.
I aim toward a goal state in which my `.emacs` is just a list of loads similar to:

    (load "~/.emacs.d/scarpaz-edit-prefs.el")

and the contents of `.emacs.d` contain the actual settings, grouped together by topic.
To keep emacs's actual startup files synchronized with the repo, it's not a crazy idea to
make them symbolic links, such as:

    cd ~/git_sandboxes/dot-emacs/.emacs.d/
    for f in scarpaz-*.el; do ln -svf `pwd`/$f ~/.emacs.d/$f; done

It's not crazy... for me. It might be crazy for you. You do you.


Key Bindings
------------



### Global key bindings

A lot of default emacs key bindings are, as perceived in 2021, insane.
Many well curated emacs packages remap them radically.

My philosophy is to change only those I use most frequently, while leaving
most original keybindings in place, unless there's other compelling
benefit to remove them.

* Some extremely common function take three key presses with the default bindings.
  One example is saving (`C-x C-s`).
  I remap that to one key press, `F2`.
  I also unbind the original binding `C-x C-s`, so that I'm forced to learn the new one.

* emacs leaves many function keys available for binding.  I map all of
  them to functions that I find useful.  In fact, I reclaim even more function
  keys than those available by default (e.g., `F3` and `F4`, originally
  associated with macro recording).  While my choice of new bindings is highly
  subjective, the higher usability that a good function-key mapping can afford you is
  hard to dispute.  This, of course, only applies if you use 100% keyboards with
  a full set of function keys.

* The `C-z` binding that suspends the emacs frame is, in my opinion, the most
  ferociously useless and exasperatingly annoying hot key in the history of
  computing.  Combined with the fact that most users are otherwise used to press
  `C-z` to undo, because that's what all software designed after 1987 does, the
  potential for mistakes is ever present.  I do what the only thing that seems
  painfully obvious: I map `C-z` to `undo` in emacs.  That way, not only I bring
  emacs closer to CUA land, but I also hide `suspend-frame`, a function that in
  2021 and on modern hardware can only serve as a foot shotgun.

* emacs dedicates to navigation an amount of hot keys that seems
  --in 2021 and on keyboard that have arrow keys-- absurd:
  `C-f`, `C-b`, `C-p`, `C-n`, `C-v`, `M-v`, `M-f`, `M-b` ...
  Today, with most contemporary keyboards having arrow keys, even those
  75% and 65% models that
  [r/MechanicalKeyboards](https://www.reddit.com/r/MechanicalKeyboards/) people love,
  it hardly makes sense to keep these bindings in place. I reclaim them.
  In the specific case of `C-v`, I go back to CUA land, making it another paste (`yank`).


### Helm key bindings

* If you use emacs the same way I do, you might frequently find yourself
  with a lot of buffers which accumulated over time, and you no longer want.
  I frequently want to declutter them, and I want to do it quickly.
  To that goal, I bind the `delete` key so that it kills buffers in `helm-mini`.
  I do so for both the PC and MacOS keys labeled "delete" or "Del", even
  if they normally perform different functions (on a Mac, the key labeled
  delete is really a backspace).


Calendar
---------

I customize the calendar significantly, because I use it significantly.

For example:

* I added an extra column displaying the number of each week in the year (1 ... 52)

* Pressing any digit ranging from '1' to '9' will select progressively wider
  calendar layouts, with per-day columns ranging from 2 to 9 characters.
  This is especially useful these days in which monitors are wide and have high resolutions.

* My calendar use is centered around weekly reports. I organize my work on a
  weekly basis, via weekly reports that timestamped on Friday dates. Pressing
  `w` or `return` on a calendar date will take you to the weekly report
  associated with the Friday of that week.

* I bind keys `+` and `=` to `diary-insert-entry`

* I bind the space bar to a function that will display both diary and holiday information on a given date.



Jira markdown integration
-------------------

Unfortunately, Atlassian products like BitBucket don't render
org-mode.  Jira tickets requesting that feature [in the
cloud](https://jira.atlassian.com/browse/BCLOUD-6569) and [in their
server software](https://jira.atlassian.com/browse/BSERV-9078) have
been langushing in the *Gathering Interest* stage for years now.

What they render is a customized version of markdown, that
automatically decorates user mentions and automatically links Jira
issues and pull requests.

The [markdown mode by Jason
Blevins](https://jblevins.org/projects/markdown-mode/) already does a
fantastic job at supporting markdown editing.

What it misses are the BitBucket-specific extensions.

I offer a minor mode named `jira-md` that you can use on top of `markdown-mode`.

It offers the same features:
* syntax highlighting
  * of user mentions
  * of jira issues (e.g., ABC-123)
  * of dates (e.g., 2021-10-26)

* "peeking", i.e., getting transient information when the cursor is over
  * user mentions
  * dates (displays the calendar; this is useful to check what day of the week a particular date falls on, as you type it)


* active elements (i.e., pressing `Alt-Enter` when the cursor is over an active element)
  * jumps to jira issues (via `org-jira`) when over a Jira issue ID
  * jumps to the calendar when over a date in one of the recognized formats
  * folds and unfolds list items and their subtrees,
    when over the bullet character (e.g., `*` or `-`)
  * expands names of days of the week into their full date.
