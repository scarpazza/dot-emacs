dot-emacs
=========

This project is a semi-systematic attempt to modernize and clean up portions of my emacs startup
files that I have accumulated over years and years of work.

The individual files should be reasonably well commented with design notes.

The fundamental philosophy of my choices are as follows:


Key Bindings
------------

A lot of default emacs keybindings are, as perceived in 2021, insane.
Many well curated projects remap them radically.

My philosophy is to change only what's used most frequently, and leave most original keybindings in
place unless it's beneficial to remove them.

* Some extremely common function like saving take three keystrokes with the default bindings (`C-x C-s`).
  I remap saving to one key, `F2`.
  I also unbind `C-x C-s`, so that I'm forced to learn the new key.

* emacs leaves many function keys available to the user.  I map virtually all of them to functions
  that I find useful. In fact, I reclaim even more function keys than those available by default
  (e.g., `F3` and `F4`).  My bindings are subjective, however higher usability that good
  function-key mapping affords you is hard to dispute.  This, of course, only applies if you use
  100% keyboards with a full set of function keys.

* The `C-z` binding that suspends the emacs frame is, in my opinion, the most ferociously useless
  and exasperatingly annoying hot key in the history of computing. Combined with the fact that `C-z`
  in CUA is associated with *Undo*, an extremely common function, it is a disaster. I do what seems
  painfully obvious: I map `C-z` to `undo` in emacs. That way, not only do I hide
  `suspend-frame`, a function that in 2021, on modern hardware, can only serve as a foot shotgun,
  but I also allow users to *undo* with the CUA hot key they are used to.

* emacs dedicates to navigation an amount of hot keys that seems --in 2021 and on keyboard that have
  arrow keys-- absurd: `C-f`, `C-b`, `C-p`, `C-n`, `C-v`, `M-v`, `M-f`, `M-b` ...  Today, with most
  contemporary keyboards having arrow keys, even the 75% and 65% models that
  [r/MechanicalKeyboards](https://www.reddit.com/r/MechanicalKeyboards/) people love, it hardly
  makes sense to keep these bindings in place. I reclaim them.

* [More to come.]
