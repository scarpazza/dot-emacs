dot-emacs
=========

This project is a semi-systematic attempt to modernize and clean up portions of my emacs startup
files that I have accumulated over the years.

They are extremely subjective. Take what you find useful, leave what you don't.
To learn more on the individual settings, just read the files. 
They should be commented reasonably well.

The fundamental philosophy of my choices are as follows.


Key Bindings
------------

A lot of default emacs keybindings are, as perceived in 2021, insane.
Many well curated emacs packages remap them radically.

My philosophy is to change only I use most frequently, while leaving most original keybindings in place, unless there's other compelling benefit to remove them.

* Some extremely common function take three keystrokes with the default bindings.
  One example is saving (`C-x C-s`).
  I remap that to one key, `F2`.
  I also unbind the original binding `C-x C-s` so that I'm forced to learn the new one.

* emacs leaves many function keys available to the user. 
  I map virtually all of them to functions that I find useful. 
  In fact, I reclaim even more function keys than those available by default
  (e.g., `F3` and `F4`, originally associated with macro recording).  
  While my choice of new bindings is highly subjective, the higher usability that good
  function-key mapping affords you is hard to dispute.  
  This, of course, only applies if you use 100% keyboards with a full set of function keys.

* The `C-z` binding that suspends the emacs frame is, in my opinion, the most 
  ferociously useless and exasperatingly annoying hot key in the history of computing.
  Combined with the fact that most users are otherwise used to press `C-z` to undo,
  because that's what all software designed after 1987 does, the potential for
  mistakes is ever present.
  I do what the only thing that seems painfully obvious: I map `C-z` to `undo` in emacs.
  That way, not only I bring emacs closer to CUA land, but I also hide
  `suspend-frame`, a function that in 2021 and on modern hardware can 
  only serve as a foot shotgun.
  
* emacs dedicates to navigation an amount of hot keys that seems 
  --in 2021 and on keyboard that have arrow keys-- absurd: 
  `C-f`, `C-b`, `C-p`, `C-n`, `C-v`, `M-v`, `M-f`, `M-b` ...  
  Today, with most contemporary keyboards having arrow keys, even those 
  75% and 65% models that
  [r/MechanicalKeyboards](https://www.reddit.com/r/MechanicalKeyboards/) people love, 
  it hardly makes sense to keep these bindings in place. I reclaim them.

* [More to come.]
