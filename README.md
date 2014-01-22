mcl-ui-lib
==========

An old library of UI widgets for Macintosh Common LISP, which I wrote
primarily in college. It works fine in MCL 3.0/68k running on System
7.5 in the Basilisk II Mac emulator.

It implements a set of 3D-shaded UI widgets based on an article in
Apple's old developer magazine *develop*, issue 15. Unfortunately, I
can no longer find a copy of that article in my archives or on the
web.

The original README is what follows.

```
README for UI-Lib version 1.0
June, 1997
Adam Alpern

This is UI-Lib 1.0.

See the accompanying file CHANGES for differences between
this and the previous public release.


----------------------------------------------------------------------
 ABOUT UI-LIB
-------------

I originally started writing UI-Lib as a replacement for parts of
Oodles-of-utils, since I wanted 3D buttons, but didn't want to drag
along the rest of OOU as well. Since then, it's grown into an
implementation of most of the 3D interface described in Apple's
developer rag develop, issue 15. Too bad Apple didn't follow those
particular guidelines. I think the Copland 3D look is overbearing and
heavy compared to the develop style.

----------------------------------------------------------------------
 UI-LIB 1.0 vs. 0.7.1
---------------------

UI-Lib version 1.0 is a different beast from the previously released
public version, v. 0.7.1. It's been re-organized and mostly re-written.
While it maintains compatibility in most places, some changes were
necessary, since 0.7.1 is basically a horrendous mess. Version 0.7.1
will continue to be available until my other public contribs have been
ported to 1.0. The differences between 0.7.1 and 1.0 are documented in
the file CHANGES.

----------------------------------------------------------------------
 INSTALLATION
-------------

UI-Lib comes with two means of installing itself. A defsystem file
has been provided, which works with Mark Kantrowitz's portable
defsystem. I highly recommend using this approach in general, as
defsystem is such a damn useful thing. The file "load-ui-lib.lisp"
has also been provided, and will compile and load all the files
as necessary. By renaming it ui-lib.lisp and placing it in a directory
that require knows about, you can load UI-lib with (require "ui-lib").

UI-Lib calls (provide "UI-LIB") and also pushes :ui-lib onto *features*,
so you may check for its presence with #+ui-lib or #-ui-lib in your code.

----------------------------------------------------------------------
 PLANS FOR VERSION 1.1
----------------------

Various things provided for compatibility with version 0.7.1 and
earlier will be removed. They exist in this 1.0 release, but are
deprecated. See the file CHANGES for a list of deprecated symbols
and what replaces them.

Plans for for a drag-manager-less drag and drop mechanism have been
scrapped. Dan Camper's drag-and-drop.lisp version 1.5 does a great
job of seamlessly providing this functionality. Get it!

- preference dialog manager
- icon-popup-menu, icon-sequence-dialog-item
- interface to Mercutio MDEF?
```
