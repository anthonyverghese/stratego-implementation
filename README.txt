This project requires LablGTK, which can be installed on a Mac through
Homebrew and opam with the commands "brew install gtk+" and
"opam install lablgtk".

If you cannot install LablGTK, you can make the command line interface work
without it by removing "package(lablgtk2)" from the _tags file.

We made a Makefile for our project, and the following commands can be used to
initialize the program in different ways:

make:  Initializes the command line interface.

make gui:  Initializes the GUI.

make test:  Runs the OUnit test file.
