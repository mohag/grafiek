grafiek
=======

A graphing program I wrote a long time ago...

Seems like I was busy translating the UI to English under the name "DosPlot"

Compiling:
It does not currently compile for Linux.

I might do some tests for other targets...

PCalcExp
========

PCalcExp is a stack-based parser for equasions, that might be useful outside this application as well.

I initially used calcexp (which was based on  Cal.pas by Colin Lamarre, 1991, but it was too slow.

PCalcExp converts the expression to postfix and then parses it using a stack. (A direct FPU implementation might be an intersting project...)

Calculat
--------
A simple calculator to test PCalcExp...

Can be compiled with `fpc -MNESTEDCOMMENTS- calculat.pas` under ArchLinux

Grafiek version 1.1 README
--------------------------
See version.txt for updates in this version.

This version's compilation was tested with Free Pascal 1.0.10.  Get it from www.freepascal.org.

It is important to inform me of bugs in order to ensure that they are fixed in the next release.

Bug reports:
------------
If reporting a bug, please give a description of what the program does and what you expected it to do.  Please ensure that the problem is repoduceable.

Please include a brief description of your computer's specifications, especially the graphics card.

Please include the dosplot.log file
