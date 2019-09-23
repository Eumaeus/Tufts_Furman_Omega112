# Workbench

**CITE/HMT Workbench**

An SBT repository for beginning work with Scala and the *Homer Multitext*.

## Using

~~~ bash
$ cd /PATH/TO/THIS/DIRECTORY
$ sbt console
$ load scripts/load-library.sc 
~~~

Important things are included in `utilities.cex`, especially the various include-statements.

Other scripts are in the `scripts/` directory. It is a good idea, between running scripts, to do:

~~~ scala
scala> :reset
~~~ 

In the console environment, SBT never knows when you are *done* with an object, so it keeps everything in memory. Eventually, the JVM environment will crash, unless you clean up with `:reset` from time to time. (When running real apps, Scala's "Garbage Collection" is very, very good at managing memory.)


