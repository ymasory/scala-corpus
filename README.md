# scala-corpus #
## about ##
This is large collection of Scala code being collected for testing static analysis tools such as [Alacs](https://github.com/alacscala/alacs).

## currently included projects ##
- [scalaz](http://code.google.com/p/scalaz/) (16k)

## building  ##
This collection is built using [sbt](http://code.google.com/p/simple-build-tool/).
    sbt update
    sbt compile

NOTE: scala-corpus cannot be built on Linux due to a [known bug](http://lampsvn.epfl.ch/trac/scala/ticket/3623) with `scalac`.
