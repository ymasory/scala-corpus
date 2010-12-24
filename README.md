# scala-corpus #
## about ##
This is large collection of Scala code being collected for testing static analysis tools such as [Alacs](https://github.com/alacscala/alacs).

## currently included projects ##
- [scalaz](http://code.google.com/p/scalaz/) (16k lines)

## building  ##
This collection is built using [sbt](http://code.google.com/p/simple-build-tool/).
    sbt update
    sbt compile

NOTE: scala-corpus cannot be built on Linux due to a [known bug](http://lampsvn.epfl.ch/trac/scala/ticket/3623) with `scalac`.

## project we would like to include ##
* [scala](https://github.com/scala/scala) (282k lines)
* [lift](https://github.com/lift/lift) (102k)
* [akka](https://github.com/jboner/akka) (24k)
* [sbt](http://code.google.com/p/simple-build-tool/source/checkout) (18k)
* [kafka](https://github.com/kafka-dev/kafka) (11k)
* [circumflex](https://github.com/inca/circumflex) (10k)
* [scalala](http://code.google.com/p/scalala/) (8k)
* [configgy](https://github.com/robey/configgy) (6k)
* [flockdb](https://github.com/twitter/flockdb) (6k)
* [gizzard](https://github.com/twitter/gizzard) (6k)
