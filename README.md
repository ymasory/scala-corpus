# scala-corpus #
## about ##
This is large collection of Scala code being collected for testing static analysis tools such as [Alacs](https://github.com/alacscala/alacs).

Line counts are generated using [cloc](http://cloc.sourceforge.net/).

## currently included projects ##
* [scalaz](https://github.com/scalaz/scalaz) (12k lines)
* [squeryl](https://github.com/max-l/Squeryl)  (9k)
* [scala-query](https://github.com/szeiger/scala-query) (7k)
* [scala-swing](https://github.com/ingoem/scala-swing) (4k)

## building  ##
This collection is built using [sbt](http://code.google.com/p/simple-build-tool/).
    sbt update
    sbt test-compile

NOTE: Some systems may not be able to build scala-corpus due to a [known bug](http://lampsvn.epfl.ch/trac/scala/ticket/3623) with `scalac` concerning file name lengths.

## project we would like to include ##
* [scala](https://github.com/scala/scala) (177k lines)
* [scala-intellij](http://git.jetbrains.org/?p=idea/scala-plugin.git) (67k)
* [lift](https://github.com/lift/lift) (58k)
* [scalatest](http://www.scalatest.org/download) (50k)
* [specs](http://code.google.com/p/specs/source/checkout) (20k)
* [scalate](https://github.com/scalate/scalate) (16k)
* [akka](https://github.com/jboner/akka) (15k)
* [akka-modules](https://github.com/jboner/akka-modules) (15k)
* [kojo](http://code.google.com/p/kojo/source/checkout) (15k)
* [sbt](http://code.google.com/p/simple-build-tool/source/checkout) (15k)
* [scalanlp](https://github.com/dlwh/scalanlp-core) (13k)
* [scala-ide](http://www.assembla.com/wiki/show/scala-ide/Source_Code) (12k)
* [uniscala](http://uniscala.net/mvn/source-repository.html) (10k)
* [kiama](http://code.google.com/p/kiama/source/checkout) (10k)

## too small for now ##
* [scalariform](https://github.com/mdr/scalariform) (8k)
* [kafka](https://github.com/kafka-dev/kafka) (7k)
* [circumflex](https://github.com/inca/circumflex) (6k)
* [scalaxb](https://github.com/eed3si9n/scalaxb) (6k)
* [scalax.io](https://github.com/eengbrec/Scalax.IO) (5k)
* [ensime](https://github.com/aemoncannon/ensime) (5k)
* [gizzard](https://github.com/twitter/gizzard) (5k)
* [scalala](http://code.google.com/p/scalala/source/checkout) (4k)
* [configgy](https://github.com/robey/configgy) (4k)
* [flockdb](https://github.com/twitter/flockdb) (4k)
* [smile](https://github.com/robey/smile) (3k)
