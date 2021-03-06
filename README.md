# scala-corpus #
## about ##
This is large collection of Scala code being collected for testing static analysis tools such as [Alacs](https://github.com/alacscala/alacs).

There are currently **258266** lines of Scala in the corpus.

Line counts are generated using [cloc](http://cloc.sourceforge.net/).

## building  ##
This collection is built using [sbt](http://code.google.com/p/simple-build-tool/). Compilation is exactly one step:
    sbt test-compile

For some reason `sbt` will print Java unchecked warnings in red as errors even though they're just warnings. Don't be alarmed, the corpus will still compile.

NOTE: Some systems (e.g., Linux with home directory encryption) may not be able to build scala-project due to a [known limitation](http://lampsvn.epfl.ch/trac/scala/ticket/3623) of `scalac` concerning file name lengths.

## currently included projects ##
* [scalatest](http://www.scalatest.org/download) (50k)
* [blueeyes](https://github.com/jdegoes/blueeyes.git) (21k)
* [specs](http://code.google.com/p/specs/source/checkout) (20k)
* [akka](https://github.com/jboner/akka) (14k)
* [factorie](http://code.google.com/p/factorie/source/checkout) (13k)
* [scalaz](https://github.com/scalaz/scalaz) (10k)
* [kiama](http://code.google.com/p/kiama/source/checkout) (10k)
* [squeryl](https://github.com/max-l/Squeryl)  (9k)
* [scalariform](https://github.com/mdr/scalariform) (8k)
* [scala-stm](https://github.com/nbronson/scala-stm.git) (8k)
* [scala-query](https://github.com/szeiger/scala-query) (7k)
* [kafka](https://github.com/kafka-dev/kafka) (7k)
* [scalaxb](https://github.com/eed3si9n/scalaxb) (5k)
* [gdata-scala-client](http://code.google.com/p/gdata-scala-client/source/checkout) (4k)
* [casbah](https://github.com/mongodb/casbah) (4k)
* [ensime](https://github.com/aemoncannon/ensime) (4k)
* [scala-swing](https://github.com/ingoem/scala-swing) (4k)
* [scala-migrations](http://code.google.com/p/scala-migrations/source/checkout) (3k)
* [smile](https://github.com/robey/smile) (3k)
* [flashup](https://github.com/ymasory/Flashup) (1k)

## project we are working on adding ##
* [scala](https://github.com/scala/scala) (177k)
* [lift](https://github.com/lift/lift) (58k)
* [apparat](http://code.google.com/p/apparat/source/checkout) (18k)
* [scalate](https://github.com/scalate/scalate) (16k)
* [sgine](hg clone https://sgine.googlecode.com/hg/ sgine) (16k)
* [akka-modules](https://github.com/jboner/akka-modules) (15k)
* [gapt](http://code.google.com/p/gapt/source/checkout) (14k)
* [scalanlp](https://github.com/dlwh/scalanlp-core) (13k)
* [scala-ide](http://www.assembla.com/wiki/show/scala-ide/Source_Code) (12k)
* [uniscala](http://uniscala.net/mvn/source-repository.html) (10k)
* [scala-refactoring](http://www.assembla.com/code/scala-refactoring/git/nodes?rev=master) (10k)
* [circumflex](https://github.com/inca/circumflex) (6k)

## cannot include: require some dependencies pre-installed on build system ##
* [scala-intellij](http://git.jetbrains.org/?p=idea/scala-plugin.git) (67k)
* [kojo](http://code.google.com/p/kojo/source/checkout) (15k)
* [gizzard](https://github.com/twitter/gizzard) (5k)
* [flockdb](https://github.com/twitter/flockdb) (4k)

## cannot include: require < 2.8.0 ##
* [sbt](http://code.google.com/p/simple-build-tool/source/checkout) (15k)
* [norbert](https://github.com/rhavyn/norbert) (5k)
* [scalax.io](https://github.com/eengbrec/Scalax.IO) (5k)
* [swap-scala](http://code.google.com/p/swap-scala/source/checkout) (5k)
* [scalala](http://code.google.com/p/scalala/source/checkout) (4k)
* [configgy](https://github.com/robey/configgy) (4k)
* [sweetscala](http://code.google.com/p/sweetscala/source/checkout) (3k)

