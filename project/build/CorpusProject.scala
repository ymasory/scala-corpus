import sbt._
import Dummy._

class CorpusProject(info: ProjectInfo) extends ParentProject(info) {

  lazy val akka = project("akka", "akka", new CorpusSubproject(_) { /* git 57d0e85a9adeb088fbe4b2cb7140647cdbc1c432 */
    lazy val akka_actor = project("akka-actor", "akka-actor", new CorpusSubproject(_))
    lazy val akka_stm = project("akka-stm", "akka-stm", new CorpusSubproject(_), akka_actor)
    lazy val akka_typed_actor = project("akka-typed-actor", "akka-typed-actor", new CorpusSubproject(_), akka_stm)
    lazy val akka_remote = project("akka-remote", "akka-remote", new CorpusSubproject(_), akka_typed_actor)
    lazy val akka_http = project("akka-http", "akka-http", new CorpusSubproject(_), akka_remote)
    lazy val akka_samples = project("akka-samples", "akka-samples", new CorpusSubproject(_) {
      lazy val akka_sample_ants = project("akka-sample-ants", "akka-sample-ants", new CorpusSubproject(_), akka_stm)
      lazy val akka_sample_fsm = project("akka-sample-fsm", "akka-sample-fsm", new CorpusSubproject(_), akka_actor)
      lazy val akka_sample_remote = project("akka-sample-remote", "akka-sample-remote", new CorpusSubproject(_), akka_remote)
    })
  })
  lazy val casbah = project("casbah", "casbah", new CorpusSubproject(_) { /* git 4c4e62442ba4bdf55388b139acb27e2d871dbe11 */
    lazy val commons = project("casbah-commons", "casbah-commons", new CorpusSubproject(_))
    lazy val core = project("casbah-core", "casbah-core", new CorpusSubproject(_), commons, query)
    lazy val query = project("casbah-query", "casbah-query", new CorpusSubproject(_), commons)
    lazy val gridfs = project("casbah-gridfs","casbah-gridfs", new CorpusSubproject(_), core)
  })
  lazy val ensime = project("ensime", "ensime", new CorpusSubproject(_)) /* git d3a4de5805e2b98fd0cb15783af6a6d4b83d535c */
  lazy val factorie = project("factorie", "factorie", new CorpusSubproject(_)) /* hg 566:04f0c4db9013 */
  lazy val flashup = project("flashup", "flashup", new CorpusSubproject(_)) /* git 61aaf4159ef739068295efa03dd08ccbde76b600 */
  lazy val gdata = project("gdata-scala-client", "gdata-scala-client", new CorpusSubproject(_)) /* svn r89 */
  lazy val lift = project("lift", "lift", new CorpusSubproject(_) { /* git b4a29977c572327195f248762bbaf7173656bb51 */
    lazy val liftbase = project("lift-base", "lift-base", new CorpusSubproject(_) {
      lazy val common = project("lift-common", "lift-common", new CorpusSubproject(_))
      lazy val actor = project("lift-actor", "lift-actor", new CorpusSubproject(_),  common)
      lazy val json = project("lift-json", "lift-json", new CorpusSubproject(_))
      lazy val json_ext = project("lift-json-ext", "lift-json-ext", new CorpusSubproject(_), json, common)
      lazy val util = project("lift-util", "lift-util", new CorpusSubproject(_), actor, json)
      lazy val webkit = project("lift-webkit", "lift-webkit", new CorpusSubproject(_), util)
    })
    lazy val liftpersist = project("lift-persistence", "lift-persistence", new CorpusSubproject(_) {
      lazy val proto = project("lift-proto", "lift-proto", new CorpusSubproject(_))
      lazy val mapper = project("lift-mapper", "lift-mapper", new CorpusSubproject(_))
      lazy val jpa = project("lift-jpa", "lift-jpa", new CorpusSubproject(_))
      lazy val record = project("lift-record", "lift-record", new CorpusSubproject(_), mapper)
      lazy val couchdb = project("lift-couchdb", "lift-couchdb", new CorpusSubproject(_), record)
      lazy val mongodb = project("lift-mongodb", "lift-mongodb", new CorpusSubproject(_))
      lazy val mongodbRecord = project("lift-mongodb-record", "lift-mongodb-record",  new CorpusSubproject(_),  mongodb, record)
    }, liftbase)
  })
  lazy val scalamigrations = project("scala-migrations", "scala-migrations", new CorpusSubproject(_)) /* hg 376:23c7b8ffc0f1 */
  lazy val scalatest = project("scalatest", "scalatest", new CorpusSubproject(_)) /* svn r2282 */
  lazy val scalaz = project("scalaz", "scalaz", new CorpusSubproject(_) with ScalazBoilerplate { /* git 32757b4ed1f59dfbd121c9e483f06dada46ff792 */
    override def mainSourceRoots = super.mainSourceRoots +++ srcManagedScala##
    override def compileAction = super.compileAction dependsOn(generateTupleW)
  })
  lazy val scalaquery = project("scala-query", "scala-query", new CorpusSubproject(_)) /* git 0b9f9adfa15716ba4e5a659324000706ee1e42f7 */
  lazy val scalariform = project("scalariform", "scalariform", new CorpusSubproject(_) { /* git 8fbff8152ed98fdca64125ad98a9fbca3564632d */
    lazy val main = project("scalariform", "scalariform", new CorpusSubproject(_))
    lazy val gui = project("gui", "gui", new CorpusSubproject(_), main)
    lazy val corpusScan = project("corpusscan", "corpusscan", new CorpusSubproject(_), main)
    lazy val perf = project("perf", "perf", new CorpusSubproject(_), main)    
  })
  lazy val scalaswing = project("scala-swing", "scala-swing", new CorpusSubproject(_)) /* git 5b45ba65a6afa15b6083bc4c0654d551a379e9a3 */
  lazy val smile = project("smile", "smile", new com.twitter.sbt.StandardProject(_)) /* git ee41070f87e2a99b2556ebde6b505c3dd54e7b88 */
  lazy val specs = project("specs", "specs", new CorpusSubproject(_)) /* svn r1368 */
  lazy val squeryl = project("squeryl", "squeryl", new CorpusSubproject(_)) /* git 0286ac918fd2e74f8f65e1cafe693e4d8f15f4ec */
}


object Dummy { //just so sbt doesn't complain about multiple project definitions
  class CorpusSubproject(info: ProjectInfo) extends DefaultProject(info) {
    override def compileOptions =
      super.compileOptions ++ Seq("-deprecation", "-unchecked").map(CompileOption(_))
    override def javaCompileOptions = JavaCompileOption("-Xlint:unchecked") :: super.javaCompileOptions.toList
  }
}
