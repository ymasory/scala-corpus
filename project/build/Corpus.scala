import sbt._

class ScalaCorpusProject(info: ProjectInfo) extends DefaultProject(info) {

  /* projects included in scala-corpus */
  lazy val scalaz = project("scalaz", "scalaz", new ScalazProject(_)) /* git 32757b4ed1f59dfbd121c9e483f06dada46ff792 */
  lazy val scalaquery = project("scala-query", "scala-query", new ScalaQueryProject(_)) /* git 0b9f9adfa15716ba4e5a659324000706ee1e42f7 */
  lazy val squeryl = project("squeryl", "squeryl", new SquerylProject(_)) /* git 0286ac918fd2e74f8f65e1cafe693e4d8f15f4ec */
  lazy val scalaswing = project("scala-swing", "scala-swing", new ScalaSwingProject(_)) /* git 5b45ba65a6afa15b6083bc4c0654d551a379e9a3 */
  //lazy val ensime = project("ensime", "ensime", new EnsimeProject(_)) /* git b72d66ee73661735bc5a61e0d23874bbcaced76e */
  lazy val flashup = project("flashup", "flashup", new FlashupProject(_)) /* git 61aaf4159ef739068295efa03dd08ccbde76b600 */
  lazy val smile = project("smile", "smile", new SmileProject(_)) /* git ee41070f87e2a99b2556ebde6b505c3dd54e7b88 */
  lazy val akka = project("akka", "akka", new AkkaProject(_)) /* git 57d0e85a9adeb088fbe4b2cb7140647cdbc1c432 */

  /* projects definitions, one for each project included in scala-corpus */
  class AkkaProject(info: ProjectInfo) extends DefaultProject(info) {
    lazy val akka_actor       = project("akka-actor",       "akka-actor",       new AkkaActorProject(_))
    lazy val akka_stm         = project("akka-stm",         "akka-stm",         new AkkaStmProject(_),        akka_actor)
    lazy val akka_typed_actor = project("akka-typed-actor", "akka-typed-actor", new AkkaTypedActorProject(_), akka_stm)
    lazy val akka_remote      = project("akka-remote",      "akka-remote",      new AkkaRemoteProject(_),     akka_typed_actor)
    lazy val akka_http        = project("akka-http",        "akka-http",        new AkkaHttpProject(_),       akka_remote)
    lazy val akka_samples     = project("akka-samples",     "akka-samples",     new AkkaSamplesParentProject(_))
    class AkkaSamplesParentProject(info: ProjectInfo) extends DefaultProject(info) {
      lazy val akka_sample_ants = project("akka-sample-ants", "akka-sample-ants", new AkkaSampleAntsProject(_), akka_stm)
      lazy val akka_sample_fsm = project("akka-sample-fsm", "akka-sample-fsm", new AkkaSampleFSMProject(_), akka_actor)
      lazy val akka_sample_remote = project("akka-sample-remote", "akka-sample-remote", new AkkaSampleRemoteProject(_), akka_remote)
      class AkkaSampleAntsProject(info: ProjectInfo) extends DefaultProject(info)
      class AkkaSampleRemoteProject(info: ProjectInfo) extends DefaultProject(info)
      class AkkaSampleFSMProject(info: ProjectInfo) extends DefaultProject(info)
    }
    class AkkaHttpProject(info: ProjectInfo) extends DefaultProject(info)
    class AkkaRemoteProject(info: ProjectInfo) extends DefaultProject(info)
    class AkkaTypedActorProject(info: ProjectInfo) extends DefaultProject(info)
    class AkkaStmProject(info: ProjectInfo) extends DefaultProject(info)
    class AkkaActorProject(info: ProjectInfo) extends DefaultProject(info)
  }

  class SmileProject(info: ProjectInfo) extends com.twitter.sbt.StandardProject(info)
  class FlashupProject(info: ProjectInfo) extends DefaultProject(info)
  class EnsimeProject(info: ProjectInfo) extends DefaultProject(info)
  class ScalaSwingProject(info: ProjectInfo) extends DefaultProject(info)
  class SquerylProject(info: ProjectInfo) extends DefaultProject(info)
  class ScalazProject(info: ProjectInfo) extends DefaultProject(info) with ScalazBoilerplate {
    override def mainSourceRoots = super.mainSourceRoots +++ srcManagedScala##
    override def compileAction = super.compileAction dependsOn(generateTupleW)
  }

  class ScalaQueryProject(info: ProjectInfo) extends DefaultProject(info)
}
