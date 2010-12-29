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
    lazy val EmbeddedRepo         = MavenRepository("Embedded Repo", (info.projectPath / "embedded-repo").asURL.toString)
    lazy val LocalMavenRepo       = MavenRepository("Local Maven Repo", (Path.userHome / ".m2" / "repository").asURL.toString)
    lazy val AkkaRepo             = MavenRepository("Akka Repository", "http://scalablesolutions.se/akka/repository")
    lazy val CodehausRepo         = MavenRepository("Codehaus Repo", "http://repository.codehaus.org")
    lazy val GuiceyFruitRepo      = MavenRepository("GuiceyFruit Repo", "http://guiceyfruit.googlecode.com/svn/repo/releases/")
    lazy val JBossRepo            = MavenRepository("JBoss Repo", "http://repository.jboss.org/nexus/content/groups/public/")
    lazy val JavaNetRepo          = MavenRepository("java.net Repo", "http://download.java.net/maven/2")
    lazy val SonatypeSnapshotRepo = MavenRepository("Sonatype OSS Repo", "http://oss.sonatype.org/content/repositories/releases")
    lazy val GlassfishRepo        = MavenRepository("Glassfish Repo", "http://download.java.net/maven/glassfish")
    lazy val ScalaToolsRelRepo    = MavenRepository("Scala Tools Releases Repo", "http://scala-tools.org/repo-releases")
    lazy val DatabinderRepo       = MavenRepository("Databinder Repo", "http://databinder.net/repo")
    lazy val embeddedRepo            = EmbeddedRepo
    lazy val localMavenRepo          = LocalMavenRepo
    lazy val jettyModuleConfig       = ModuleConfiguration("org.eclipse.jetty", sbt.DefaultMavenRepository)
    lazy val guiceyFruitModuleConfig = ModuleConfiguration("org.guiceyfruit", GuiceyFruitRepo)
    lazy val glassfishModuleConfig   = ModuleConfiguration("org.glassfish", GlassfishRepo)
    lazy val jbossModuleConfig       = ModuleConfiguration("org.jboss", JBossRepo)
    lazy val jerseyContrModuleConfig = ModuleConfiguration("com.sun.jersey.contribs", JavaNetRepo)
    lazy val jerseyModuleConfig      = ModuleConfiguration("com.sun.jersey", JavaNetRepo)
    lazy val multiverseModuleConfig  = ModuleConfiguration("org.multiverse", CodehausRepo)
    lazy val nettyModuleConfig       = ModuleConfiguration("org.jboss.netty", JBossRepo)
    lazy val scalaTestModuleConfig   = ModuleConfiguration("org.scalatest", ScalaToolsRelRepo)
    lazy val logbackModuleConfig     = ModuleConfiguration("ch.qos.logback", sbt.DefaultMavenRepository)
    lazy val spdeModuleConfig        = ModuleConfiguration("us.technically.spde", DatabinderRepo)
    lazy val processingModuleConfig  = ModuleConfiguration("org.processing", DatabinderRepo)
    lazy val DISPATCH_VERSION      = "0.7.4"
    lazy val HAWT_DISPATCH_VERSION = "1.0"
    lazy val JACKSON_VERSION       = "1.4.3"
    lazy val JERSEY_VERSION        = "1.3"
    lazy val MULTIVERSE_VERSION    = "0.6.1"
    lazy val SCALATEST_VERSION     = "1.2"
    lazy val LOGBACK_VERSION       = "0.9.24"
    lazy val SLF4J_VERSION         = "1.6.0"
    lazy val JETTY_VERSION         = "7.1.6.v20100715"
    lazy val JAVAX_SERVLET_VERSION = "3.0"
    object Dependencies {
      lazy val aopalliance = "aopalliance" % "aopalliance" % "1.0" % "compile"
      lazy val aspectwerkz = "org.codehaus.aspectwerkz" % "aspectwerkz" % "2.2.3" % "compile"
      lazy val commonsHttpClient = "commons-httpclient" % "commons-httpclient" % "3.1" % "compile"
      lazy val commons_codec = "commons-codec" % "commons-codec" % "1.4" % "compile"
      lazy val commons_io = "commons-io" % "commons-io" % "1.4" % "compile"
      lazy val commons_pool = "commons-pool" % "commons-pool" % "1.5.4" % "compile"
      lazy val configgy = "net.lag" % "configgy" % "2.0.2-nologgy" % "compile"
      lazy val dispatch_http = "net.databinder" % "dispatch-http_2.8.0" % DISPATCH_VERSION % "compile"
      lazy val dispatch_json = "net.databinder" % "dispatch-json_2.8.0" % DISPATCH_VERSION % "compile"
      lazy val javax_servlet_30 = "org.glassfish" % "javax.servlet" % JAVAX_SERVLET_VERSION % "provided"
      lazy val jetty         = "org.eclipse.jetty" % "jetty-server"  % JETTY_VERSION % "compile"
      lazy val jetty_util    = "org.eclipse.jetty" % "jetty-util"    % JETTY_VERSION % "compile"
      lazy val jetty_xml     = "org.eclipse.jetty" % "jetty-xml"     % JETTY_VERSION % "compile"
      lazy val jetty_servlet = "org.eclipse.jetty" % "jetty-servlet" % JETTY_VERSION % "compile"
      lazy val uuid       = "com.eaio" % "uuid" % "3.2" % "compile"
      lazy val guicey = "org.guiceyfruit" % "guice-all" % "2.0" % "compile"
      lazy val h2_lzf = "voldemort.store.compress" % "h2-lzf" % "1.0" % "compile"
      lazy val hawtdispatch = "org.fusesource.hawtdispatch" % "hawtdispatch-scala" % HAWT_DISPATCH_VERSION % "compile"
      lazy val jackson          = "org.codehaus.jackson" % "jackson-mapper-asl" % JACKSON_VERSION % "compile"
      lazy val jackson_core     = "org.codehaus.jackson" % "jackson-core-asl"   % JACKSON_VERSION % "compile"
      lazy val jersey         = "com.sun.jersey"          % "jersey-core"   % JERSEY_VERSION % "compile"
      lazy val jersey_json    = "com.sun.jersey"          % "jersey-json"   % JERSEY_VERSION % "compile"
      lazy val jersey_server  = "com.sun.jersey"          % "jersey-server" % JERSEY_VERSION % "compile"
      lazy val jersey_contrib = "com.sun.jersey.contribs" % "jersey-scala"  % JERSEY_VERSION % "compile"
      lazy val jsr166x = "jsr166x" % "jsr166x" % "1.0" % "compile"
      lazy val jsr250 = "javax.annotation" % "jsr250-api" % "1.0" % "compile"
      lazy val jsr311 = "javax.ws.rs" % "jsr311-api" % "1.1" % "compile"
      lazy val multiverse = "org.multiverse" % "multiverse-alpha" % MULTIVERSE_VERSION % "compile" intransitive
      lazy val multiverse_test = "org.multiverse" % "multiverse-alpha" % MULTIVERSE_VERSION % "test" intransitive
      lazy val netty = "org.jboss.netty" % "netty" % "3.2.3.Final" % "compile"
      lazy val protobuf = "com.google.protobuf" % "protobuf-java" % "2.3.0" % "compile"
      lazy val sbinary = "sbinary" % "sbinary" % "2.8.0-0.3.1" % "compile"
      lazy val sjson = "sjson.json" % "sjson" % "0.8-2.8.0" % "compile"
      lazy val sjson_test = "sjson.json" % "sjson" % "0.8-2.8.0" % "test"
      lazy val slf4j       = "org.slf4j" % "slf4j-api"     % SLF4J_VERSION % "compile"
      lazy val logback      = "ch.qos.logback" % "logback-classic" % LOGBACK_VERSION % "compile"
      lazy val logback_core = "ch.qos.logback" % "logback-core" % LOGBACK_VERSION % "compile"
      lazy val stax_api = "javax.xml.stream" % "stax-api" % "1.0-2" % "compile"
      lazy val thrift = "com.facebook" % "thrift" % "r917130" % "compile"
      lazy val google_coll    = "com.google.collections" % "google-collections"  % "1.0"             % "compile"
      lazy val commons_coll   = "commons-collections"    % "commons-collections" % "3.2.1"           % "test"
      lazy val testJetty      = "org.eclipse.jetty"      % "jetty-server"   
      lazy val testJettyWebApp= "org.eclipse.jetty"      % "jetty-webapp"        % JETTY_VERSION     % "test"
      lazy val junit          = "junit"                  % "junit"               % "4.5"             % "test"
      lazy val mockito        = "org.mockito"            % "mockito-all"         % "1.8.1"           % "test"
      lazy val scalatest      = "org.scalatest"          % "scalatest"           % SCALATEST_VERSION % "test"
    }

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

    class AkkaHttpProject(info: ProjectInfo) extends DefaultProject(info) {
      val jsr250           = Dependencies.jsr250
      val javax_servlet30  = Dependencies.javax_servlet_30
      val jetty            = Dependencies.jetty
      val jetty_util       = Dependencies.jetty_util
      val jetty_xml        = Dependencies.jetty_xml
      val jetty_servlet    = Dependencies.jetty_servlet
      val jackson_core     = Dependencies.jackson_core
      val jersey           = Dependencies.jersey
      val jersey_contrib   = Dependencies.jersey_contrib
      val jersey_json      = Dependencies.jersey_json
      val jersey_server    = Dependencies.jersey_server
      val jsr311           = Dependencies.jsr311
      val stax_api         = Dependencies.stax_api
      val junit     = Dependencies.junit
      val mockito   = Dependencies.mockito
      val scalatest = Dependencies.scalatest
    }

    class AkkaRemoteProject(info: ProjectInfo) extends DefaultProject(info) {
      val commons_codec = Dependencies.commons_codec
      val commons_io    = Dependencies.commons_io
      val dispatch_http = Dependencies.dispatch_http
      val dispatch_json = Dependencies.dispatch_json
      val guicey        = Dependencies.guicey
      val h2_lzf        = Dependencies.h2_lzf
      val jackson       = Dependencies.jackson
      val jackson_core  = Dependencies.jackson_core
      val netty         = Dependencies.netty
      val protobuf      = Dependencies.protobuf
      val sbinary       = Dependencies.sbinary
      val sjson         = Dependencies.sjson
      val junit     = Dependencies.junit
      val scalatest = Dependencies.scalatest
    }

    class AkkaTypedActorProject(info: ProjectInfo) extends DefaultProject(info) {
      val aopalliance  = Dependencies.aopalliance
      val aspectwerkz  = Dependencies.aspectwerkz
      val guicey       = Dependencies.guicey
      val junit     = Dependencies.junit
      val scalatest = Dependencies.scalatest
    }


    class AkkaStmProject(info: ProjectInfo) extends DefaultProject(info) {
      val multiverse = Dependencies.multiverse
      val junit     = Dependencies.junit
      val scalatest = Dependencies.scalatest
    }


    class AkkaActorProject(info: ProjectInfo) extends DefaultProject(info) {
      val uuid          = Dependencies.uuid
      val configgy      = Dependencies.configgy
      val hawtdispatch  = Dependencies.hawtdispatch
      val jsr166x       = Dependencies.jsr166x
      val slf4j         = Dependencies.slf4j
      val logback       = Dependencies.logback
      val logback_core  = Dependencies.logback_core
      val junit           = Dependencies.junit
      val scalatest       = Dependencies.scalatest
      val multiverse_test = Dependencies.multiverse_test
    }
  }

  class SmileProject(info: ProjectInfo) extends com.twitter.sbt.StandardProject(info) {
    val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5" % "provided"
    val xrayspecs = "com.twitter" %% "xrayspecs_2.8.0" % "2.0"
    val vscaladoc = "org.scala-tools" % "vscaladoc" % "1.1-md-3"
    val hamcrest  = "org.hamcrest" % "hamcrest-all" % "1.1"
    val jmock     = "org.jmock" % "jmock" % "2.4.0"
    val objenesis = "org.objenesis" % "objenesis" % "1.1"
    val configgy = "net.lag" % "configgy" % "2.0.0"
    val naggati = "net.lag" %% "naggati" % "0.7.4"
    val mina = "org.apache.mina" % "mina-core" % "2.0.0-M6"
    val slf4j_api = "org.slf4j" % "slf4j-api" % "1.5.2"
    val slf4j_jdk14 = "org.slf4j" % "slf4j-jdk14" % "1.5.2"
  }

  class FlashupProject(info: ProjectInfo) extends DefaultProject(info) {
    val scalatest = "org.scalatest" % "scalatest" % "1.2"
    val jsap = "com.martiansoftware" % "jsap" % "2.1"
  }

  class EnsimeProject(info: ProjectInfo) extends DefaultProject(info) {
    val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
    val jbossRepo = "JBoss Maven 2 Repo" at "http://repository.jboss.org/maven2"
    val ant = "org.apache.ant" % "ant" % "1.8.1" % "compile;runtime;test"
    val ivy = "org.apache.ivy" % "ivy" % "2.1.0" % "compile;runtime;test"
    val maven = "org.apache.maven" % "maven-ant-tasks" % "2.1.0" % "compile;runtime;test"
    val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
    val jdt = "org.eclipse.jdt" % "core" % "3.4.2.v_883_R34x" % "compile;runtime;test"
    val scalariform = "org.scalariform" % "scalariform_2.8.0" % "0.0.7"%"compile;runtime;test"
    val refactoring = "org.scala-refactoring" % "org.scala-refactoring.library" % "compile;runtime;test" from "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring.library/0.3.0-SNAPSHOT/org.scala-refactoring.library-0.3.0-20101213.203503-7.jar"
    val asm = "asm" % "asm" % "3.2"
    val asmCommons = "asm" % "asm-commons" % "3.2"
  }

  class ScalaSwingProject(info: ProjectInfo) extends DefaultProject(info)

  class SquerylProject(info: ProjectInfo) extends DefaultProject(info) {
    val cglib = "cglib" % "cglib-nodep" % "2.2"
    val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5" % "provided"  
  }

  class ScalazProject(info: ProjectInfo) extends DefaultProject(info) with ScalazBoilerplate {
    val servlets = "javax.servlet" % "servlet-api" % "2.5" withSources
    // val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"
    // val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
    // val specs = "org.scala-tools.testing" %% "specs" % "1.6.7-SNAPSHOT" % "test" withSources
    override def mainSourceRoots = super.mainSourceRoots +++ srcManagedScala##
    override def compileAction = super.compileAction dependsOn(generateTupleW)
  }

  class ScalaQueryProject(info: ProjectInfo) extends DefaultProject(info) {
    val junitInterface = "com.novocode" % "junit-interface" % "0.5" % "test"
  }
}
