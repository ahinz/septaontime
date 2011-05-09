import sbt._

class SeptaOnTime(info:ProjectInfo) extends ParentProject(info) {

    lazy val core = project("core","core", new Core(_))
    lazy val server = project("server","server", new Server(_), core)
    lazy val router = project("router","router", new Router(_), core)
	lazy val services = project("services","services", new Services(_), core)
    
    class Core(info: ProjectInfo) extends DefaultProject(info) with Deps
    class Server(info: ProjectInfo) extends DefaultProject(info) with Deps
    class Router(info: ProjectInfo) extends DefaultProject(info) with Deps
	class Services(info: ProjectInfo) extends DefaultProject(info) with Deps

    trait Deps {
        val scalaSwing = "org.scala-lang" % "scala-swing" % "2.8.1"
        val sqliteJDBC = "org.xerial" % "sqlite-jdbc" % "3.6.16"
        val scalatest = "org.scalatest" % "scalatest" % "1.3"
        val liftjson = "net.liftweb" % "lift-json_2.8.0" % "2.3"
    }

  }

// vim: set ts=4 sw=4 et:
