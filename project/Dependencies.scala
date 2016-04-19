import sbt._

object Dependencies {

  val akkaVersion = "2.4.3"
  val akka = Seq(
    "akka-actor",
    "akka-slf4j",
    "akka-agent",
    "akka-testkit",
    "akka-remote",
    "akka-cluster",
    "akka-cluster-metrics",
    "akka-cluster-tools"
  ).map("com.typesafe.akka" %% _ % akkaVersion)

  val config = Seq(
    "com.typesafe" % "config" % "1.2.1",
    "net.ceedubs" %% "ficus" % "1.1.2"
  )

  val random = Seq("org.apache.commons" % "commons-math3" % "3.4.1")

  val logging = Seq("ch.qos.logback" % "logback-classic" % "1.1.2")

  val test = Seq(
    "org.scalatest" %% "scalatest" % "2.2.3",
    "org.mockito" % "mockito-core" % "1.10.19",
    "org.scalamock" %% "scalamock-specs2-support" % "3.2.1",
    "org.scalacheck" %% "scalacheck" % "1.12.1"
  ).map(_ % "test")

  val geneticDeps = config ++ random

  val emasDeps = config ++ logging

  val coreDeps = config ++ random ++ logging ++ akka ++ test
}
