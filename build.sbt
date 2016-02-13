lazy val root = (project in file(".")).settings(
  name := "Write Yourself a Scheme",
  scalaVersion := "2.11.7",
  libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.1.1",
    "org.tpolecat" %% "atto-core"  % "0.4.2")
)
