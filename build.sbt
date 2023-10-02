val ZIOVersion = "2.0.17"

ThisBuild / name         := "functional-effects"
ThisBuild / version      := "0.1.0"
ThisBuild / scalaVersion := "3.3.1"

lazy val root =
	project
		.in(file("."))

ThisBuild / libraryDependencies ++= Seq(
	"com.lihaoyi"   %% "pprint"    % "0.8.1",
	// ZIO
	"dev.zio" %% "zio"          % ZIOVersion,
	"dev.zio" %% "zio-streams"  % ZIOVersion,
	"dev.zio" %% "zio-test"     % ZIOVersion % "test",
	"dev.zio" %% "zio-test-sbt" % ZIOVersion % "test",
	// URL parsing
	"io.lemonlabs" %% "scala-uri" % "4.0.3"
)

ThisBuild / scalacOptions ++= Seq(
	"-encoding", "utf8",
	"-language:implicitConversions",
	"-language:existentials",
	"-deprecation",
	"-feature",
	"-unchecked",
	"-Werror",
)

ThisBuild / watchBeforeCommand := Watch.clearScreen

ThisBuild / shellPrompt := {
	(state: State) =>
		s"sbt:${(ThisBuild / name).value}:" +
			s"${Project.extract(state).currentProject.id}" +
			s"${scala.Console.CYAN}>${scala.Console.RESET}"
}

Compile / run / fork := true
Compile / run / connectInput := true
Compile / run / javaOptions += "-Xmx4G"
