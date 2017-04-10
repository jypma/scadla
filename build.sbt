name := "scadla"

organization := "io.github.dzufferey"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.1"

scalacOptions in Compile ++= Seq(
    //"-Xlog-implicits",
    "-unchecked",
    "-deprecation",
    "-feature",
    "-Xmax-classfile-name", "110"
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "io.github.dzufferey" %% "misc-scala-utils" % "0.1-SNAPSHOT",
  "com.twitter" %% "chill" % "0.9.1",
  "org.scalafx" %% "scalafx" % "8.0.102-R11",
  "eu.mihosoft.vrl.jcsg" % "jcsg" % "0.5.2"
)

//addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")

fork := true

resolvers +=  "dzufferey maven repo" at "https://github.com/dzufferey/my_mvn_repo/raw/master/repository"

publishMavenStyle := true

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

pomExtra :=
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>https://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
    </license>
  </licenses>

