import sbt._

class ConwayLifeProject(info: ProjectInfo) extends DefaultProject(info) {
  override def compileOptions = Deprecation :: Unchecked :: ExplainTypes :: super.compileOptions.toList
  val snapshots = ScalaToolsSnapshots 
  val sc = "org.scala-tools.testing" %% "scalacheck" % "1.9"
  val scalaSwing = "org.scala-lang" % "scala-swing"
  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"
}