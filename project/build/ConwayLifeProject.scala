import sbt._

class ConwayLifeProject(info: ProjectInfo) extends DefaultProject(info) {
  override def compileOptions = Deprecation :: Unchecked :: ExplainTypes :: super.compileOptions.toList
  val snapshots = ScalaToolsSnapshots 
  val sc = "org.scala-tools.testing" % "scalacheck_2.8.0.Beta1-RC5" % "1.7-SNAPSHOT"
}