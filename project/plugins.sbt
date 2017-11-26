scalacOptions ++= Seq("-unchecked", "-deprecation")
ivyLoggingLevel := UpdateLogging.Quiet
libraryDependencies += "org.scala-sbt" % "scripted-plugin" % sbtVersion.value

addSbtPlugin("com.fommil" % "sbt-sensible" % "2.2.1")

// sbt-ensime is needed for the integration tests
addSbtPlugin("org.ensime" % "sbt-ensime" % "2.1.0")

addSbtPlugin(
  "com.eed3si9n" % "sbt-assembly" % "0.14.5" exclude ("org.apache.maven", "maven-plugin-api")
)
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")
