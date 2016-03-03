name := "identity-frontend"

organization := "com.gu.identity"

scalaVersion := "2.11.7"

version := "1.0.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(
  PlayScala, SbtNativePackager, UniversalPlugin, RiffRaffArtifact, BuildInfoPlugin, FrontendBuildPlugin)

lazy val functionalTests = Project("functional-tests", file("functional-tests"))

libraryDependencies ++= Seq(
  "org.scalatestplus" %% "play" % "1.4.0-M3" % "test",
  "org.scalatest" %% "scalatest" % "2.2.5" % "test",
  "org.mockito" % "mockito-core" % "1.9.5" % "test",
  ws,
  filters,
  "jp.co.bizreach" %% "play2-handlebars" % "0.3.0",
  "com.mohiva" %% "play-html-compressor" % "0.5.0",
  "com.typesafe.akka" %% "akka-actor" % "2.4.1",
  "com.typesafe.akka" %% "akka-slf4j" % "2.4.0",
  "com.amazonaws" % "aws-java-sdk-cloudwatch" % "1.10.54"
)

// Set logs options and default local resource for running locally (run and test)
javaOptions ++= Seq("-Dlogs.home=logs", "-Dconfig.resource=DEV.conf")

testOptions in Test += Tests.Argument("-oDF")

// Config for packaging app for deployment with riffraff
packageName in Universal := normalizedName.value

riffRaffPackageType := (packageZipTarball in Universal).value
riffRaffPackageName := name.value
riffRaffManifestProjectName := s"identity:${name.value}"
riffRaffBuildIdentifier := Option(System.getenv("CIRCLE_BUILD_NUM")).getOrElse("DEV")
riffRaffUploadArtifactBucket := Option("riffraff-artifact")
riffRaffUploadManifestBucket := Option("riffraff-builds")

mappings in Universal ++= (baseDirectory.value / "deploy" ***).get pair relativeTo(baseDirectory.value)

// Prout
buildInfoKeys := Seq[BuildInfoKey](
  name,
  BuildInfoKey.constant("gitCommitId", Option(System.getenv("CIRCLE_SHA1")) getOrElse(try {
    "git rev-parse HEAD".!!.trim
  } catch { case e: Exception => "unknown" })),
  BuildInfoKey.constant("buildNumber", Option(System.getenv("CIRCLE_BUILD_NUM")) getOrElse "DEV")
)

// Disable packaging of scaladoc
sources in (Compile, doc) := Seq.empty
publishArtifact in (Compile, packageDoc) := false


play.PlayImport.PlayKeys.playDefaultPort := 8860
routesGenerator := InjectedRoutesGenerator

addCommandAlias("devrun", "run")
