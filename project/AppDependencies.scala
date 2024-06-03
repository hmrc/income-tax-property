import sbt.*

object AppDependencies {

  private val bootstrapVersion = "8.4.0"
  private val hmrcMongoVersion = "1.7.0"
  private val monocleVersion = "3.2.0"
  val jacksonAndPlayExclusions: Seq[ExclusionRule] = Seq(
    ExclusionRule(organization = "com.fasterxml.jackson.core"),
    ExclusionRule(organization = "com.fasterxml.jackson.datatype"),
    ExclusionRule(organization = "com.fasterxml.jackson.module"),
    ExclusionRule(organization = "com.fasterxml.jackson.core:jackson-annotations"),
    ExclusionRule(organization = "com.typesafe.play")
  )

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"                   %% "bootstrap-backend-play-30"  % bootstrapVersion,
    "uk.gov.hmrc.mongo"             %% "hmrc-mongo-play-30"         % hmrcMongoVersion,
    "org.typelevel" %% "cats-core" % "2.12.0",
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.17.0",
    "com.beachape"                  %% "enumeratum"                 % "1.7.3",
    "dev.optics" %% "monocle-core" % monocleVersion,
    "dev.optics" %% "monocle-macro" % monocleVersion,
    "com.beachape" %% "enumeratum-play-json" % "1.8.0" excludeAll (jacksonAndPlayExclusions *)
  )


  val test: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"             %% "bootstrap-test-play-30"     % bootstrapVersion              % Test,
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-test-play-30"    % hmrcMongoVersion              % Test,
    "org.scalamock" %% "scalamock" % "6.0.0" % Test,
    "org.scalatest" %% "scalatest" % "3.2.18" % Test,
    "org.scalacheck" %% "scalacheck" % "1.18.0" % Test,
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % Test,
    "com.vladsch.flexmark" % "flexmark-all" % "0.64.8" % Test,
    "com.github.tomakehurst" % "wiremock-jre8-standalone" % "3.0.1" % Test
  )
}
