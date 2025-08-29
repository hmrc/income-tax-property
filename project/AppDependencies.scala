import sbt.*

object AppDependencies {

  private val bootstrapVersion = "10.1.0"
  private val hmrcMongoVersion = "2.7.0"
  private val monocleVersion = "3.3.0"
  val jacksonAndPlayExclusions: Seq[ExclusionRule] = Seq(
    ExclusionRule(organization = "com.fasterxml.jackson.core"),
    ExclusionRule(organization = "com.fasterxml.jackson.datatype"),
    ExclusionRule(organization = "com.fasterxml.jackson.module"),
    ExclusionRule(organization = "com.fasterxml.jackson.core:jackson-annotations"),
    ExclusionRule(organization = "com.typesafe.play")
  )

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"                  %% "bootstrap-backend-play-30" % bootstrapVersion,
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-play-30"        % hmrcMongoVersion,
    "org.typelevel"                %% "cats-core"                 % "2.13.0",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"      % "2.19.2",
    "com.beachape"                 %% "enumeratum"                % "1.9.0",
    "com.beachape"                 %% "enumeratum-play-json"      % "1.9.0" excludeAll (jacksonAndPlayExclusions *),
    "dev.optics"                   %% "monocle-core"              % monocleVersion,
    "dev.optics"                   %% "monocle-macro"             % monocleVersion
  )

  val test: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"           %% "bootstrap-test-play-30"   % bootstrapVersion,
    "uk.gov.hmrc.mongo"     %% "hmrc-mongo-test-play-30"  % hmrcMongoVersion,
    "org.scalamock"         %% "scalamock"                % "7.4.1",
    "org.scalatest"         %% "scalatest"                % "3.2.19",
    "org.scalacheck"        %% "scalacheck"               % "1.18.1",
    "org.scalatestplus"     %% "scalacheck-1-15"          % "3.2.11.0",
    "com.vladsch.flexmark"   % "flexmark-all"             % "0.64.8",
    "com.github.tomakehurst" % "wiremock-jre8-standalone" % "3.0.1"
  ).map(_ % "test")
}
