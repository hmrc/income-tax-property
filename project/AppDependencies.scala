import sbt.*

object AppDependencies {

  private val bootstrapVersion = "8.4.0"
  private val hmrcMongoVersion = "1.7.0"

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
    "com.fasterxml.jackson.module"  %% "jackson-module-scala"       % "2.14.2",
    "com.beachape"                  %% "enumeratum"                 % "1.7.3",
    "com.beachape"                  %% "enumeratum-play-json"      % "1.7.3" excludeAll (jacksonAndPlayExclusions *)
  )


  val test: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"             %% "bootstrap-test-play-30"     % bootstrapVersion              % Test,
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-test-play-30"    % hmrcMongoVersion              % Test,
    "org.scalamock"           %% "scalamock"                  % "5.2.0"                       % Test,
    "org.scalatest"           %% "scalatest"                  % "3.2.15"                      % Test,
    "org.scalatestplus"       %% "scalacheck-1-15"            % "3.2.10.0"                    % Test,
    "com.vladsch.flexmark"    % "flexmark-all"                % "0.64.6"                      % Test,
    "com.github.tomakehurst"  % "wiremock-jre8-standalone"    % "2.35.1"                      % Test
  )
}
