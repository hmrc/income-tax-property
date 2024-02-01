import sbt.*

object AppDependencies {

  private val bootstrapVersion = "8.4.0"
  private val hmrcMongoVersion = "1.7.0"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"                   %% "bootstrap-backend-play-30"  % bootstrapVersion,
    "uk.gov.hmrc.mongo"             %% "hmrc-mongo-play-30"         % hmrcMongoVersion,
    "com.fasterxml.jackson.module"  %% "jackson-module-scala"       % "2.14.2",
    "org.typelevel"                 %% "cats-core"                  % "2.9.0"
  )


  val test: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"             %% "bootstrap-test-play-30"     % bootstrapVersion % "test, it",
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-test-play-30"    % hmrcMongoVersion              % Test,
    "org.scalamock"           %% "scalamock"                  % "5.2.0"                       % Test,
    "org.scalatest"           %% "scalatest"                  % "3.2.15"                      % Test,
    "com.vladsch.flexmark"    % "flexmark-all"                % "0.64.6"                      % "test, it",
    "com.github.tomakehurst"  % "wiremock-jre8"               % "2.35.0"                      % "test, it"
  )
}
