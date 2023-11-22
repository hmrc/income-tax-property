
import play.core.PlayVersion.current

import sbt._

object AppDependencies {

  private val bootstrapVersion = "7.19.0"
  private val hmrcMongoVersion = "1.3.0"

  val compile = Seq(
    "uk.gov.hmrc"                   %% "bootstrap-backend-play-28"  % bootstrapVersion,
    "uk.gov.hmrc.mongo"             %% "hmrc-mongo-play-28"         % hmrcMongoVersion,
    "com.fasterxml.jackson.module"  %% "jackson-module-scala"       % "2.14.2",
    "org.typelevel"                 %% "cats-core"                  % "2.9.0"
  )


  val test = Seq(
    "uk.gov.hmrc"             %% "bootstrap-test-play-28"     % bootstrapVersion % "test, it",
    "com.typesafe.play"       %% "play-test"                  % current                       % Test,
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-test-play-28"    % hmrcMongoVersion              % Test,
    "org.scalamock"           %% "scalamock"                  % "5.2.0"                       % Test,
    "org.scalatest"           %% "scalatest"                  % "3.2.15"                      % Test,
    "com.vladsch.flexmark"    % "flexmark-all"                % "0.64.6"                      % "test, it",
    "com.github.tomakehurst"  % "wiremock-jre8"               % "2.35.0"                      % "test, it"
  )
}
