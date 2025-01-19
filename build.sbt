import play.sbt.routes.RoutesKeys
import uk.gov.hmrc.DefaultBuildSettings

lazy val coverageSettings: Seq[Setting[?]] = {
  import scoverage.ScoverageKeys

  val excludedPackages = Seq(
    "<empty>",
    ".*Reverse.*",
    ".*standardError*.*",
    ".*govuk_wrapper*.*",
    ".*main_template*.*",
    "uk.gov.hmrc.BuildInfo",
    "app.*",
    "prod.*",
    "config.*",
    "testOnly.*",
    "testOnlyDoNotUseInAppConf.*",
    "controllers.testOnly.*",
    "models.*",
    "logging.*",
    "utils.*",
    "Mock*.scala",
    ".*feedback*.*",
    "partials.*",
    "app.Routes",
    "ukAndForeignProperty.Routes",
    "health.Routes",
    "foreign.Routes",
    "foreign.RoutesPrefix"
  )

  Seq(
    ScoverageKeys.coverageExcludedPackages := excludedPackages.mkString(";"),
    ScoverageKeys.coverageMinimumStmtTotal := 80,
    ScoverageKeys.coverageFailOnMinimum := true,
    ScoverageKeys.coverageHighlighting := true
  )
}

Global / scalacOptions += "-Ymacro-annotations"

inThisBuild(
  List(
    majorVersion := 0,
    scalaVersion := "2.13.12",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

lazy val microservice = Project("income-tax-property", file("."))
  .enablePlugins(play.sbt.PlayScala, SbtDistributablesPlugin)
  .settings(
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test,
    scalacOptions += "-Wconf:cat=unused-imports&src=html/.*:s",
    scalacOptions += "-Wconf:src=routes/.*:s"
  )
  .settings(
    resolvers += Resolver.jcenterRepo,
    RoutesKeys.routesImport ++= Seq(
      "models.common._",
      "uk.gov.hmrc.play.bootstrap.binders.RedirectUrl"
    )
  )
  .settings(PlayKeys.playDefaultPort := 19160)
  .disablePlugins(sbt.plugins.JUnitXmlReportPlugin)
  .disablePlugins(JUnitXmlReportPlugin)
  .settings(coverageSettings *)

lazy val it = project
  .enablePlugins(PlayScala)
  .dependsOn(microservice % "test->test") // the "test->test" allows reusing test code and test dependencies
  .settings(DefaultBuildSettings.itSettings())
