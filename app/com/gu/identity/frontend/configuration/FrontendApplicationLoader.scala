package com.gu.identity.frontend.configuration

import com.gu.identity.frontend.controllers._
import com.gu.identity.frontend.csrf.CSRFConfig
import com.gu.identity.frontend.errors.ErrorHandler
import com.gu.identity.frontend.filters.{SecurityHeadersFilter, Filters}
import com.gu.identity.frontend.services.{GoogleRecaptchaServiceHandler, IdentityServiceRequestHandler, IdentityServiceImpl, IdentityService}
import com.gu.identity.service.client.IdentityClient
import jp.co.bizreach.play2handlebars.HandlebarsPlugin
import play.api.i18n.I18nComponents
import play.api.routing.Router
import play.filters.gzip.GzipFilter
import router.Routes
import play.api.libs.ws.ning.NingWSComponents
import play.api.{Mode, Logger, BuiltInComponentsFromContext, ApplicationLoader}
import play.api.ApplicationLoader.Context
import com.mohiva.play.htmlcompressor.DefaultHTMLCompressorFilter

class FrontendApplicationLoader extends ApplicationLoader {
  def load(context: Context) = {
    val app = new ApplicationComponents(context).application
    new HandlebarsPlugin(app)
    app
  }
}

class ApplicationComponents(context: Context) extends BuiltInComponentsFromContext(context) with NingWSComponents with I18nComponents {
  lazy val frontendConfiguration = Configuration(configuration)
  lazy val csrfConfig = CSRFConfig(configuration)

  lazy val identityServiceRequestHandler = new IdentityServiceRequestHandler(wsClient)
  lazy val identityClient: IdentityClient = new IdentityClient
  lazy val identityService: IdentityService = new IdentityServiceImpl(frontendConfiguration, identityServiceRequestHandler, identityClient)

  lazy val applicationController = new Application(frontendConfiguration, messagesApi, csrfConfig)
  lazy val healthcheckController = new HealthCheck()
  lazy val manifestController = new Manifest()
  lazy val cspReporterController = new CSPViolationReporter()
  lazy val googleRecaptchaServiceHandler = new GoogleRecaptchaServiceHandler(wsClient, frontendConfiguration)
  lazy val googleRecaptchaCheck = new GoogleRecaptchaCheck(googleRecaptchaServiceHandler)
  lazy val signinController = new SigninAction(identityService, messagesApi, csrfConfig, googleRecaptchaCheck, frontendConfiguration)
  lazy val registerController = new RegisterAction(identityService, messagesApi, frontendConfiguration, csrfConfig)
  lazy val resetPasswordController = new ResetPasswordAction(identityService, csrfConfig)
  lazy val assets = new controllers.Assets(httpErrorHandler)

  override lazy val httpFilters = new Filters(new SecurityHeadersFilter(
    frontendConfiguration),
    new GzipFilter(),
    new DefaultHTMLCompressorFilter(configuration, environment)
  ).filters

  override lazy val httpErrorHandler = new ErrorHandler(frontendConfiguration, messagesApi, environment, sourceMapper, Some(router))

  // Makes sure the logback.xml file is being found in DEV environments
  if(environment.mode == Mode.Dev) {
    Logger.configure(environment)
  }

  override lazy val router: Router = new Routes(httpErrorHandler, applicationController, signinController, registerController, resetPasswordController, cspReporterController, healthcheckController, manifestController, assets)
}
