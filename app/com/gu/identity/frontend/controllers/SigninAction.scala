package com.gu.identity.frontend.controllers

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.csrf.{CSRFConfig, CSRFCheck}
import com.gu.identity.frontend.logging.{LogOnErrorAction, MetricsLoggingActor, Logging}
import com.gu.identity.frontend.models.ClientID
import com.gu.identity.frontend.errors.RedirectOnError
import com.gu.identity.frontend.models.{ReturnUrl, TrackingData}
import com.gu.identity.frontend.request.RequestParameters.SignInRequestParameters
import com.gu.identity.frontend.request.SignInActionRequestBody
import com.gu.identity.frontend.services._
import play.api.i18n.{MessagesApi, I18nSupport}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{Request, RequestHeader, Controller}

import scala.concurrent.Future
import play.api.i18n.Messages.Implicits._


/**
 * Form actions controller
 */
class SigninAction(identityService: IdentityService, val messagesApi: MessagesApi, csrfConfig: CSRFConfig, config: Configuration) extends Controller with Logging with MetricsLoggingActor with I18nSupport {

  val redirectRoute: String = routes.Application.signIn().url

  val SignInServiceAction =
    ServiceAction andThen
    RedirectOnError(redirectRoute) andThen
    LogOnErrorAction(logger) andThen
    CSRFCheck(csrfConfig)

  val bodyParser = SignInActionRequestBody.bodyParser(config)

  def signIn = SignInServiceAction(bodyParser) { request: Request[SignInActionRequestBody] =>
    val formParams = request.body

    val trackingData = TrackingData(request, formParams.returnUrl.flatMap(_.toStringOpt))
    lazy val returnUrl = formParams.returnUrl.getOrElse(ReturnUrl.defaultForClient(config, formParams.clientId))

    identityService.authenticate(formParams, trackingData).map {
      case Left(errors) => Left(errors)
      case Right(cookies) => Right {
        logSuccessfulSignin()
        SeeOther(returnUrl.url)
          .withCookies(cookies: _*)
      }
    }
  }

}

