package com.gu.identity.frontend.controllers

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.csrf.{CSRFConfig, CSRFCheck}
import com.gu.identity.frontend.logging.{MetricsLoggingActor, Logging}
import com.gu.identity.frontend.models.ClientID
import com.gu.identity.frontend.errors.RedirectOnError
import com.gu.identity.frontend.models.{ReturnUrl, TrackingData}
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

  final val SignInServiceAction =
    ServiceAction andThen
      RedirectOnError(redirectRoute) andThen
      CSRFCheck(csrfConfig)


  def signIn = SignInServiceAction(SignInActionRequestBody.parser) { request: Request[SignInActionRequestBody] =>
    val formParams = request.body

    val trackingData = TrackingData(request, formParams.returnUrl)
    val returnUrl = ReturnUrl(formParams.returnUrl, request.headers.get("Referer"), config, formParams.clientID)

    identityService.authenticate(formParams.email, formParams.password, formParams.rememberMe, trackingData).map {
      case Left(errors) => Left(errors)
      case Right(cookies) => Right {
        SeeOther(returnUrl.url)
          .withCookies(cookies: _*)
      }
    }
  }

}

