package com.gu.identity.frontend.controllers


import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.csrf.{CSRFConfig, CSRFCheck}
import com.gu.identity.frontend.errors.RedirectOnError
import com.gu.identity.frontend.logging.{MetricsLoggingActor, Logging}
import com.gu.identity.frontend.models.{ClientID, UrlBuilder, ClientIp, TrackingData, ReturnUrl}
import com.gu.identity.frontend.request.RegisterActionRequestBody
import com.gu.identity.frontend.services.{ServiceAction, IdentityService}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Cookie => PlayCookie, Controller}



class RegisterAction(identityService: IdentityService, val messagesApi: MessagesApi, val config: Configuration, csrfConfig: CSRFConfig) extends Controller with Logging with MetricsLoggingActor with I18nSupport {

  val redirectRoute: String = routes.Application.register().url

  val RegisterServiceAction =
    ServiceAction andThen
    RedirectOnError(redirectRoute) andThen
    CSRFCheck(csrfConfig)

  val bodyParser = RegisterActionRequestBody.bodyParser(config)

  def register = RegisterServiceAction(bodyParser) { request =>
    val clientIp = ClientIp(request)
    val body = request.body

    val trackingData = TrackingData(request, body.returnUrl)
    identityService.registerThenSignIn(body, clientIp, trackingData).map {
      case Left(errors) =>
        Left(errors)
      case Right(cookies) => Right {
        registerSuccessRedirectUrl(cookies, body.returnUrl, body.skipConfirmation, body.group, body.clientId)
      }
    }
  }

  private def registerSuccessRedirectUrl(cookies: Seq[PlayCookie], returnUrl: ReturnUrl, skipConfirmation: Option[Boolean], group: Option[String], clientId: Option[ClientID]) = {
    (group, skipConfirmation.getOrElse(false)) match {
      case(Some(group), false) => {
        val skipConfirmationUrl = UrlBuilder(routes.Application.confirm(), Seq("returnUrl" -> returnUrl.url))
        val url = build3rdPartyUrl(group, skipConfirmationUrl, skipConfirmation = false, clientId)
        registerSuccessResult(url, cookies)
      }
      case(Some(group), true) => {
        val url = build3rdPartyUrl(group, returnUrl.url, skipConfirmation = true, clientId)
        registerSuccessResult(url, cookies)
      }
      case (None, false) => {
        val url = UrlBuilder(routes.Application.confirm(), returnUrl, clientId)
        registerSuccessResult(url, cookies)
      }
      case (None, true) => {
        registerSuccessResult(returnUrl.url, cookies)
      }
    }
  }

  private def registerSuccessResult(url: String, cookies: Seq[PlayCookie]) = {
    logSuccessfulRegister
    SeeOther(url).withCookies(cookies: _*)
  }

  private def build3rdPartyUrl(group: String, returnUrl: String, skipConfirmation: Boolean, clientId: Option[ClientID]) = {
    val baseUrl = s"${config.identityProfileBaseUrl}/agree/$group"
    val params = Seq(
      "returnUrl" -> returnUrl,
      "skipConfirmation" -> skipConfirmation.toString,
      "skipThirdPartyLandingPage" -> "true",
      "clientId" -> clientId.map(_.id).getOrElse("")
    )
    UrlBuilder(baseUrl, params)
  }
}
