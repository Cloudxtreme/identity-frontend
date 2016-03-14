package com.gu.identity.frontend.controllers


import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.csrf.{CSRFConfig, CSRFCheck}
import com.gu.identity.frontend.errors.AppException
import com.gu.identity.frontend.logging.{MetricsLoggingActor, Logging}
import com.gu.identity.frontend.models.{ClientID, UrlBuilder, ClientIp, TrackingData, ReturnUrl}
import com.gu.identity.frontend.request.RegisterActionRequestBody
import com.gu.identity.frontend.services.IdentityService
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Cookie => PlayCookie, Controller}



class RegisterAction(identityService: IdentityService, val messagesApi: MessagesApi, val config: Configuration, csrfConfig: CSRFConfig) extends Controller with Logging with MetricsLoggingActor with I18nSupport {

  val bodyParser = RegisterActionRequestBody.bodyParser(config)

  def register = CSRFCheck(csrfConfig).async(bodyParser) { request =>
    val clientIp = ClientIp(request)
    val body = request.body

    val trackingData = TrackingData(request, body.returnUrl)
    val returnUrl = ReturnUrl(body.returnUrl, request.headers.get("Referer"), config, body.clientID)
    identityService.registerThenSignIn(body, clientIp, trackingData).map {
      case Left(errors) =>
        redirectToRegisterPageWithErrors(errors, returnUrl, body.skipConfirmation, body.group, body.clientID)
      case Right(cookies) => {
        registerSuccessRedirectUrl(cookies, returnUrl, body.skipConfirmation, body.group, body.clientID)
      }
    }
  }

  private def redirectToRegisterPageWithErrors(errors: Seq[AppException], returnUrl: ReturnUrl, skipConfirmation: Option[Boolean], group: Option[String], clientId: Option[ClientID]) = {

    val idErrors = errors.map {
      error => "error" -> (checkUserDataIsUnique(error))
    }

    val params = Seq(
      Some("returnUrl" -> returnUrl.url),
      skipConfirmation.map("skipConfirmation" -> _.toString),
      group.map("group" -> _),
      clientId.map("clientId" -> _.id)
    ).flatten

    SeeOther(
      UrlBuilder(routes.Application.register(), params ++ idErrors)
    )
  }

  private def checkUserDataIsUnique(error: AppException): String =
    error.id.key

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
