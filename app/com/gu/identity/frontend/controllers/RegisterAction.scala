package com.gu.identity.frontend.controllers


import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.csrf.{CSRFConfig, CSRFCheck}
import com.gu.identity.frontend.errors.AppException
import com.gu.identity.frontend.logging.{MetricsLoggingActor, Logging}
import com.gu.identity.frontend.models.{ClientID, UrlBuilder, ClientIp, TrackingData, ReturnUrl}
import com.gu.identity.frontend.models.ClientID.FormMappings.{clientId => clientIdMapping}
import com.gu.identity.frontend.services.IdentityService
import play.api.data.{Mapping, Form}
import play.api.data.Forms._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Cookie => PlayCookie, RequestHeader, Controller}

import scala.concurrent.Future
import scala.util.control.NonFatal

case class RegisterRequest(
    firstName: String,
    lastName: String,
    email: String,
    username: String,
    password: String,
    receiveGnmMarketing: Boolean,
    receive3rdPartyMarketing: Boolean,
    returnUrl: Option[String],
    skipConfirmation: Option[Boolean],
    group: Option[String],
    clientID: Option[ClientID])

class RegisterAction(identityService: IdentityService, val messagesApi: MessagesApi, val config: Configuration, csrfConfig: CSRFConfig) extends Controller with Logging with MetricsLoggingActor with I18nSupport {

  private val username: Mapping[String] = text.verifying(
    "error.username", name => name.matches("[A-z0-9]+") && name.length > 5 && name.length < 21
  )

  private val password: Mapping[String] = text.verifying(
    "error.password", name => name.length > 5 && name.length < 21
  )

  private val group: Mapping[String] = text.verifying(
    "error.group", name => name == "GRS" || name == "GTNF"
  )

  val registerForm = Form(
    mapping(
      "firstName" -> nonEmptyText,
      "lastName" -> nonEmptyText,
      "email" -> email,
      "username" -> username,
      "password" -> password,
      "receiveGnmMarketing" -> boolean,
      "receive3rdPartyMarketing" -> boolean,
      "returnUrl" -> optional(text),
      "skipConfirmation" -> optional(boolean),
      "group" -> optional(group),
      "clientId" -> optional(clientIdMapping)
    )(RegisterRequest.apply)(RegisterRequest.unapply)
  )

  def register = CSRFCheck(csrfConfig).async { implicit request =>
    val clientIp = ClientIp(request)
    registerForm.bindFromRequest.fold(
      errorForm => {
        val errors = errorForm.errors.map(error => s"register-error-${error.key}")
        Future.successful(SeeOther(routes.Application.register(errors).url))},
      successForm => {
        val trackingData = TrackingData(request, successForm.returnUrl)
        val returnUrl = ReturnUrl(successForm.returnUrl, request.headers.get("Referer"), config, successForm.clientID)
        identityService.registerThenSignIn(successForm, clientIp, trackingData).map {
          case Left(errors) =>
            redirectToRegisterPageWithErrors(errors, returnUrl, successForm.skipConfirmation, successForm.group, successForm.clientID)
          case Right(cookies) => {
            registerSuccessRedirectUrl(cookies, returnUrl, successForm.skipConfirmation, successForm.group, successForm.clientID)
          }
        }
      }
    )
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

  private def checkUserDataIsUnique(error: AppException): String = {
    /*error.message match {
      case "Username in use" => "register-error-username-in-use"
      case "Email in use" => "register-error-email-in-use"
      case _ =>*/ s"register-${error.id}"
    //}
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
