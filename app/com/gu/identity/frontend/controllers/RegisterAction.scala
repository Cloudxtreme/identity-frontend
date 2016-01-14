package com.gu.identity.frontend.controllers

import java.net.URLEncoder

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.logging.Logging
import com.gu.identity.frontend.models.{ClientRegistrationIp, TrackingData, ReturnUrl}
import com.gu.identity.frontend.services.{ServiceGatewayError, ServiceError, IdentityService}
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, Controller, Cookie => PlayCookie}

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
    group: Option[String])

class RegisterAction(identityService: IdentityService, val messagesApi: MessagesApi, val config: Configuration) extends Controller with Logging with I18nSupport {

  val registerForm = Form(
    mapping(
      "firstName" -> nonEmptyText,
      "lastName" -> nonEmptyText,
      "email" -> email,
      "username" -> nonEmptyText,
      "password" -> nonEmptyText,
      "receiveGnmMarketing" -> boolean,
      "receive3rdPartyMarketing" -> boolean,
      "returnUrl" -> optional(text),
      "skipConfirmation" -> optional(boolean),
      "group" -> optional(text)
    )(RegisterRequest.apply)(RegisterRequest.unapply)
  )

  def register = Action.async { implicit request =>
    NoCache {
      val clientIp = ClientRegistrationIp(request)
      registerForm.bindFromRequest.fold(
        errorForm => Future.successful(SeeOther(routes.Application.register(Seq("error-registration")).url)),
        successForm => {
          val trackingData = TrackingData(request, successForm.returnUrl)
          val returnUrl = ReturnUrl(successForm.returnUrl, request.headers.get("Referer"))
          identityService.registerThenSignIn(successForm, clientIp, trackingData).map {
            case Left(errors) =>
              redirectToRegisterPageWithErrors(errors, returnUrl, successForm.skipConfirmation, successForm.group)
            case Right(cookies) => {
              redirectRegisterSuccess(cookies, returnUrl, successForm.skipConfirmation, successForm.group)
            }
          }.recover {
            case NonFatal(ex) => {
              logger.warn(s"Unexpected error while registering: ${ex.getMessage}", ex)
              redirectToRegisterPageWithErrors(Seq(ServiceGatewayError(ex.getMessage)), returnUrl, successForm.skipConfirmation, successForm.group)
            }

          }
        }
      )
    }
  }

  private def redirectToRegisterPageWithErrors(errors: Seq[ServiceError], returnUrl: ReturnUrl, skipConfirmation: Option[Boolean], group: Option[String]) = {
    val idErrors = errors.map("register-" + _.id)
    SeeOther(routes.Application.register(idErrors, Some(returnUrl.encodedUrl), skipConfirmation, group).url)
  }

  private def redirectRegisterSuccess(cookies: Seq[PlayCookie], returnUrl: ReturnUrl, skipConfirmation: Option[Boolean], group: Option[String]) = {
    val groupCode = validateGroupCode(group)
    (groupCode, skipConfirmation.getOrElse(false)) match {
      case(Some(group), false) => {
        val skipConfirmationUrl = routes.Application.confirm(returnUrl.encodedUrl).url
        val skipConfirmationEncodedUrl = URLEncoder.encode(skipConfirmationUrl, "UTF-8")
        SeeOther(build3rdPartyUrl(group, skipConfirmationEncodedUrl, true)).withCookies(cookies: _*)
      }
      case(Some(group), true) => {
        SeeOther(build3rdPartyUrl(group, returnUrl.encodedUrl, true)).withCookies(cookies: _*)
      }
      case (None, false) => {
        SeeOther(routes.Application.confirm(returnUrl.encodedUrl).url).withCookies(cookies: _*)
      }
      case (None, true) => {
        SeeOther(returnUrl.url).withCookies(cookies: _*)
      }
    }
  }

  private def validateGroupCode(group: Option[String]): Option[String] = {
    group match {
      case Some("GRS") => Some("GRS")
      case Some("GTNF") => Some("GTNF")
      case _ => None
    }
  }

  private def build3rdPartyUrl(group: String, returnUrl: String, skipConfirmation: Boolean) = {
    val baseUrl = s"${config.identityProfileBase}/agree/$group"
    val fullUrl = s"$baseUrl?returnUrl=${returnUrl}&skipConfirmation=${skipConfirmation.toString}&skipThirdPartyLandingPage=true"
    fullUrl
  }
}
