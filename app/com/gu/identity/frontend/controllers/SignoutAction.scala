package com.gu.identity.frontend.controllers

import com.gu.identity.frontend.authentication.{AuthenticationService, CookieName}
import com.gu.identity.frontend.authentication.CookieName._
import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.logging.Logging
import com.gu.identity.frontend.models.{ReturnUrl, TrackingData}
import com.gu.identity.frontend.services.IdentityService
import com.gu.identity.frontend.utils.ExecutionContexts
import play.api.http.HttpErrorHandler
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._

import scala.concurrent.Future

class SignOutAction(identityService: IdentityService, val messagesApi: MessagesApi, config: Configuration) extends Controller with ExecutionContexts with Logging with I18nSupport {

  implicit def cookieNameToString(cookieName: Name): String = cookieName.toString

  def signOut(returnUrl: Option[String]) = Action.async { implicit request =>
    val validReturnUrl = ReturnUrl(returnUrl, config)
    val trackingData = TrackingData(request, None)
    request.cookies.get(CookieName.SC_GU_U).map { cookie =>
      identityService.deauthenticate(cookie, trackingData).map {
        case Left(errors) => {
          logger.info(s"Error returned from API signout: ${errors.map(_.description).mkString(", ")}")
          performSignout(request, validReturnUrl, Seq.empty)
        }
        case Right(signOutCookies) => performSignout(request, validReturnUrl, signOutCookies)
      }
    }.getOrElse {
      logger.info("User attempting signout without SC_GU_U cookie")
      Future.successful(performSignout(request, validReturnUrl, Seq.empty))
    }
  }

  def performSignout(request: RequestHeader, returnUrl: ReturnUrl, signoutCookies: Seq[Cookie]) = AuthenticationService.terminateSession(request, returnUrl.url, config.identityCookieDomain, signoutCookies)

}
