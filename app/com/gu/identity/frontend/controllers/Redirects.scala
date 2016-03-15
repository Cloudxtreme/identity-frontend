package com.gu.identity.frontend.controllers

import com.gu.identity.frontend.logging.Logging
import play.api.mvc.{Call, Action, Controller}


final class Redirects extends Controller with Logging {

  lazy val signInRoute: Call = routes.Application.signIn()
  lazy val signOutRoute: Call = routes.SignOutAction.signOut()
  lazy val registerRoute: Call = routes.Application.register()


  def indexRedirect =
    redirectToRoute(signInRoute)


  def signInPageTrailingSlash =
    redirectToRoute(signInRoute, isLegacy = true)

  def signOutPageTrailingSlash =
    redirectToRoute(signOutRoute, isLegacy = true)

  def registerPageTrailingSlash =
    redirectToRoute(registerRoute, isLegacy = true)


  private def redirectToRoute(call: Call, isLegacy: Boolean = false) = Action { request =>
    if (isLegacy) {
      val referrer = request.headers.get(REFERER).getOrElse("_unknown_")

      logger.info(s"Legacy route called: ${request.uri} from $referrer")

      MovedPermanently(call.url)

    } else {
      SeeOther(call.url)
    }
  }

}
