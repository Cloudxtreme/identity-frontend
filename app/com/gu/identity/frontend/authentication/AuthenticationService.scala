package com.gu.identity.frontend.authentication

import com.gu.identity.frontend.authentication.CookieName.Name
import com.gu.identity.model.User
import play.api.mvc.{Result, Cookie, DiscardingCookie, RequestHeader}
import play.api.mvc.Results._

case class AuthenticatedUser(userId: String)

object AuthenticationService {

  val knownCookies: Seq[GuardianCookie] = Seq(
    DotComCookie(CookieName.gu_user_features_expiry, secure = false),
    DotComCookie(CookieName.gu_paying_member, secure = false),
    IdentityCookie(CookieName.GU_U, secure = false),
    IdentityCookie(CookieName.GU_ID_CSRF, secure = true),
    IdentityCookie(CookieName.SC_GU_U, secure = true)
  )

  implicit def cookieNameToString(cookieName: Name): String = cookieName.toString

  def authenticatedUserFor[A](request: RequestHeader, cookieDecoder: String => Option[User]): Option[AuthenticatedUser] = for {
    scGuU <- request.cookies.get(CookieName.SC_GU_U)
    minimalSecureUser <- cookieDecoder(scGuU.value)
    userId <- Option(minimalSecureUser.getId)
  } yield AuthenticatedUser(userId)

  def terminateSession(
      request: RequestHeader,
      verifiedReturnUrl: String,
      cookieDomain: String,
      newCookies: Seq[Cookie] = Seq.empty): Result = {

    val cookiesToDiscard: Seq[DiscardingCookie] = knownCookies.map { cookie =>
      DiscardingCookie(name = cookie.name, path = "/", domain = Some(cookieDomain), secure = cookie.secure)
    }

    SeeOther(verifiedReturnUrl)
      .withHeaders("Cache-Control" -> "no-cache", "Pragma" -> "no-cache")
      .discardingCookies(cookiesToDiscard:_*)
      .withCookies(newCookies: _*)
  }
}


