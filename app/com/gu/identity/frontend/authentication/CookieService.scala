package com.gu.identity.frontend.authentication

import com.gu.identity.frontend.configuration.Configuration
import org.joda.time.{DateTime, Seconds}
import play.api.mvc.{Cookie => PlayCookie}

sealed trait GuardianCookie {
  def name: CookieName.Name
  def secure: Boolean
}

final case class DotComCookie(name: CookieName.Name, secure: Boolean) extends GuardianCookie

final case class IdentityCookie(name: CookieName.Name, secure: Boolean) extends GuardianCookie

final case class IdentityApiCookie(name: String, value: String, isSession: Boolean, expires: DateTime)

object CookieService {

  def getMaxAge(expiration: DateTime, now: Option[DateTime] = None) = Seconds.secondsBetween(now.getOrElse(DateTime.now), expiration).getSeconds

  def signInCookies(cookies: Seq[IdentityApiCookie], rememberMe: Boolean, now: Option[DateTime] = None)(config: Configuration): Seq[PlayCookie] = {
    cookies.map { c =>
      val maxAge = if (rememberMe) Some(getMaxAge(c.expires, now)) else None
      val secureHttpOnly = CookieName.isSecureCookie(c.name)
      val cookieMaxAgeOpt = maxAge.filterNot(_ => c.isSession)

      PlayCookie(
        name = c.name,
        value = c.value,
        maxAge = cookieMaxAgeOpt,
        path = "/",
        domain = Some(config.identityCookieDomain),
        secure = secureHttpOnly,
        httpOnly = secureHttpOnly
      )
    }
  }

  def signOutCookies(cookies: Seq[IdentityApiCookie], now: Option[DateTime] = None)(config: Configuration): Seq[PlayCookie] = {
    cookies.map { c =>
      val maxAgeOpt = Some(getMaxAge(c.expires, now))

      PlayCookie(
        name = c.name,
        value = c.value,
        maxAge = maxAgeOpt,
        path = "/",
        domain = Some(config.identityCookieDomain),
        secure = false,
        httpOnly = false
      )

    }
  }

}
