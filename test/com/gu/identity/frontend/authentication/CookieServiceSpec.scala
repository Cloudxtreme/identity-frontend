package com.gu.identity.frontend.authentication

import com.gu.identity.frontend.configuration.Configuration
import org.joda.time.DateTime
import org.scalatestplus.play.PlaySpec

class CookieServiceSpec extends PlaySpec {

  val configuration = Configuration.testConfiguration

  val expires = new DateTime(2016, 1, 1, 14, 0)

  val signInCookiesFromApi = Seq(
    IdentityApiCookie(name = CookieName.GU_U.toString, value = "data for GU_U", isSession = false, expires = expires),
    IdentityApiCookie(name = CookieName.SC_GU_U.toString, value = "data for SC_GU_U", isSession = false, expires = expires),
    IdentityApiCookie(name = CookieName.SC_GU_LA.toString, value = "data for SC_GU_LA", isSession = true, expires = expires)
  )

  val signOutCookiesFromApi = Seq(
    IdentityApiCookie(name = CookieName.GU_SO.toString, value = "data for GU_SO", isSession = false, expires = expires)
  )

  "getMaxAge" should {

    "calculate expiry time in seconds between two datetimes" in {
      val expiration = new DateTime(2016, 1, 1, 14, 0)
      val now = new DateTime(2016, 1, 1, 13, 0)
      CookieService.getMaxAge(expiration, Some(now)) mustEqual 3600
    }

  }

  "signInCookies" should {

    "preserve cookie values" in {
      val cookieResult = CookieService.signInCookies(signInCookiesFromApi, rememberMe = false)(configuration)
      cookieResult.filter(c => c.name == CookieName.GU_U.toString).head.value mustEqual "data for GU_U"
      cookieResult.filter(c => c.name == CookieName.SC_GU_U.toString).head.value mustEqual "data for SC_GU_U"
      cookieResult.filter(c => c.name == CookieName.SC_GU_LA.toString).head.value mustEqual "data for SC_GU_LA"
    }

    "make all cookies session cookies if rememberMe is false" in {
      val cookieResult = CookieService.signInCookies(signInCookiesFromApi, rememberMe = false)(configuration)
      cookieResult.filter(c => c.name == CookieName.GU_U.toString).head.maxAge mustEqual None
      cookieResult.filter(c => c.name == CookieName.SC_GU_U.toString).head.maxAge mustEqual None
      cookieResult.filter(c => c.name == CookieName.SC_GU_LA.toString).head.maxAge mustEqual None
    }

    "set expiry for SC_GU_U and GU_U if rememberMe is true" in {
      val oneHourEarlier = new DateTime(2016, 1, 1, 13, 0)
      val cookieResult = CookieService.signInCookies(signInCookiesFromApi, rememberMe = true, now = Some(oneHourEarlier))(configuration)
      cookieResult.filter(c => c.name == CookieName.GU_U.toString).head.maxAge mustEqual Some(3600)
      cookieResult.filter(c => c.name == CookieName.SC_GU_U.toString).head.maxAge mustEqual Some(3600)
      cookieResult.filter(c => c.name == CookieName.SC_GU_LA.toString).head.maxAge mustEqual None
    }

    "set correct flags on secure cookies" in {
      val cookieResult = CookieService.signInCookies(signInCookiesFromApi, rememberMe = false)(configuration)
      cookieResult.filter(c => c.name == CookieName.SC_GU_U.toString).head.secure mustEqual true
      cookieResult.filter(c => c.name == CookieName.SC_GU_U.toString).head.httpOnly mustEqual true
      cookieResult.filter(c => c.name == CookieName.SC_GU_LA.toString).head.secure mustEqual true
      cookieResult.filter(c => c.name == CookieName.SC_GU_LA.toString).head.httpOnly mustEqual true
    }

    "set correct domain on cookies" in {
      val cookieResult = CookieService.signInCookies(signInCookiesFromApi, rememberMe = false)(configuration)
      cookieResult.foreach { cookie =>
        cookie.domain.get mustEqual "dev-theguardian.com"
      }
    }

  }

  "signOutCookies" should {

      "set expiry for GU_SO cookie" in {
        val oneHourEarlier = new DateTime(2016, 1, 1, 13, 0)
        val cookieResult = CookieService.signOutCookies(signOutCookiesFromApi, now = Some(oneHourEarlier))(configuration)
        cookieResult.filter(c => c.name == CookieName.GU_SO.toString).head.maxAge mustEqual Some(3600)
      }

      "set correct flag on GU_SO cookie" in {
        val cookieResult = CookieService.signOutCookies(signOutCookiesFromApi)(configuration)
        cookieResult.filter(c => c.name == CookieName.GU_SO.toString).head.secure mustEqual false
        cookieResult.filter(c => c.name == CookieName.GU_SO.toString).head.httpOnly mustEqual false
      }

  }

}
