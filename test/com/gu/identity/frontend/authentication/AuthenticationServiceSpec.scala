package com.gu.identity.frontend.authentication

import com.gu.identity.cookie.{IdentityKeys, IdentityCookieDecoder}
import com.gu.identity.frontend.authentication.CookieName._
import com.gu.identity.frontend.configuration.Configuration
import org.scalatestplus.play.PlaySpec
import play.api.mvc.Cookie
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.Future

class AuthenticationServiceSpec extends PlaySpec {

  implicit def cookieNameToString(cookieName: Name): String = cookieName.toString

  val testKeys = new IdentityKeys(Configuration.testConfiguration.identityCookiePublicKey)

  val cookieDecoder = new IdentityCookieDecoder(testKeys)



  def requestWithCookies(cookies: Seq[Cookie]) = {
    FakeRequest().withCookies(cookies: _*)
  }

  def createCookie(value: String, name: String) = {
    Cookie(name = name, value = value)
  }

  "AuthenticatedUserFor" should {

    "decode valid cookie" in {
      val validRequestCookieData = "WyIxMDAwMDgxMSIsMTQ2Mjg5MjgyNDYxMV0.MCwCFG_PdoPk2PpSO5KoXbRLWJ0BvuqhAhRFIt1mlDcO2SN1Y6X7ktSs_oRJJw"
      val cookie = createCookie(validRequestCookieData, CookieName.SC_GU_U)
      val validCookieRequest = requestWithCookies(Seq(cookie))
      val response = AuthenticationService.authenticatedUserFor(validCookieRequest, cookieDecoder.getUserDataForScGuU)
      val expectedUser = AuthenticatedUser("10000811")

      response mustEqual Some(expectedUser)
    }

    "fail to decode invalid cookie" in {
      val invalidCookieRequest  = requestWithCookies(Seq(Cookie("abc", CookieName.SC_GU_U)))
      val response = AuthenticationService.authenticatedUserFor(invalidCookieRequest, cookieDecoder.getUserDataForScGuU)

      response mustEqual None
    }
  }

  "terminateSession" should {

    "return a response where all cookies have no value" in {
      val cookielessRequest = requestWithCookies(Seq.empty)
      val result = AuthenticationService.terminateSession(cookielessRequest, "www.theguardian.com", "www.theguardian.com")
      val resultCookies = cookies(Future.successful(result))
      resultCookies.get(CookieName.gu_user_features_expiry).value.value.isEmpty mustBe true
      resultCookies.get(CookieName.gu_paying_member).value.value.isEmpty mustBe true
      resultCookies.get(CookieName.GU_U).value.value.isEmpty mustBe true
      resultCookies.get(CookieName.SC_GU_U).value.value.isEmpty mustBe true
      resultCookies.get(CookieName.GU_ID_CSRF).value.value.isEmpty mustBe true
    }

    "return a response with a GU_TEST cookie" in {
      val cookielessRequest = requestWithCookies(Seq.empty)
      val testCookie = Seq(createCookie("test_value", "GU_TEST"))
      val result = AuthenticationService.terminateSession(cookielessRequest, "www.theguardian.com", "www.theguardian.com", testCookie)
      val resultCookies = cookies(Future.successful(result))
      resultCookies.get(CookieName.gu_user_features_expiry).value.value.isEmpty mustBe true
      resultCookies.get(CookieName.gu_paying_member).value.value.isEmpty mustBe true
      resultCookies.get(CookieName.GU_U).value.value.isEmpty mustBe true
      resultCookies.get(CookieName.SC_GU_U).value.value.isEmpty mustBe true
      resultCookies.get(CookieName.GU_ID_CSRF).value.value.isEmpty mustBe true
      resultCookies.get("GU_TEST").value.value mustEqual "test_value"
    }
  }
}
