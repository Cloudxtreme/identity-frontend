package com.gu.identity.service.client.request

import com.gu.identity.frontend.models.TrackingData
import com.gu.identity.service.client.{ClientBadRequestError, IdentityClientConfiguration, IdentityClientRequestHandler}
import org.scalatest.mock.MockitoSugar
import org.scalatest.{Matchers, WordSpec}


class AuthenticateCookiesApiRequestSpec extends WordSpec with Matchers with MockitoSugar {

  val handler = mock[IdentityClientRequestHandler]

  implicit val testConfig = IdentityClientConfiguration("test.gu.com", "##key##", handler)

  val trackingData = TrackingData(
    returnUrl = Some("https://profile.theguardian.com"),
    registrationType = Some("facebook"),
    omnitureSVi = Some("omnitureCode"),
    ipAddress = Some("127.0.0.1"),
    referrer = Some("http://www.theguardian.com"),
    userAgent = Some("chrome")
  )

  "Authenticate Cookies API Request" should {
    "Parse correctly with a valid email address, password" in {
      val email = Some("test@guardian.co.uk")
      val password = Some("god")

      val result = AuthenticateCookiesApiRequest(email, password, rememberMe = false, trackingData)

      result.isRight shouldBe true
      result.right.get.body.get.email should equal (email.get)
      result.right.get.body.get.password should equal(password.get)
      result.right.get.parameters should contain("persistent" -> "false")
    }

    "Set persistent parameter on request" in {
      val email = Some("test@guardian.co.uk")
      val password = Some("god")

      val result = AuthenticateCookiesApiRequest(email, password, rememberMe = true, trackingData)

      result.isRight shouldBe true
      result.right.get.parameters should contain("persistent" -> "true")
    }

    "Set tracking data parameters on request" in {
      val email = Some("test@guardian.co.uk")
      val password = Some("god")

      val result = AuthenticateCookiesApiRequest(email, password, rememberMe = true, trackingData)

      result.isRight shouldBe true
      val params = result.right.get.parameters
      trackingData.parameters.foreach( td => params should contain(td))
    }

    "Fail when given an invalid email domain input" in {
      val email = Some("me@")
      val password = Some("god")

      val result = AuthenticateCookiesApiRequest(email, password, rememberMe = false, trackingData)

      result.isLeft shouldBe true
      result.left.get shouldBe a[ClientBadRequestError]
    }


    "Fail when given an invalid start of email input" in {
      val email = Some("@blah.com")
      val password = Some("god")

      val result = AuthenticateCookiesApiRequest(email, password, rememberMe = false, trackingData)

      result.isLeft shouldBe true
      result.left.get shouldBe a[ClientBadRequestError]
    }

    "Fail when given an empty email input" in {
      val email = Some("")
      val password = Some("god")

      val result = AuthenticateCookiesApiRequest(email, password, rememberMe = false, trackingData)

      result.isLeft shouldBe true
      result.left.get shouldBe a[ClientBadRequestError]
    }

    "Fail when given an invalid password input" in {
      val email = Some("me@guardian.co.uk")
      val password = Some("")

      val result = AuthenticateCookiesApiRequest(email, password, rememberMe = false, trackingData)

      result.isLeft shouldBe true
      result.left.get shouldBe a[ClientBadRequestError]
    }

    "include X-Forwarded-For header on requests" in {
      val email = Some("test@guardian.co.uk")
      val password = Some("god")

      val result = AuthenticateCookiesApiRequest(email, password, rememberMe = false, trackingData)

      result shouldBe 'right
      result.right.get.headers should contain("X-Forwarded-For" -> "127.0.0.1")
    }
  }

}
