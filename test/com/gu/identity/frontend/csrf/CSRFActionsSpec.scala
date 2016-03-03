package com.gu.identity.frontend.csrf

import com.gu.identity.frontend.request.RequestParameters.CSRFTokenRequestParameter
import com.gu.identity.frontend.test.SimpleAppPerSuite
import org.scalatestplus.play.PlaySpec
import play.api.mvc._
import play.api.test._
import play.api.test.Helpers._
import play.filters.csrf.{CSRFConfig => PlayCSRFConfig}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class CSRFActionsSpec extends PlaySpec with SimpleAppPerSuite {

  val csrfCookieName = "GU_TEST_CSRF"
  val underlyingCsrfConfig = PlayCSRFConfig(
    cookieName = Some(csrfCookieName)
  )
  val csrfConfig = CSRFConfig(underlyingCsrfConfig)

  def mockCSRFCheck(isValid: Boolean) = new CSRFCheck(csrfConfig) {
    override protected def playCSRFCheckAction[A](wrappedAction: Action[A]) = new Action[A] {
      def parser: BodyParser[A] = wrappedAction.parser

      def apply(request: Request[A]): Future[Result] =
        if (isValid) wrappedAction(request)
        else Future.successful(Results.Forbidden("Failed CSRF check"))
    }
  }


  case class MyCustomRequestBodyType(
      csrfToken: String,
      testString: String = "test from type")
    extends CSRFTokenRequestParameter

  val myCustomRequestBodyParser = BodyParsers.parse.urlFormEncoded.map { data =>
    MyCustomRequestBodyType("~~stubbed csrf token~~")
  }


  val controller = new Controller {
    def testCSRFAddToken = CSRFAddToken(csrfConfig) {
      Ok("test add token response")
    }

    def testCSRFCheck = mockCSRFCheck(isValid = true) {
      Ok("test check response")
    }

    def testFailedCSRFCheck = mockCSRFCheck(isValid = false) {
      Ok("should have failed")
    }

    def testCSRFCheckForCustomBodyParser = mockCSRFCheck(isValid = true)(myCustomRequestBodyParser) { request =>
      Ok(s"works for custom body parser: ${request.body.testString}")
    }
  }

  "Add token Action" must {

    "have non-cacheable response" in {
      val resp = controller.testCSRFAddToken(FakeRequest())

      val cacheHeader = header("Cache-Control", resp)

      cacheHeader.value must include("no-cache")
    }

    "have token on response" in {
      val resp = controller.testCSRFAddToken(FakeRequest())

      val respCookies = cookies(resp)
      val csrfCookie = respCookies.get(csrfCookieName)

      csrfCookie must not be empty
    }

  }


  "Check token Action" must {

    "have non-cacheable response when check valid" in {
      val resp = controller.testCSRFCheck(FakeRequest())

      val cacheHeader = header("Cache-Control", resp)

      cacheHeader.value must include("no-cache")
      contentAsString(resp) mustEqual "test check response"
    }


    "have failed response when check invalid" in {
      val resp = controller.testFailedCSRFCheck(FakeRequest())

      status(resp) mustEqual 403
      contentAsString(resp) mustEqual "Failed CSRF check"
    }

    "handle CSRF check properly when using custom body parser" in {
      val request = FakeRequest().withBody(MyCustomRequestBodyType("fake token"))
      val resp = controller.testCSRFCheckForCustomBodyParser(request)

      status(resp) mustEqual 200
      contentAsString(resp) mustEqual "works for custom body parser: test from type"
    }

    "handle failed CSRF check properly when using custom body parser" in {

    }

  }
}
