package com.gu.identity.frontend.controllers

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.csrf.CSRFConfig
import com.gu.identity.frontend.errors.{SignInServiceGatewayAppException, SignInServiceBadRequestException}
import com.gu.identity.frontend.models.TrackingData
import com.gu.identity.frontend.services._
import com.gu.identity.service.client.{GatewayError, BadRequest}
import org.mockito.Mockito._
import org.mockito.Matchers.{any => argAny, eq => argEq}
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.PlaySpec
import play.api.i18n.MessagesApi
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import org.scalatest.Matchers._

import scala.concurrent.{ExecutionContext, Future}

class SigninActionSpec extends PlaySpec with MockitoSugar {
  val fakeCsrfConfig = CSRFConfig.disabled
  val mockGoogleRecaptchaServiceHandler = mock[GoogleRecaptchaServiceHandler]

  trait WithControllerMockedDependencies {
    val mockIdentityService = mock[IdentityService]
    val messages = mock[MessagesApi]
    val mockGoogleRecaptchaCheck = new MockRecaptchaCheck()
    val config = Configuration.testConfiguration
    lazy val controller = new SigninAction(mockIdentityService, messages, fakeCsrfConfig, mockGoogleRecaptchaCheck, config)
  }

  trait WithFailedRecaptchaCheck extends WithControllerMockedDependencies{
    override val mockGoogleRecaptchaCheck = new MockRecaptchaCheck(false)
  }

  def fakeSigninRequest(
      email: Option[String],
      password: Option[String],
      rememberMe: Option[String],
      returnUrl: Option[String],
      googleRecaptchaResponse: Option[String] = None) = {
    val bodyParams = Seq("email" -> email, "password" -> password, "keepMeSignedIn" -> rememberMe, "returnUrl" -> returnUrl, "g-recaptcha-response" -> googleRecaptchaResponse)
      .filter(_._2.isDefined)
      .map(p => p._1 -> p._2.get)

    FakeRequest("POST", "/actions/signin")
      .withFormUrlEncodedBody(bodyParams: _*)
  }

  def fakeBadRequestError(message: String) =
    Seq(SignInServiceBadRequestException(BadRequest(message)))

  def fakeGatewayError(message: String = "Unexpected 500 error") =
    Seq(SignInServiceGatewayAppException(GatewayError(message)))


  class MockRecaptchaCheck(stubbedResult: Boolean = true) extends GoogleRecaptchaCheck(mockGoogleRecaptchaServiceHandler) {

    override def apply (
        googleRecaptchaResponse: Option[String],
        errorHandler: => Future[Result])
        (result: => Future[Result])
        (implicit ec: ExecutionContext): Future[Result] =
      if (stubbedResult) result else errorHandler
  }


  "POST /signin" should {

    "redirect to returnUrl when passed authentication" in new WithControllerMockedDependencies {
      val email = Some("me@me.com")
      val password = Some("password")
      val rememberMe = None
      val returnUrl = Some("http://www.theguardian.com/yeah")

      val testCookie = Cookie("SC_GU_U", "##hash##")

      when(mockIdentityService.authenticate(argEq(email), argEq(password), argEq(rememberMe.isDefined), argAny[TrackingData])(argAny[ExecutionContext]))
        .thenReturn {
          Future.successful {
            Right(Seq(testCookie))
          }
        }

      val result = call(controller.signIn, fakeSigninRequest(email, password, None, returnUrl))
      val resultCookies = cookies(result)

      status(result) mustEqual SEE_OTHER
      redirectLocation(result) mustEqual returnUrl

      resultCookies.size mustEqual 1
      resultCookies.head mustEqual testCookie
    }

    "redirect to sign in page when failed authentication" in new WithControllerMockedDependencies {
      val email = Some("me@me.com")
      val password = Some("password")
      val rememberMe = None
      val returnUrl = Some("http://www.theguardian.com/yeah")

      when(mockIdentityService.authenticate(argEq(email), argEq(password), argEq(rememberMe.isDefined), argAny[TrackingData])(argAny[ExecutionContext]))
        .thenReturn {
          Future.successful {
            Left(fakeBadRequestError("Invalid email or password"))
          }
        }

      val result = call(controller.signIn, fakeSigninRequest(email, password, None, returnUrl))

      status(result) mustEqual SEE_OTHER
      redirectLocation(result).get should startWith (routes.Application.signIn().url)

      // TODO check error parameters
    }

    "redirect to sign in page when service error" in new WithControllerMockedDependencies {
      val email = Some("me@me.com")
      val password = Some("password")
      val rememberMe = None
      val returnUrl = Some("http://www.theguardian.com/yeah")

      when(mockIdentityService.authenticate(argEq(email), argEq(password), argEq(rememberMe.isDefined), argAny[TrackingData])(argAny[ExecutionContext]))
        .thenReturn {
          Future.successful {
            Left(fakeGatewayError())
          }
        }

      val result = call(controller.signIn, fakeSigninRequest(email, password, None, returnUrl))

      status(result) mustEqual SEE_OTHER
      redirectLocation(result).get should startWith (routes.Application.signIn().url)

      // TODO check error parameters
    }


    "redirect to sign in page when error from future" in new WithControllerMockedDependencies {
      val email = Some("me@me.com")
      val password = Some("password")
      val rememberMe = None
      val returnUrl = Some("http://www.theguardian.com/yeah")

      when(mockIdentityService.authenticate(argEq(email), argEq(password), argEq(rememberMe.isDefined), argAny[TrackingData])(argAny[ExecutionContext]))
        .thenReturn {
          Future.failed {
            new RuntimeException("Unexpected 500 error")
          }
        }

      val result = call(controller.signIn, fakeSigninRequest(email, password, None, returnUrl))

      status(result) mustEqual SEE_OTHER
      redirectLocation(result).get should startWith (routes.Application.signIn().url)

      // TODO check error parameters
    }

    "redirect to return url when google captcha code is valid" in new WithControllerMockedDependencies {
      val email = Some("me@me.com")
      val password = Some("password")
      val rememberMe = None
      val returnUrl = Some("http://www.theguardian.com/yeah")
      val googleRecaptchaResponse = Some("12345")

      val testCookie = Cookie("SC_GU_U", "##hash##")

      when(mockIdentityService.authenticate(argEq(email), argEq(password), argEq(rememberMe.isDefined), argAny[TrackingData])(argAny[ExecutionContext]))
        .thenReturn {
          Future.successful {
            Right(Seq(testCookie))
          }
        }

      val result = call(controller.signIn, fakeSigninRequest(email, password, None, returnUrl, googleRecaptchaResponse))
      val resultCookies = cookies(result)

      status(result) mustEqual SEE_OTHER
      redirectLocation(result) mustEqual returnUrl

      resultCookies.size mustEqual 1
      resultCookies.head mustEqual testCookie
    }

    "redirect to sign in page when google captcha code is invalid" in new WithFailedRecaptchaCheck {
      val email = Some("me@me.com")
      val password = Some("password")
      val rememberMe = None
      val returnUrl = Some("http://www.theguardian.com/yeah")
      val googleRecaptchaResponse = Some("12345")

      val result = call(controller.signIn, fakeSigninRequest(email, password, None, returnUrl, googleRecaptchaResponse))
      status(result) mustEqual SEE_OTHER
      redirectLocation(result).get should startWith (routes.Application.signIn().url)
    }

  }

}
