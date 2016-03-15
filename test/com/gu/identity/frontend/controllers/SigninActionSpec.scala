package com.gu.identity.frontend.controllers

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.csrf.CSRFConfig
import com.gu.identity.frontend.errors.{SignInServiceGatewayAppException, SignInServiceBadRequestException}
import com.gu.identity.frontend.models.TrackingData
import com.gu.identity.frontend.request.RequestParameters.SignInRequestParameters
import com.gu.identity.frontend.services._
import com.gu.identity.service.client.{ClientGatewayError, ClientBadRequestError}
import org.mockito.ArgumentMatcher
import org.mockito.Mockito._
import org.mockito.Matchers.{any => argAny, argThat}
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

  val signInPageUrl = routes.Application.signIn().url


  trait WithControllerMockedDependencies {
    val mockIdentityService = mock[IdentityService]
    val messages = mock[MessagesApi]
    val config = Configuration.testConfiguration
    lazy val controller = new SigninAction(mockIdentityService, messages, fakeCsrfConfig, config)

    def mockAuthenticate(
        email: String,
        password: String,
        rememberMe: Boolean = false) = {

      val mockRequest = MockSignInRequest(email, password, rememberMe)

      mockIdentityService.authenticate(
        argThat(signInRequestParamsMatcher(mockRequest)),
        argAny[TrackingData]
      )(argAny[ExecutionContext])
    }
  }

  private case class MockSignInRequest(email: String, password: String, rememberMe: Boolean)
    extends SignInRequestParameters

  def signInRequestParamsMatcher(expect: SignInRequestParameters) =
    new ArgumentMatcher[SignInRequestParameters] {
      def matches(arg: scala.Any): Boolean = arg match {
        case r: SignInRequestParameters
          if r.email == expect.email && r.password == expect.password && r.rememberMe == expect.rememberMe => true
        case _ => false
      }
    }



  def fakeSigninRequest(
      email: Option[String] = None,
      password: Option[String] = None,
      rememberMe: Option[String] = None,
      returnUrl: Option[String] = None) = {

    val bodyParams = Seq(
      email.map("email" -> _),
      password.map("password" -> _),
      rememberMe.map("rememberMe" -> _),
      returnUrl.map("returnUrl" -> _),
      Some("csrfToken" -> "~~fake token~~")
    ).flatten

    FakeRequest("POST", "/actions/signin")
      .withFormUrlEncodedBody(bodyParams: _*)
  }

  def fakeBadRequestError(message: String) =
    Seq(SignInServiceBadRequestException(ClientBadRequestError(message)))

  def fakeGatewayError(message: String = "Unexpected 500 error") =
    Seq(SignInServiceGatewayAppException(ClientGatewayError(message)))


  "POST /signin" should {

    "redirect to returnUrl when passed authentication" in new WithControllerMockedDependencies {
      val email = "me@me.com"
      val password = "password"
      val returnUrl = Some("http://www.theguardian.com/yeah")

      val testCookie = Cookie("SC_GU_U", "##hash##")

      when(mockAuthenticate(email, password))
        .thenReturn {
          Future.successful {
            Right(Seq(testCookie))
          }
        }

      val result = call(controller.signIn, fakeSigninRequest(Some(email), Some(password), returnUrl = returnUrl))
      val resultCookies = cookies(result)

      status(result) mustEqual SEE_OTHER
      redirectLocation(result) mustEqual returnUrl

      resultCookies.size mustEqual 1
      resultCookies.head mustEqual testCookie
    }

    "redirect to sign in page when failed authentication" in new WithControllerMockedDependencies {
      val email = "me@me.com"
      val password = "password"
      val returnUrl = Some("http://www.theguardian.com/yeah")

      when(mockAuthenticate(email, password))
        .thenReturn {
          Future.successful {
            Left(fakeBadRequestError("Invalid email or password"))
          }
        }

      val result = call(controller.signIn, fakeSigninRequest(Some(email), Some(password), returnUrl = returnUrl))

      status(result) mustEqual SEE_OTHER
      redirectLocation(result).get should startWith (signInPageUrl)
      redirectLocation(result).get should include ("error=signin-error-bad-request")
    }

    "redirect to sign in page when service error" in new WithControllerMockedDependencies {
      val email = "me@me.com"
      val password = "password"
      val returnUrl = Some("http://www.theguardian.com/yeah")

      when(mockAuthenticate(email, password))
        .thenReturn {
          Future.successful {
            Left(fakeGatewayError())
          }
        }

      val result = call(controller.signIn, fakeSigninRequest(Some(email), Some(password), None, returnUrl))

      status(result) mustEqual SEE_OTHER
      redirectLocation(result).value should startWith (signInPageUrl)
      redirectLocation(result).value should include ("error=signin-error-gateway")
    }


    "redirect to sign in page when error from future" in new WithControllerMockedDependencies {
      val email = "me@me.com"
      val password = "password"
      val returnUrl = Some("http://www.theguardian.com/yeah")

      when(mockAuthenticate(email, password))
        .thenReturn {
          Future.failed {
            new RuntimeException("Unexpected 500 error")
          }
        }

      val result = call(controller.signIn, fakeSigninRequest(Some(email), Some(password), None, returnUrl))

      status(result) mustEqual SEE_OTHER
      redirectLocation(result).value should startWith (signInPageUrl)
      redirectLocation(result).value should include ("error=error-unexpected")

    }

  }

}
