package com.gu.identity.frontend.controllers

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.csrf.CSRFConfig
import com.gu.identity.frontend.errors.ResetPasswordServiceGatewayAppException
import com.gu.identity.frontend.models.ClientIp
import com.gu.identity.frontend.services.IdentityService
import com.gu.identity.service.client.{ClientGatewayError, SendResetPasswordEmailResponse}
import org.scalatest.mock.MockitoSugar
import org.mockito.Matchers.{eq => argEq, any => argAny}
import org.mockito.Mockito._
import org.scalatestplus.play.PlaySpec
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.{ExecutionContext, Future}

class ResetPasswordActionSpec extends PlaySpec with MockitoSugar {
  val fakeCsrfConfig = CSRFConfig.disabled

  trait WithControllerMockedDependencies {
    val mockIdentityService = mock[IdentityService]
    val config = Configuration.testConfiguration
    lazy val controller = new ResetPasswordAction(mockIdentityService, fakeCsrfConfig)
  }

  def fakeGatewayError(message: String = "Unexpected 500 error") =
    Seq(ResetPasswordServiceGatewayAppException(ClientGatewayError(message)))

  "POST /actions/reset" should {
    "redirect to the email validation sent confirmation page when email was properly sent" in new WithControllerMockedDependencies {
      val email = "example@gu.com"
      val fakeRequest = FakeRequest("POST", "/actions/reset")
        .withFormUrlEncodedBody("email" -> email)

      when(mockIdentityService.sendResetPasswordEmail(argAny[ResetPasswordData], argAny[ClientIp])(argAny[ExecutionContext]))
        .thenReturn {
          Future.successful{
            Right(SendResetPasswordEmailResponse())
          }
        }

      val result = call(controller.reset, fakeRequest)

      status(result) mustEqual SEE_OTHER
      redirectLocation(result) mustEqual Some("/reset/email-sent")
      header("Cache-Control", result) mustEqual Some("no-cache, private")
    }
  }

  "POST /actions/reset" should {
    "return an error if the email is empty" in new WithControllerMockedDependencies {
      val email = ""
      val fakeRequest = FakeRequest("POST", "/actions/reset")
        .withFormUrlEncodedBody("email" -> email)

      val result = call(controller.reset, fakeRequest)

      status(result) mustEqual SEE_OTHER
      redirectLocation(result) mustEqual Some("/reset?error=reset-password-error-email")
      header("Cache-Control", result) mustEqual Some("no-cache, private")
    }
  }

  "POST /actions/reset" should {
    "redirect to reset password form if an unexpected error happens when calling the service" in new WithControllerMockedDependencies {
      val email = "example@gu.com"
      val fakeRequest = FakeRequest("POST", "/actions/reset")
        .withFormUrlEncodedBody("email" -> email)


      when(mockIdentityService.sendResetPasswordEmail(argAny[ResetPasswordData], argAny[ClientIp])(argAny[ExecutionContext]))
        .thenReturn {
          Future.failed {
            new RuntimeException("Unexpected 500 error")
          }
        }

      val result = call(controller.reset, fakeRequest)

      status(result) mustEqual SEE_OTHER
      redirectLocation(result) mustEqual Some("/reset?error=error-unexpected")
      header("Cache-Control", result) mustEqual Some("no-cache, private")
    }
  }

  "POST /actions/reset" should {
    "return an error if the service failed to send the email" in new WithControllerMockedDependencies {
      val email = "example@gu.com"
      val fakeRequest = FakeRequest("POST", "/actions/reset")
        .withFormUrlEncodedBody("email" -> email)


      when(mockIdentityService.sendResetPasswordEmail(argAny[ResetPasswordData], argAny[ClientIp])(argAny[ExecutionContext]))
        .thenReturn {
          Future.successful {
            Left(fakeGatewayError())
          }
        }

      val result = call(controller.reset, fakeRequest)

      status(result) mustEqual SEE_OTHER
      redirectLocation(result) mustEqual Some("/reset?error=reset-password-error-gateway")
      header("Cache-Control", result) mustEqual Some("no-cache, private")
    }
  }
}
