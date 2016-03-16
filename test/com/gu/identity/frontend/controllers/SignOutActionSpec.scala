package com.gu.identity.frontend.controllers

import com.gu.identity.frontend.authentication.{IdentityApiCookie, CookieName}
import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.csrf.CSRFConfig
import com.gu.identity.frontend.models.TrackingData
import com.gu.identity.frontend.services._
import org.mockito.{ArgumentCaptor, ArgumentMatcher}
import org.mockito.Matchers.{any => argAny, eq => argEq}
import org.mockito.Mockito._
import org.scalatest.Matchers._
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.PlaySpec
import play.api.i18n.MessagesApi
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.{ExecutionContext, Future}

class SignOutActionSpec extends PlaySpec with MockitoSugar {

  trait WithControllerMockedDependencies {
    val mockIdentityService = mock[IdentityService]
    val messages = mock[MessagesApi]
    val config = Configuration.testConfiguration
    lazy val controller = new SignOutAction(mockIdentityService, messages, config)
  }

  val secureCookie = Cookie(name = CookieName.SC_GU_U.toString, value = "SC_GU_U_data", maxAge = None, path = "/", domain = Some("dev-theguardian.com"), secure = true, httpOnly = true)

  val signedInCookies = Seq(
    secureCookie,
    Cookie(name = CookieName.SC_GU_LA.toString, value = "SC_GU_LA", maxAge = None, path = "/", domain = Some("dev-theguardian.com"), secure = true, httpOnly = true),
    Cookie(name = CookieName.GU_U.toString, value = "GU_U", maxAge = None, path = "/", domain = Some("dev-theguardian.com"), secure = false, httpOnly = false)
  )

  def fakeSignOutRequest(cookies: Seq[Cookie] = Seq.empty) = FakeRequest("GET", "/signout").withCookies(cookies: _*)

  "GET /signout" should {

    "redirect to returnUrl when user has SC_GU_U cookie" in new WithControllerMockedDependencies {
      val returnUrl = Some("http://www.theguardian.com/yeah")
      val signOutCookie = Cookie(name = CookieName.GU_SO.toString, value = "data_for_GU_SO")

      val captor = ArgumentCaptor.forClass(classOf[Cookie])

      when(mockIdentityService.deauthenticate(captor.capture(), argAny[TrackingData])(argAny[ExecutionContext]))
        .thenReturn {
          Future.successful {
            Right(Seq(signOutCookie))
          }
        }

      val result = call(controller.signOut(returnUrl), fakeSignOutRequest(signedInCookies))
      val resultCookies = cookies(result)

      captor.getValue.name mustEqual secureCookie.name
      captor.getValue.value mustEqual secureCookie.value

      status(result) mustEqual SEE_OTHER
      redirectLocation(result) mustEqual returnUrl

      resultCookies.size mustEqual 6
    }

    "redirect to returnUrl when Identity API call fails" in new WithControllerMockedDependencies {
      val returnUrl = Some("http://www.theguardian.com/yeah")

      val signOutCookie = Cookie(name = CookieName.GU_SO.toString, value = "data_for_GU_SO")

      when(mockIdentityService.deauthenticate(argAny[Cookie], argAny[TrackingData])(argAny[ExecutionContext]))
        .thenReturn {
          Future.successful {
            Left(Seq(ServiceBadRequest("Something went wrong")))
          }
        }

      val result = call(controller.signOut(returnUrl), fakeSignOutRequest(signedInCookies))
      val resultCookies = cookies(result)

      status(result) mustEqual SEE_OTHER
      redirectLocation(result) mustEqual returnUrl

      resultCookies.size mustEqual 5
    }

    "redirect to returnUrl despite lack of SC_GU_U cookie" in new WithControllerMockedDependencies {
      val returnUrl = Some("http://www.theguardian.com/yeah")

      val result = call(controller.signOut(returnUrl), fakeSignOutRequest(Seq.empty))
      val resultCookies = cookies(result)

      status(result) mustEqual SEE_OTHER
      redirectLocation(result) mustEqual returnUrl

      resultCookies.size mustEqual 5
    }

    "set GU_SO cookie when user has SC_GU_U cookie" in new WithControllerMockedDependencies {
      val returnUrl = Some("http://www.theguardian.com/yeah")
      val signOutCookie = Cookie(name = CookieName.GU_SO.toString, value = "data_for_GU_SO")

      val captor = ArgumentCaptor.forClass(classOf[Cookie])

      when(mockIdentityService.deauthenticate(captor.capture(), argAny[TrackingData])(argAny[ExecutionContext]))
        .thenReturn {
          Future.successful {
            Right(Seq(signOutCookie))
          }
        }

      val result = call(controller.signOut(returnUrl), fakeSignOutRequest(signedInCookies))
      val resultCookies = cookies(result)

      captor.getValue.name mustEqual secureCookie.name
      captor.getValue.value mustEqual secureCookie.value

      resultCookies.size mustEqual 6
      resultCookies.get("GU_SO").get.name == "GU_SO"
      resultCookies.get("GU_SO").get.value == "data_for_GU_SO"
    }

  }

}
