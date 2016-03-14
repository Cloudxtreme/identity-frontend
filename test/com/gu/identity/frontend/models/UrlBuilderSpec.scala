package com.gu.identity.frontend.models

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.errors.{SeqAppExceptions, SignInActionBadRequestAppException, UnexpectedAppException}
import org.scalatest.{FlatSpec, Matchers}

class UrlBuilderSpec extends FlatSpec with Matchers{

  it should "create url register with return url" in {
    UrlBuilder("register", Seq(("returnUrl", "www.test.com"))) should be ("register?returnUrl=www.test.com")
  }

  it should "create url register with no params" in {
    UrlBuilder("register", Seq.empty) should be ("register")
  }

  it should "create a register url with 'returnUrl=www.test.com' and ignore any None objects" in {
    UrlBuilder("register", Seq(("returnUrl", "www.test.com"))) should be ("register?returnUrl=www.test.com")
  }

  it should "create a url with return url params encoded" in {
    val params = Seq(
      "returnUrl" -> "https://subscribe.theguardian.com/?INTCMP=NGW_HEADER_UK_GU_SUBSCRIBE&yes=no",
      "another" -> "hello"
    )

    UrlBuilder("/register", params) should be (
      "/register?returnUrl=https%3A%2F%2Fsubscribe.theguardian.com%2F%3FINTCMP%3DNGW_HEADER_UK_GU_SUBSCRIBE%26yes%3Dno&another=hello"
    )
  }

  it should "create valid url when provided a valid returnUrl object" in {
    val returnUrl = ReturnUrl(Some("http://www.theguardian.com/uk"), Configuration.testConfiguration)

    UrlBuilder("/register/confirm", returnUrl) should be("/register/confirm?returnUrl=http%3A%2F%2Fwww.theguardian.com%2Fuk")
  }

  it should "create valid url without any returnUrl when provided the default returnUrl" in {
    val returnUrl = ReturnUrl(None, Configuration.testConfiguration)

    UrlBuilder("/register/confirm", returnUrl) should be("/register/confirm")
  }

  it should "create a valid url when given a single error" in {
    val error = UnexpectedAppException("Oh no!")

    UrlBuilder("/signin", error) should be(s"/signin?error=${error.id.key}")
  }

  it should "create a valid url when given a multiple errors" in {
    val error1 = UnexpectedAppException("Oh no!")
    val error2 = SignInActionBadRequestAppException("Failed request")

    val error = SeqAppExceptions(Seq(error1, error2))

    UrlBuilder("/signin", error) should be(s"/signin?error=${error1.id.key}&error=${error2.id.key}")
  }

}
