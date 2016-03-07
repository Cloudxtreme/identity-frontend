package com.gu.identity.frontend.models

import java.net.{URLEncoder, URI}

import com.gu.identity.frontend.configuration.Configuration
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

  it should "Include group code in return url when one is provided" in {
    val baseUrl = "/register/confirm"
    val returnUrl = ReturnUrl(Some("http://www.theguardian.com/uk"), Configuration.testConfiguration)
    val group = Some("ABC")

    val result = UrlBuilder(baseUrl, returnUrl, skipConfirmation = None, clientId = None, group = group)
    result should be("/register/confirm?returnUrl=http%3A%2F%2Fwww.theguardian.com%2Fuk&group=ABC")
  }

  it should "return a url including a reference to the third party additional ts and cs page" in {
    val baseDomain = "oauth.thegulocal.com"
    val baseUrl = s"http://$baseDomain"
    val url = "http://www.jobs.theguardian.com"
    val encodedUrl = URLEncoder.encode(url, "UTF-8")
    val returnUrl = ReturnUrl(Some(url), Configuration.testConfiguration)
    val groupCode = GroupCode("GRS").get

    val result = UrlBuilder.buildThirdPartyReturnUrl(baseUrl, returnUrl, Some(true), None, groupCode, Configuration.testConfiguration)
    val validUri = new URI(result)
    validUri.getHost shouldBe baseDomain
    validUri.getQuery.contains(encodedUrl) shouldBe true

  }
}
