package com.gu.identity.frontend.views.models

import org.scalatest.{FlatSpec, Matchers}

class UrlBuilder$Test extends FlatSpec with Matchers{

  it should "create url register plus return url param" in {
    UrlBuilder("register", Some(("redirectUrl", "www.test.com"))) should be ("register/?redirectUrl=www.test.com")
  }

  it should "create url register with no params" in {
    UrlBuilder("register", None) should be ("register")
  }
}
