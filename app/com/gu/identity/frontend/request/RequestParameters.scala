package com.gu.identity.frontend.request

import com.gu.identity.frontend.models.{ClientID, ReturnUrl}


object RequestParameters {

  sealed trait RequestParameter

  trait ReturnUrlRequestParameter extends RequestParameter {
    val returnUrl: ReturnUrl
  }

  trait SkipConfirmationRequestParameter extends RequestParameter {
    val skipConfirmation: Option[Boolean]
  }

  trait ClientIdRequestParameter extends RequestParameter {
    val clientId: Option[ClientID]
  }

  trait CSRFTokenRequestParameter extends RequestParameter {
    val csrfToken: String
  }

  trait SignInRequestParameters extends RequestParameter {
    val email: Option[String]
    val password: Option[String]
    val rememberMe: Boolean
  }

  type CoreSessionParameters =
    ReturnUrlRequestParameter with
    SkipConfirmationRequestParameter with
    ClientIdRequestParameter

  object CoreSessionParameters {
    def unapply(params: CoreSessionParameters): Option[(ReturnUrl, Option[Boolean], Option[ClientID])] =
      Some(params.returnUrl, params.skipConfirmation, params.clientId)
  }
}
