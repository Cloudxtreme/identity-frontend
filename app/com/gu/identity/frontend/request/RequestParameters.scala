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

  trait GroupRequestParameter extends RequestParameter {
    val group: Option[String]
  }

  trait CSRFTokenRequestParameter extends RequestParameter {
    val csrfToken: String
  }

  trait SignInRequestParameters extends RequestParameter {
    val email: String
    val password: String
    val rememberMe: Boolean
  }

  type CoreSessionParameters =
    ReturnUrlRequestParameter with
    SkipConfirmationRequestParameter with
    ClientIdRequestParameter with
    GroupRequestParameter

  object CoreSessionParameters {
    def unapply(params: CoreSessionParameters): Option[(ReturnUrl, Option[Boolean], Option[ClientID], Option[String])] =
      Some(params.returnUrl, params.skipConfirmation, params.clientId, params.group)
  }
}
