package com.gu.identity.frontend.request

import com.gu.identity.frontend.csrf.RequestBodyWithCSRFToken
import com.gu.identity.frontend.models.ClientID
import play.api.data.Form
import play.api.data.Forms.{boolean, default, mapping, optional, text}
import play.api.mvc.BodyParsers.parse


case class SignInActionRequestBody(
    email: Option[String],
    password: Option[String],
    rememberMe: Boolean,
    returnUrl: Option[String],
    skipConfirmation: Option[Boolean],
    googleRecaptchaResponse: Option[String],
    clientID: Option[ClientID],
    csrfToken: String)
  extends RequestBodyWithCSRFToken

object SignInActionRequestBody {
  val signInForm = Form(
    mapping(
      "email" -> optional(text),
      "password" -> optional(text),
      "rememberMe" -> default(boolean, false),
      "returnUrl" -> optional(text),
      "skipConfirmation" -> optional(boolean),
      "g-recaptcha-response" -> optional(text),
      "clientId" -> optional(ClientID.FormMappings.clientId),
      "csrfToken" -> text
    )(SignInActionRequestBody.apply)(SignInActionRequestBody.unapply)
  )

  val parser = parse.form(signInForm)
}
