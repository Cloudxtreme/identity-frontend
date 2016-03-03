package com.gu.identity.frontend.request

import com.gu.identity.frontend.csrf.RequestBodyWithCSRFToken
import com.gu.identity.frontend.errors.{SeqAppExceptions, AppException, SignInActionBadRequestAppException, ForgeryTokenAppException}
import com.gu.identity.frontend.models.ClientID
import play.api.data.{FormError, Form}
import play.api.data.Forms.{boolean, default, mapping, optional, text}
import play.api.mvc.BodyParsers.parse
import play.api.mvc.{BodyParser, Results, Result}


case class SignInActionRequestBody(
    email: Option[String],
    password: Option[String],
    rememberMe: Boolean,
    returnUrl: Option[String],
    skipConfirmation: Option[Boolean],
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
      "clientId" -> optional(ClientID.FormMappings.clientId),
      "csrfToken" -> text
    )(SignInActionRequestBody.apply)(SignInActionRequestBody.unapply)
  )

  val parser = BodyParser("SignInActionRequestBody") {
    parse.form(signInForm, onErrors = onParserErrors)
  }

  private def onParserErrors(form: Form[SignInActionRequestBody]): Result = throw {
    if (form.errors.size == 1) formErrorToAppException(form.errors.head)
    else SeqAppExceptions {
      form.errors.map(formErrorToAppException)
    }
  }

  private def formErrorToAppException(formError: FormError): AppException = formError match {
    case FormError("csrfToken", _, _) => ForgeryTokenAppException("Missing csrfToken on request")
    case e => SignInActionBadRequestAppException(s"Unexpected error: ${e.message}")
  }
}
