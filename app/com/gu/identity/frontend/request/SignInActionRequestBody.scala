package com.gu.identity.frontend.request

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.errors.{SeqAppExceptions, AppException, SignInActionBadRequestAppException, ForgeryTokenAppException}
import com.gu.identity.frontend.models.{ClientID, ReturnUrl}
import com.gu.identity.frontend.request.RequestParameters._
import play.api.data.{Mapping, FormError, Form}
import play.api.mvc.{BodyParsers, BodyParser, Result}
import play.api.http.HeaderNames


case class SignInActionRequestBody(
    email: Option[String],
    password: Option[String],
    rememberMe: Boolean,
    returnUrl: ReturnUrl,
    skipConfirmation: Option[Boolean],
    clientId: Option[ClientID],
    csrfToken: String)
  extends SignInRequestParameters
  with ReturnUrlRequestParameter
  with SkipConfirmationRequestParameter
  with ClientIdRequestParameter
  with CSRFTokenRequestParameter


object SignInActionRequestBody {


  def bodyParser(configuration: Configuration) =
    BodyParser("SignInActionRequestBody") { requestHeader =>
      val refererHeader = requestHeader.headers.get(HeaderNames.REFERER)
      val form = Form {
        FormMapping.signInForm(configuration, refererHeader)
      }

      BodyParsers.parse.form(form, onErrors = onParserErrors).apply(requestHeader)
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


  object FormMapping {
    import play.api.data.Forms.{boolean, default, mapping, optional, text}
    import ClientID.FormMapping.clientId
    import ReturnUrl.FormMapping.returnUrl

    def signInForm(configuration: Configuration, refererHeader: Option[String]): Mapping[SignInActionRequestBody] =
      mapping(
        "email" -> optional(text),
        "password" -> optional(text),
        "rememberMe" -> default(boolean, false),
        "returnUrl" -> returnUrl(refererHeader, configuration),
        "skipConfirmation" -> optional(boolean),
        "clientId" -> optional(clientId),
        "csrfToken" -> text
      )(SignInActionRequestBody.apply)(SignInActionRequestBody.unapply)

  }
}
