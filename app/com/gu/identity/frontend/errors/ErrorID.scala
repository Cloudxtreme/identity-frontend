package com.gu.identity.frontend.errors


sealed trait ErrorID {
  val key: String
}

object ErrorIDs {

  case object SignInGatewayErrorID extends ErrorID {
    val key = "signin-error-gateway"
  }

  case object SignInBadRequestErrorID extends ErrorID {
    val key = "signin-error-bad-request"
  }

  case object SignInInvalidCredentialsErrorID extends ErrorID {
    val key = "signin-error-credentials"
  }

  case object SignInInvalidCaptchaErrorID extends ErrorID {
    val key = "signin-error-captcha"
  }

  case object SignInActionBadRequestErrorID extends ErrorID {
    val key = "signin-error-action-bad-request"
  }


  case object RegisterGatewayErrorID extends ErrorID {
    val key = "register-error-gateway"
  }

  case object RegisterBadRequestErrorID extends ErrorID {
    val key = "register-error-bad-request"
  }

  case object RegisterEmailConflictErrorID extends ErrorID {
    val key = "register-error-email-conflict"
  }

  case object RegisterUsernameConflictErrorID extends ErrorID {
    val key = "register-error-username-conflict"
  }

  case object RegisterActionInvalidFirstNameErrorID extends ErrorID {
    val key = "register-error-firstname"
  }

  case object RegisterActionInvalidLastNameErrorID extends ErrorID {
    val key = "register-error-lastname"
  }

  case object RegisterActionInvalidEmailErrorID extends ErrorID {
    val key = "register-error-email"
  }

  case object RegisterActionInvalidUsernameErrorID extends ErrorID {
    val key = "register-error-username"
  }

  case object RegisterActionInvalidPasswordErrorID extends ErrorID {
    val key = "register-error-password"
  }

  case object RegisterActionInvalidGroupErrorID extends ErrorID {
    val key = "register-error-group"
  }

  case object RegisterActionBadRequestErrorID extends ErrorID {
    val key = "register-error-bad-request"
  }


  case object ResetPasswordGatewayErrorID extends ErrorID {
    val key = "reset-password-error-gateway"
  }

  case object ResetPasswordBadRequestErrorID extends ErrorID {
    val key = "reset-password-error-bad-request"
  }

  case object ResetPasswordInvalidEmailErrorID extends ErrorID {
    val key = "reset-password-error-email"
  }


  case object ForgeryTokenErrorID extends ErrorID {
    val key = "error-forgery-token"
  }

  case object UnexpectedErrorID extends ErrorID {
    val key = "error-unexpected"
  }

}
