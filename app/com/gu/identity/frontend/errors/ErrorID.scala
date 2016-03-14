package com.gu.identity.frontend.errors


sealed trait ErrorID {
  val key: String
}

case object SignInGatewayErrorID extends ErrorID { val key = "signin-error-gateway" }
case object SignInBadRequestErrorID extends ErrorID { val key = "signin-error-bad-request" }
case object SignInInvalidCredentialsErrorID extends ErrorID { val key = "signin-error-credentials" }

case object RegisterGatewayErrorID extends ErrorID { val key = "register-error-gateway" }
case object RegisterBadRequestErrorID extends ErrorID { val key = "register-error-bad-request" }

case object RegisterEmailConflictErrorID extends ErrorID { val key = "register-error-email-conflict" }
case object RegisterUsernameConflictErrorID extends ErrorID { val key = "register-error-username-conflict" }

case object ForgeryTokenErrorID extends ErrorID { val key = "error-forgery-token" }

case object SignInActionBadRequestErrorID extends ErrorID { val key = "signin-error-bad-request" }

case object UnexpectedErrorID extends ErrorID { val key = "error-unexpected" }
