package com.gu.identity.frontend.errors

import com.gu.identity.frontend.errors.ErrorIDs.{ResetPasswordInvalidEmailErrorID, ResetPasswordBadRequestErrorID, ResetPasswordGatewayErrorID}
import com.gu.identity.service.client.{ClientGatewayError, ClientBadRequestError, IdentityClientError}

sealed trait ResetPasswordAppException extends AppException

object ResetPasswordAppException {
  def apply(clientError: IdentityClientError): ResetPasswordAppException =
    clientError match {
      case err: ClientBadRequestError => ResetPasswordServiceBadRequestAppException(clientError)
      case err: ClientGatewayError => ResetPasswordServiceGatewayAppException(clientError)
    }
}

case class ResetPasswordServiceGatewayAppException(
    clientError: IdentityClientError)
  extends ServiceGatewayAppException(clientError)
  with ResetPasswordAppException {

  val id = ResetPasswordGatewayErrorID
}

case class ResetPasswordServiceBadRequestAppException(
    clientError: IdentityClientError)
  extends ServiceBadRequestAppException(clientError)
  with ResetPasswordAppException {

  val id = ResetPasswordBadRequestErrorID
}

case class ResetPasswordInvalidEmailAppException(message: String)
  extends AbstractAppException(message)
  with BadRequestAppException
  with ResetPasswordAppException {

  val id = ResetPasswordInvalidEmailErrorID
}
