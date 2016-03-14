package com.gu.identity.frontend.errors

import com.gu.identity.service.client.{ClientGatewayError, ClientBadRequestError, ClientInvalidCredentialsError, IdentityClientError}

/**
 * Errors which may occur during the Sign in process.
 */
sealed trait SignInAppException extends AppException

sealed trait SignInServiceAppException extends SignInAppException

object SignInServiceAppException {
  def apply(clientError: IdentityClientError): SignInServiceAppException =
    clientError match {
      case ClientInvalidCredentialsError => SignInInvalidCredentialsAppException
      case err: ClientBadRequestError => SignInServiceBadRequestException(clientError)
      case err: ClientGatewayError => SignInServiceGatewayAppException(clientError)
    }
}

case class SignInServiceGatewayAppException(
    clientError: IdentityClientError)
  extends ServiceGatewayAppException(clientError)
  with SignInServiceAppException {

  val id = SignInGatewayErrorID
}

case class SignInServiceBadRequestException(
    clientError: IdentityClientError)
  extends ServiceBadRequestAppException(clientError)
  with SignInServiceAppException {

  val id = SignInBadRequestErrorID
}

case object SignInInvalidCredentialsAppException
  extends ServiceBadRequestAppException(ClientInvalidCredentialsError)
  with SignInServiceAppException {

  val id = SignInInvalidCredentialsErrorID
}



case class SignInActionBadRequestAppException(message: String)
  extends AbstractAppException(message)
  with BadRequestAppException
  with SignInAppException {

  val id = SignInActionBadRequestErrorID
}
