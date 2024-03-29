package com.gu.identity.service.client

import com.gu.identity.frontend.models.TrackingData
import com.gu.identity.service.client.request._

import scala.concurrent.{ExecutionContext, Future}

class IdentityClient extends Logging {

  def authenticateCookies(email: Option[String], password: Option[String], rememberMe: Boolean, trackingData: TrackingData)(implicit configuration: IdentityClientConfiguration, ec: ExecutionContext): Future[Either[IdentityClientErrors, Seq[IdentityCookie]]] =
    AuthenticateCookiesApiRequest(email, password, rememberMe, trackingData) match {
      case Right(request) => authenticateCookies(request)
      case Left(err) => Future.successful(Left(Seq(err)))
    }

  def authenticateCookies(request: AuthenticateCookiesApiRequest)(implicit configuration: IdentityClientConfiguration, ec: ExecutionContext): Future[Either[IdentityClientErrors, Seq[IdentityCookie]]] =
    configuration.requestHandler.handleRequest(request).map {
      case Left(error) => Left(error)
      case Right(AuthenticationCookiesResponse(cookies)) =>
        Right(cookies.values.map { c =>
          IdentityCookie(c.key, c.value, c.sessionCookie.getOrElse(false), cookies.expiresAt)
        })
      case Right(other) => Left(Seq(GatewayError("Unknown response")))
    }

  def register(request: RegisterApiRequest)(implicit configuration: IdentityClientConfiguration, ec: ExecutionContext): Future[Either[IdentityClientErrors, RegisterResponseUser]] = {
    configuration.requestHandler.handleRequest(request).map {
      case Left(error) => Left(error)
      case Right(RegisterResponse(user)) =>
        Right(user)
      case Right(other) => Left(Seq(GatewayError("Unknown response")))
    }
  }

  def sendResetPasswordEmail(request: SendResetPasswordEmailApiRequest)(implicit configuration: IdentityClientConfiguration, ec: ExecutionContext): Future[Either[IdentityClientErrors, SendResetPasswordEmailResponse ]] = {
    configuration.requestHandler.handleRequest(request).map {
      case Left(error) => {
        logger.error("Failed to send reset password email request")
        Left(error)
      }
      case Right(r: SendResetPasswordEmailResponse) => {
        logger.info("Successfully sent reset password email request")
        Right(r)
      }
      case Right(other) => Left(Seq(GatewayError("Unknown response")))
    }
  }

}
