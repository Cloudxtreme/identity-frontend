package com.gu.identity.frontend.services

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.controllers.RegisterRequest
import com.gu.identity.frontend.controllers.ResetPasswordData
import com.gu.identity.frontend.errors._
import com.gu.identity.frontend.models.{ClientIp, TrackingData}
import com.gu.identity.service.client._
import com.gu.identity.service.client.request.{SendResetPasswordEmailApiRequest, RegisterApiRequest}
import org.joda.time.{DateTime, Seconds}
import play.api.mvc.{Cookie => PlayCookie}

import com.gu.identity.frontend.logging.Logging

import scala.concurrent.{ExecutionContext, Future}


/**
 * Adapter for the identity service client.
 */
trait IdentityService {
  type PlayCookies = Seq[PlayCookie]

  def authenticate(email: Option[String], password: Option[String], rememberMe: Boolean, trackingData: TrackingData)(implicit ec: ExecutionContext): Future[Either[ServiceExceptions, PlayCookies]]
  def registerThenSignIn(request:RegisterRequest, clientIp: ClientIp, trackingData: TrackingData)(implicit ec: ExecutionContext): Future[Either[ServiceExceptions, PlayCookies]]
  def register(request: RegisterRequest, clientIp: ClientIp, trackingData: TrackingData)(implicit ec: ExecutionContext): Future[Either[ServiceExceptions, RegisterResponseUser]]
  def sendResetPasswordEmail(data: ResetPasswordData, clientIp: ClientIp)(implicit ec: ExecutionContext): Future[Either[ServiceExceptions, SendResetPasswordEmailResponse ]]
}


class IdentityServiceImpl(config: Configuration, adapter: IdentityServiceRequestHandler, client: IdentityClient) extends IdentityService with Logging {

  implicit val clientConfiguration = IdentityClientConfiguration(config.identityApiHost, config.identityApiKey, adapter)

  override def authenticate(email: Option[String], password: Option[String], rememberMe: Boolean, trackingData: TrackingData)(implicit ec: ExecutionContext) = {
    client.authenticateCookies(email, password, rememberMe, trackingData).map {
      case Left(errors) =>
        Left(errors.map(SignInServiceAppException.apply))

      case Right(cookies) => Right(cookies.map { c =>
        val maxAge = if (rememberMe) Some(Seconds.secondsBetween(DateTime.now, c.expires).getSeconds) else None
        val secureHttpOnly = c.key.startsWith("SC_")
        val cookieMaxAgeOpt = maxAge.filterNot(_ => c.isSession)

        PlayCookie(c.key, c.value, cookieMaxAgeOpt, "/", Some(config.identityCookieDomain), secure = secureHttpOnly, httpOnly = secureHttpOnly)
      })
    }
  }

  override def register(request: RegisterRequest, clientIp: ClientIp, trackingData: TrackingData)(implicit ec: ExecutionContext): Future[Either[ServiceExceptions, RegisterResponseUser]] = {
    val apiRequest = RegisterApiRequest(request, clientIp, trackingData)
    client.register(apiRequest).map {
      case Left(errors) =>
        Left(errors.map(RegisterServiceAppException.apply))

      case Right(user) => Right(user)
    }
  }

  override def registerThenSignIn(request: RegisterRequest,
                                  clientIp: ClientIp,
                                  trackingData: TrackingData
                                 )(implicit ec: ExecutionContext): Future[Either[ServiceExceptions, PlayCookies]] = {
    register(request, clientIp, trackingData).flatMap{
      case Left(errors) => Future.successful(Left(errors))
      case Right(user) => {
        authenticate(Some(request.email), Some(request.password), true, trackingData).map {
          case Left(signInErrors) => {
            logger.error(s"User could not be logged in after registering: $signInErrors")
            signInErrors.foreach { err =>
              logger.error(s"Sign in error after registering: ${err.getMessage}", err)
            }
            Right(Seq.empty)
          }
          case Right(cookies) => Right(cookies)
        }
      }
    }
  }

  override def sendResetPasswordEmail(resetPasswordData: ResetPasswordData, clientIp: ClientIp)(implicit ec: ExecutionContext): Future[Either[Seq[ServiceError], SendResetPasswordEmailResponse ]] = {
    val apiRequest = SendResetPasswordEmailApiRequest(resetPasswordData, clientIp)
    client.sendResetPasswordEmail(apiRequest).map {
      case Left(errors) => Left {
        // TODO explicit errors for sendResetPasswordEmail
        errors.map {
          case e: BadRequest => UnexpectedAppException(e.message)
          case e: GatewayError => UnexpectedAppException(e.message)
          case _ => UnexpectedAppException("Unknown error")
        }
      }
      case Right(okResponse) => Right(okResponse)
    }
  }
}
