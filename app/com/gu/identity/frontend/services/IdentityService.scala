package com.gu.identity.frontend.services

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.controllers.ResetPasswordData
import com.gu.identity.frontend.errors._
import com.gu.identity.frontend.models.{ClientIp, TrackingData}
import com.gu.identity.frontend.request.RegisterActionRequestBody
import com.gu.identity.frontend.request.RequestParameters.SignInRequestParameters
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

  def authenticate(signInRequest: SignInRequestParameters, trackingData: TrackingData)(implicit ec: ExecutionContext): Future[Either[ServiceExceptions, PlayCookies]]
  def registerThenSignIn(request:RegisterActionRequestBody, clientIp: ClientIp, trackingData: TrackingData)(implicit ec: ExecutionContext): Future[Either[ServiceExceptions, PlayCookies]]
  def register(request: RegisterActionRequestBody, clientIp: ClientIp, trackingData: TrackingData)(implicit ec: ExecutionContext): Future[Either[ServiceExceptions, RegisterResponseUser]]
  def sendResetPasswordEmail(data: ResetPasswordData, clientIp: ClientIp)(implicit ec: ExecutionContext): Future[Either[ServiceExceptions, SendResetPasswordEmailResponse ]]
}


class IdentityServiceImpl(config: Configuration, adapter: IdentityServiceRequestHandler, client: IdentityClient) extends IdentityService with Logging {

  implicit val clientConfiguration = IdentityClientConfiguration(config.identityApiHost, config.identityApiKey, adapter)

  override def authenticate(signInRequest: SignInRequestParameters, trackingData: TrackingData)(implicit ec: ExecutionContext) = {
    client.authenticateCookies(signInRequest.email, signInRequest.password, signInRequest.rememberMe, trackingData).map {
      case Left(errors) =>
        Left(errors.map(SignInServiceAppException.apply))

      case Right(cookies) => Right(cookies.map { c =>
        val maxAge = if (signInRequest.rememberMe) Some(Seconds.secondsBetween(DateTime.now, c.expires).getSeconds) else None
        val secureHttpOnly = c.key.startsWith("SC_")
        val cookieMaxAgeOpt = maxAge.filterNot(_ => c.isSession)

        PlayCookie(c.key, c.value, cookieMaxAgeOpt, "/", Some(config.identityCookieDomain), secure = secureHttpOnly, httpOnly = secureHttpOnly)
      })
    }
  }

  override def register(request: RegisterActionRequestBody, clientIp: ClientIp, trackingData: TrackingData)(implicit ec: ExecutionContext): Future[Either[ServiceExceptions, RegisterResponseUser]] = {
    val apiRequest = RegisterApiRequest(request, clientIp, trackingData)
    client.register(apiRequest).map {
      case Left(errors) =>
        Left(errors.map(RegisterServiceAppException.apply))

      case Right(user) => Right(user)
    }
  }

  override def registerThenSignIn(request: RegisterActionRequestBody,
                                  clientIp: ClientIp,
                                  trackingData: TrackingData
                                 )(implicit ec: ExecutionContext): Future[Either[ServiceExceptions, PlayCookies]] = {
    register(request, clientIp, trackingData).flatMap{
      case Left(errors) => Future.successful(Left(errors))
      case Right(user) => {
        // TODO use Request Parameter types for Register Request
        val authRequest = new SignInRequestParameters {
          val email: Option[String] = Some(request.email)
          val password: Option[String] = Some(request.password)
          val rememberMe: Boolean = true
        }

        authenticate(authRequest, trackingData).map {
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

  override def sendResetPasswordEmail(resetPasswordData: ResetPasswordData, clientIp: ClientIp)(implicit ec: ExecutionContext): Future[Either[ServiceExceptions, SendResetPasswordEmailResponse ]] = {
    val apiRequest = SendResetPasswordEmailApiRequest(resetPasswordData, clientIp)
    client.sendResetPasswordEmail(apiRequest).map {
      case Left(errors) => Left {
        // TODO explicit errors for sendResetPasswordEmail
        errors.map {
          case e: ClientBadRequestError => UnexpectedAppException(e.message)
          case e: ClientGatewayError => UnexpectedAppException(e.message)
          case _ => UnexpectedAppException("Unknown error")
        }
      }
      case Right(okResponse) => Right(okResponse)
    }
  }
}
