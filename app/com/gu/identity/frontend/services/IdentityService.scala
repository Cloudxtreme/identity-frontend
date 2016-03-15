package com.gu.identity.frontend.services

import com.gu.identity.frontend.authentication.CookieService
import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.controllers.RegisterRequest
import com.gu.identity.frontend.controllers.ResetPasswordData
import com.gu.identity.frontend.models.{ClientIp, TrackingData}
import com.gu.identity.service.client.models.User
import com.gu.identity.service.client._
import com.gu.identity.service.client.request._
import org.joda.time.{DateTime, Seconds}
import play.api.mvc.{Cookie => PlayCookie}

import com.gu.identity.frontend.logging.Logging

import scala.concurrent.{ExecutionContext, Future}


/**
 * Adapter for the identity service client.
 */
trait IdentityService {
  def authenticate(email: Option[String], password: Option[String], rememberMe: Boolean, trackingData: TrackingData)(implicit ec: ExecutionContext): Future[Either[Seq[ServiceError], Seq[PlayCookie]]]
  def deauthenticate(cookie: PlayCookie, trackingData: TrackingData)(implicit ec: ExecutionContext): Future[Either[Seq[ServiceError], Seq[PlayCookie]]]
  def registerThenSignIn(request:RegisterRequest, clientIp: ClientIp, trackingData: TrackingData)(implicit ec: ExecutionContext): Future[Either[Seq[ServiceError], Seq[PlayCookie]]]
  def register(request: RegisterRequest, clientIp: ClientIp, trackingData: TrackingData)(implicit ec: ExecutionContext): Future[Either[Seq[ServiceError], RegisterResponseUser]]
  def sendResetPasswordEmail(data: ResetPasswordData, clientIp: ClientIp)(implicit ec: ExecutionContext): Future[Either[Seq[ServiceError], SendResetPasswordEmailResponse ]]
  def getUser(cookie: PlayCookie)(implicit ec: ExecutionContext): Future[Either[Seq[ServiceError], User]]
  def assignGroupCode(group: String, cookie: PlayCookie)(implicit ec: ExecutionContext): Future[Either[Seq[ServiceError], AssignGroupResponse]]
}


class IdentityServiceImpl(config: Configuration, adapter: IdentityServiceRequestHandler, client: IdentityClient) extends IdentityService with Logging {

  implicit val clientConfiguration = IdentityClientConfiguration(config.identityApiHost, config.identityApiKey, adapter)

  override def authenticate(email: Option[String], password: Option[String], rememberMe: Boolean, trackingData: TrackingData)(implicit ec: ExecutionContext) = {
    client.authenticateCookies(email, password, rememberMe, trackingData).map {
      case Left(errors) => Left {
        errors.map {
          case e: BadRequest => ServiceBadRequest(e.message, e.description)
          case e: GatewayError => ServiceGatewayError(e.message, e.description)
        }
      }
      case Right(cookies) => Right(CookieService.signInCookies(cookies, rememberMe)(config))
    }
  }

  override def deauthenticate(cookie: PlayCookie, trackingData: TrackingData)(implicit ec: ExecutionContext) = {
    val apiRequest = DeauthenticateApiRequest(cookie, trackingData)
    client.deauthenticate(apiRequest).map {
      case Left(errors) => Left {
        errors.map {
          case e: BadRequest => ServiceBadRequest(e.message, e.description)
          case e: GatewayError => ServiceGatewayError(e.message, e.description)
        }
      }
      case Right(cookies) => Right(CookieService.signOutCookies(cookies)(config))
    }
  }

  override def register(request: RegisterRequest, clientIp: ClientIp, trackingData: TrackingData)(implicit ec: ExecutionContext): Future[Either[Seq[ServiceError], RegisterResponseUser]] = {
    val apiRequest = RegisterApiRequest(request, clientIp, trackingData)
    client.register(apiRequest).map {
      case Left(errors) => Left {
        errors.map {
          case e: BadRequest => ServiceBadRequest(e.message, e.description)
          case e: GatewayError => ServiceGatewayError(e.message, e.description)
        }
      }
      case Right(user) => Right(user)
    }
  }

  override def registerThenSignIn(request: RegisterRequest,
                                  clientIp: ClientIp,
                                  trackingData: TrackingData
                                 )(implicit ec: ExecutionContext): Future[Either[Seq[ServiceError], Seq[PlayCookie]]] = {
    register(request, clientIp, trackingData).flatMap{
      case Left(errors) => Future.successful(Left(errors))
      case Right(user) => {
        authenticate(Some(request.email), Some(request.password), true, trackingData).map {
          case Left(signInErrors) => {
            logger.warn(s"User could not be logged in after registering: ${signInErrors}")
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
        errors.map {
          case e: BadRequest => ServiceBadRequest(e.message, e.description)
          case e: GatewayError => ServiceGatewayError(e.message, e.description)
          case _ => ServiceGatewayError("Unknown error")
        }
      }
      case Right(okResponse) => Right(okResponse)
    }
  }

  override def getUser(cookie: PlayCookie)(implicit ec: ExecutionContext): Future[Either[Seq[ServiceError], User]] = {
    val apiRequest = UserApiRequest(cookie)
    client.getUser(apiRequest).map {
      case Left(errors) => Left {
        errors.map {
          case e: BadRequest => ServiceBadRequest(e.message, e.description)
          case e: GatewayError => ServiceGatewayError(e.message, e.description)
        }
      }
      case Right(user) => Right(user)
    }
  }

  override def assignGroupCode(group: String, cookie: PlayCookie)(implicit ec: ExecutionContext): Future[Either[Seq[ServiceError], AssignGroupResponse]] = {
    val apiRequest = AssignGroupApiRequest(group, cookie)
    client.assignGroupCode(apiRequest).map {
      case Left(errors) => Left {
        errors.map {
          case e: BadRequest => ServiceBadRequest(e.message, e.description)
          case e: GatewayError => ServiceGatewayError(e.message, e.description)
        }
      }
      case Right(response) => Right(response)
    }
  }
}
