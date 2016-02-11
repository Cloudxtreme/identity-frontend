package com.gu.identity.frontend

import com.gu.identity.frontend.errors.AppException

package object services {

  type ServiceException = AppException
  type ServiceExceptions = Seq[AppException]

//  type ServiceErrors = Seq[ServiceError]

//  sealed trait ServiceError extends NoStackTrace {
//    val id: String
//    val message: String
//    val description: Option[String]
//  }

//  case class ServiceGatewayError(message: String, description: Option[String] = None) extends ServiceError {
//    override val id: String = "error-gateway"
//  }
//  case class ServiceBadRequest(message: String, description: Option[String] = None) extends ServiceError {
//    override val id: String = "error-bad-request"
//  }

}
