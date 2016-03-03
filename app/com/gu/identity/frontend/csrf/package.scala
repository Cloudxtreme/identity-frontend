package com.gu.identity.frontend

import play.api.mvc.{RequestHeader, Result}
import play.filters.csrf.CSRF

import scala.concurrent.Future


package object csrf {
  type CSRFErrorHandler = (RequestHeader, String) => Future[Result]

  lazy val defaultErrorHandler: CSRFErrorHandler = CSRF.DefaultErrorHandler.handle

}
