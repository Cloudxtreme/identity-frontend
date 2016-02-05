package com.gu.identity.frontend.errors

import play.api.http.Status._
import play.api.mvc.Results.Status


sealed trait ErrorResult {
  val error: Throwable // TODO use own exception which have `key`: String for redirects
}

class UnexpectedErrorResult(val error: Throwable) extends Status(INTERNAL_SERVER_ERROR) with ErrorResult
class BadGatewayErrorResult(val error: Throwable) extends Status(BAD_GATEWAY) with ErrorResult
class BadRequestErrorResult(val error: Throwable) extends Status(BAD_REQUEST) with ErrorResult
