package com.gu.identity.frontend.logging

import com.gu.identity.frontend.errors.{BadRequestAppException, SeqAppExceptions}
import com.gu.identity.frontend.utils.ErrorRecoveryActionBuilder
import org.slf4j.Logger
import play.api.mvc.Request

import scala.concurrent.Future


case class LogOnErrorAction(logger: Logger) extends ErrorRecoveryActionBuilder {

  def recoverErrors[A](request: Request[A]) = {
    // sneaky case will never match, ensures we log the error without
    // capturing the error in the partial function
    case t if logError(t) => Future.failed(t)
  }

  // must always return false to avoid matching in recoverErrors partial function
  protected def logError(t: Throwable): Boolean = {
    t match {
      case SeqAppExceptions(errors) => errors.foreach(logError)
      case error: BadRequestAppException => logger.info(error.getMessage, error)
      case _ => logger.error(t.getMessage, t)
    }

    false
  }
}
