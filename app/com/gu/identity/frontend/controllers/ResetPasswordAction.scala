package com.gu.identity.frontend.controllers

import com.gu.identity.frontend.csrf.{CSRFCheck, CSRFConfig}
import com.gu.identity.frontend.logging.Logging
import com.gu.identity.frontend.models.UrlBuilder
import com.gu.identity.frontend.services.{ServiceError, ServiceGatewayError, IdentityService}

import play.api.data.{Form}
import play.api.data.Forms._
import play.api.mvc.{RequestHeader, Controller}

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.util.control.NonFatal

case class ResetPasswordData(email: String)

case class ResetPasswordAction(identityService: IdentityService,
                                csrfConfig: CSRFConfig) extends Controller with Logging {
  val resetPasswordForm = Form(
    mapping(
      "email" -> email
    )(ResetPasswordData.apply)(ResetPasswordData.unapply)
  )

  def reset = CSRFCheck(csrfConfig, handleCSRFError).async { implicit request =>
    resetPasswordForm.bindFromRequest.fold(
      errorForm => {
        val errors = errorForm.errors.map(error => s"reset-password-error-${error.key}")
        Future.successful(NoCache(SeeOther(routes.Application.reset(errors).url)))
      },
      successForm => {
        identityService.sendResetPasswordEmail(successForm).map {
          case Left(errors) => {
            redirectToResetPageWithErrors(errors)
          }
          case Right(okResponse) => NoCache(SeeOther(routes.Application.reset().url))
        }.recover {
          case NonFatal(ex) => {
            logger.error(s"Unexpected error when trying to send reset password email: ${ex.getMessage}" )
            redirectToResetPageWithErrors(Seq(ServiceGatewayError("reset-error-gateway")))
          }
        }
      }
    )
  }

  private def redirectToResetPageWithErrors(errors: Seq[ServiceError]) = {
    val idErrors = errors.map {
      error => "error" -> error.message
    }
    NoCache(SeeOther(UrlBuilder(routes.Application.reset(), idErrors)))
  }

  private def handleCSRFError(request: RequestHeader, msg: String) = Future.successful {
    logger.error(s"CSRF error during Reset password: $msg")
    val errors = Seq("reset-error-csrf")

    SeeOther(routes.Application.signIn(errors).url)
  }
}
