package com.gu.identity.frontend.controllers

import com.gu.identity.frontend.csrf.{CSRFCheck, CSRFConfig}
import com.gu.identity.frontend.errors.RedirectOnError
import com.gu.identity.frontend.logging.{LogOnErrorAction, Logging}
import com.gu.identity.frontend.models.{ClientIp, UrlBuilder}
import com.gu.identity.frontend.services.{ServiceAction, IdentityService}

import play.api.data.Form
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

  val redirectRoute: String = routes.Application.reset().url

  val ResetPasswordServiceAction =
    ServiceAction andThen
      RedirectOnError(redirectRoute) andThen
      LogOnErrorAction(logger) andThen
      CSRFCheck(csrfConfig)


  def reset = ResetPasswordServiceAction { implicit request =>
    resetPasswordForm.bindFromRequest.fold(
      errorForm => {
        // TODO replace with ResetPasswordInvalidEmailAppException
        val errors = errorForm.errors.map(error => s"reset-password-error-${error.key}")
        Future.successful(Right(NoCache(SeeOther(routes.Application.reset(errors).url))))
      },
      successForm => {
        val ip = ClientIp(request)
        identityService.sendResetPasswordEmail(successForm, ip).map {
          case Left(errors) =>
            Left(errors)

          case Right(okResponse) => Right {
            NoCache(SeeOther(routes.Application.resetPasswordEmailSent().url))
          }
        }
      }
    )
  }
}
