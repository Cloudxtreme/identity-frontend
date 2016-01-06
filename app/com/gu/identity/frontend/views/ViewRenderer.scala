package com.gu.identity.frontend.views

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.models.ReturnUrl
import com.gu.identity.frontend.views.models.{ErrorViewModel, LayoutViewModel, SignInViewModel}
import jp.co.bizreach.play2handlebars.HBS
import play.api.i18n.Messages
import play.api.mvc.RequestHeader

/**
 * Adapter for Handlebars view renderer
 */
object ViewRenderer {
  def render(view: String, attributes: Map[String, Any] = Map.empty) =
    HBS(view, attributes)

  def renderSignIn(configuration: Configuration, errorIds: Seq[String], returnUrl: Option[String], skipConfirmation: Option[Boolean])(implicit request: RequestHeader, messages: Messages) = {
    val errors = errorIds.map(ErrorViewModel.apply)
    val attrs = LayoutViewModel(configuration).toMap ++
      SignInViewModel(
        errors = errors,
        returnUrl = ReturnUrl(returnUrl, request.headers.get("Referer")),
        skipConfirmation = skipConfirmation
      ).toMap
    render("signin-page", attrs)
  }
}
