package com.gu.identity.frontend.views.models

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.models.text.ResetPasswordText
import play.api.i18n.Messages


case class ResetPasswordViewModel private(
    layout: LayoutViewModel,
    resetPasswordText: ResetPasswordText,
    userHelpEmailAddress: String = ResetPasswordViewModel.userHelpEmailAddress,
    resources: Seq[PageResource with Product],
    indirectResources: Seq[PageResource with Product]
  )
  extends ViewModel
  with ViewModelResources

object ResetPasswordViewModel {

  val userHelpEmailAddress = "userhelp@theguardian.com?subject=Account%20help"

  def apply(configuration: Configuration)(implicit messages: Messages): ResetPasswordViewModel = {
    val layout = LayoutViewModel(configuration)

    ResetPasswordViewModel(
      layout = layout,
      resetPasswordText = ResetPasswordText(),
      resources = layout.resources,
      indirectResources = layout.indirectResources
    )
  }
}
