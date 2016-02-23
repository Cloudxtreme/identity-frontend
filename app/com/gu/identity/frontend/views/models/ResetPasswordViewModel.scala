package com.gu.identity.frontend.views.models

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.models.text.ResetPasswordText
import play.api.i18n.Messages


case class ResetPasswordViewModel private(
    pageTitle: String,
    titleText: String,
    subtitleText: String,
    emailAddressFieldText: String,
    buttonText: String,
    socialNetworksTitleText: String,
    socialNetworksContentText: String,
    userHelpTitleText: String,
    userHelpEmailText: String,
    userHelpContentText: String,
    userHelpEmailAddress: String = ResetPasswordViewModel.userHelpEmailAddress,
    resources: Seq[PageResource with Product],
    indirectResources: Seq[PageResource with Product]
  )
  extends ViewModel
  with ViewModelResources

object ResetPasswordViewModel {

  val userHelpEmailAddress = "userhelp@theguardian.com?subject=Account%20help"

  def apply(configuration: Configuration)(implicit messages: Messages): ResetPasswordViewModel = {
    val text = ResetPasswordText()
    val layout = LayoutViewModel(configuration)
    ResetPasswordViewModel(
      pageTitle = text.pageTitle,
      titleText = text.title,
      subtitleText = text.subtitle,
      emailAddressFieldText = text.emailAddressField,
      buttonText = text.button,
      socialNetworksTitleText = text.socialNetworksTitle,
      socialNetworksContentText = text.socialNetworksContent,
      userHelpTitleText = text.userHelpTitle,
      userHelpEmailText = text.userHelpEmail,
      userHelpContentText = text.userHelpContent,
      resources = layout.resources,
      indirectResources = layout.indirectResources
    )
  }
}
