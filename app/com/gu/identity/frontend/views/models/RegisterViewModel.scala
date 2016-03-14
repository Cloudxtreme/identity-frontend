package com.gu.identity.frontend.views.models

import com.gu.identity.frontend.configuration.Configuration
import com.gu.identity.frontend.controllers.routes
import com.gu.identity.frontend.csrf.CSRFToken
import com.gu.identity.frontend.models.{GuardianMembersClientID, ClientID, UrlBuilder, ReturnUrl}
import com.gu.identity.frontend.models.text.RegisterText
import com.gu.identity.frontend.mvt._
import play.api.i18n.Messages


case class RegisterViewModel(
    layout: LayoutViewModel,

    oauth: OAuthRegistrationViewModel,

    registerPageText: RegisterText,
    terms: TermsViewModel,

    hasErrors: Boolean,
    errors: Seq[ErrorViewModel],

    showStandfirst: Boolean,

    csrfToken: Option[CSRFToken],
    returnUrl: String,
    skipConfirmation: Boolean,
    clientId: Option[ClientID],

    actions: RegisterActions,
    links: RegisterLinks,

    resources: Seq[PageResource with Product],
    indirectResources: Seq[PageResource with Product])
  extends ViewModel with ViewModelResources


object RegisterViewModel {

  def apply(
      configuration: Configuration,
      activeTests: ActiveMultiVariantTests,
      errors: Seq[ErrorViewModel],
      csrfToken: Option[CSRFToken],
      returnUrl: ReturnUrl,
      skipConfirmation: Option[Boolean],
      clientId: Option[ClientID])
      (implicit messages: Messages): RegisterViewModel = {

    val layout = LayoutViewModel(configuration, activeTests, clientId, Some(returnUrl))

    RegisterViewModel(
      layout = layout,

      oauth = OAuthRegistrationViewModel(configuration, returnUrl, skipConfirmation, clientId),

      registerPageText = RegisterText(),
      terms = TermsViewModel(),

      hasErrors = errors.nonEmpty,
      errors = errors,

      showStandfirst = showStandfirst(activeTests, clientId),

      csrfToken = csrfToken,
      returnUrl = returnUrl.url,
      skipConfirmation = skipConfirmation.getOrElse(false),
      clientId = clientId,

      actions = RegisterActions(),
      links = RegisterLinks(returnUrl, skipConfirmation, clientId),

      resources = layout.resources,
      indirectResources = layout.indirectResources
    )
  }

  private def showStandfirst(activeTests: ActiveMultiVariantTests, clientId: Option[ClientID]) =
    clientId.contains(GuardianMembersClientID) && activeTests.contains(RegisterMembershipStandfirstTest)

}


case class RegisterActions private(
    register: String)

object RegisterActions {
  def apply(): RegisterActions =
    RegisterActions(
      register = routes.RegisterAction.register().url
    )
}


case class RegisterLinks private(
    signIn: String)

object RegisterLinks {
  def apply(returnUrl: ReturnUrl, skipConfirmation: Option[Boolean], clientId: Option[ClientID]): RegisterLinks =
    RegisterLinks(
      signIn = UrlBuilder(routes.Application.signIn().url, returnUrl, skipConfirmation, clientId, group = None)
    )
}
