package com.gu.identity.frontend.models.text

import play.api.i18n.Messages

sealed trait OAuthText {
  val facebook: String
  val google: String
}

object OAuthText {
  def facebook(implicit messages: Messages): String =
    messages("oauth.facebook")

  def google(implicit messages: Messages): String =
    messages("oauth.google")
}

case class OAuthSignInText private(
    facebook: String,
    google: String)
  extends OAuthText

object OAuthSignInText {
  import OAuthText._

  def apply()(implicit messages: Messages): OAuthSignInText =
    OAuthSignInText(
      facebook = messages("oauth.signIn", facebook),
      google = messages("oauth.signIn", google)
    )
}

case class OAuthRegistrationText private(
    facebook: String,
    google: String)
  extends OAuthText

object OAuthRegistrationText {
  import OAuthText._

  def apply()(implicit messages: Messages): OAuthRegistrationText =
    OAuthRegistrationText(
      facebook = messages("oauth.register", facebook),
      google = messages("oauth.register", google)
    )
}

case class OAuthPermissionsText private(a: String, b: String)

object OAuthPermissionsText {
  def apply()(implicit messages: Messages): OAuthPermissionsText = OAuthPermissionsText(
    a = messages("oauth.permission.a"),
    b = messages("oauth.permission.b")
  )
}
