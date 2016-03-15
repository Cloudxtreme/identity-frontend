package com.gu.identity.frontend.authentication


object CookieName extends Enumeration {

  type Name = Value
  val GU_U, SC_GU_U, SC_GU_LA, GU_ID_CSRF, gu_user_features_expiry, gu_paying_member = Value

  val SECURE_COOKIE_PREFIX = "SC"

  def isSecureCookie(cookieName: String) = cookieName.startsWith(SECURE_COOKIE_PREFIX)
}
