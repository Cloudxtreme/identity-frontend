package com.gu.identity.service.client

import org.joda.time.DateTime

sealed trait ApiResponse

case class ApiErrorResponse(errors: List[ApiErrorResponseError]) extends ApiResponse
case class ApiErrorResponseError(message: String, description: Option[String] = None, context: Option[String] = None)

case class AuthenticationCookiesResponse(cookies: AuthenticationCookiesResponseCookieList) extends ApiResponse
case class AuthenticationCookiesResponseCookieList(expiresAt: DateTime, values: List[AuthenticationCookiesResponseCookie])
case class AuthenticationCookiesResponseCookie(key: String, value: String, sessionCookie: Option[Boolean] = None)

case class RegisterResponse(user: RegisterResponseUser) extends ApiResponse
case class RegisterResponseUser(userGroups: List[RegisterResponseUserGroups])
case class RegisterResponseUserGroups(packageCode: String, path: String)

case class SendResetPasswordEmailResponse() extends ApiResponse
