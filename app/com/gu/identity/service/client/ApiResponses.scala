package com.gu.identity.service.client

import com.gu.identity.service.client.models.User
import org.joda.time.DateTime

sealed trait ApiResponse

case class ApiErrorResponse(errors: List[ApiErrorResponseError]) extends ApiResponse
case class ApiErrorResponseError(message: String, description: Option[String] = None, context: Option[String] = None)

case class  AuthenticationCookiesResponse(cookies: AuthenticationCookiesResponseCookieList) extends ApiResponse
case class AuthenticationCookiesResponseCookieList(expiresAt: DateTime, values: List[AuthenticationCookiesResponseCookie])
case class AuthenticationCookiesResponseCookie(key: String, value: String, sessionCookie: Option[Boolean] = None)

case class DeauthenticationCookiesResponse(cookies: DeauthenticationCookiesResponseCookieList) extends ApiResponse
case class DeauthenticationCookiesResponseCookieList(expiresAt: DateTime, values: List[DeauthenticationCookiesResponseCookie])
case class DeauthenticationCookiesResponseCookie(key: String, value: String)

case class RegisterResponse(user: RegisterResponseUser) extends ApiResponse
case class RegisterResponseUser(userGroups: List[RegisterResponseUserGroups])
case class RegisterResponseUserGroups(packageCode: String, path: String)

case class SendResetPasswordEmailResponse() extends ApiResponse

case class UserResponse(user: User) extends ApiResponse

case class AssignGroupResponse(groupCode: String) extends ApiResponse
