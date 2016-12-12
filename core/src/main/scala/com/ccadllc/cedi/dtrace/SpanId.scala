/*
 * Copyright 2016 Combined Conditional Access Development, LLC.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.ccadllc.cedi.dtrace

import java.util.UUID

import scala.language.higherKinds
import scala.util.{ Random, Try }
import scala.util.matching.Regex

import fs2.util.Async
import fs2.util.syntax._

case class SpanId(traceId: UUID, parentSpanId: Long, spanId: Long) {

  def root: Boolean = parentSpanId == spanId

  val toHeader: String = s"${SpanId.TraceIdHeader}=$traceId;${SpanId.ParentIdHeader}=$parentSpanId;${SpanId.SpanIdHeader}=$spanId"

  def newChild[F[_]: Async]: F[SpanId] = SpanId.nextSpanIdValue map { newSpanId => copy(parentSpanId = spanId, spanId = newSpanId) }

  override def toString: String = s"SpanId~$traceId~$parentSpanId~$spanId"
}

object SpanId {
  final val HeaderName: String = "X-MoneyTrace"
  final val TraceIdHeader: String = "trace-id"
  final val ParentIdHeader: String = "parent-id"
  final val SpanIdHeader: String = "span-id"
  final val HeaderRegex: Regex = s"$TraceIdHeader=([0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-fA-F]{12});$ParentIdHeader=([\\-0-9]+);$SpanIdHeader=([\\-0-9]+)".r

  def root[F[_]](implicit F: Async[F]): F[SpanId] = for {
    traceId <- F.delay(UUID.randomUUID)
    parentChildId <- nextSpanIdValue
  } yield SpanId(traceId, parentChildId, parentChildId)

  def fromHeader(headerName: String, headerValue: String): Option[SpanId] =
    if (headerName == HeaderName) fromHeaderValue(headerValue) else None

  def fromHeaderValue(headerValue: String): Option[SpanId] = headerValue match {
    case HeaderRegex(traceId, parentId, spanId) => Try(SpanId(UUID.fromString(traceId), parentId.toLong, spanId.toLong)).toOption
    case _ => None
  }

  private[dtrace] def nextSpanIdValue[F[_]](implicit F: Async[F]): F[Long] = F.delay(Random.nextLong)

  private[dtrace] val empty: SpanId = SpanId(UUID.randomUUID, 0L, 0L)
}
