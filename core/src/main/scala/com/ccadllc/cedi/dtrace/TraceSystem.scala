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

import fs2.util.Async

import java.util.UUID

import scala.language.higherKinds

case class TraceSystem(identity: TraceSystem.Identity, emitter: TraceSystem.Emitter) {
  override def toString: String = s"[emitter=$identity] [emitter=${emitter.description}]"
}
object TraceSystem {
  case class Identity(app: Identity.Application, node: Identity.Node, process: Identity.Process, deployment: Identity.Deployment, environment: Identity.Environment) {
    override def toString: String = s"[app=${app.name}] [node=${node.name}] [process=${process.id}] [deployment=${deployment.name}] [environment=${environment.name}]"
  }
  object Identity {
    case class Application(name: String, id: UUID)
    case class Node(name: String, id: UUID)
    case class Process(id: UUID)
    case class Deployment(name: String)
    case class Environment(name: String)
  }
  trait Emitter {
    def emit[F[_]: Async](tc: TraceContext): F[Unit]
    def description: String
  }
  import Identity._
  private[dtrace] val empty: TraceSystem = TraceSystem(
    Identity(Application("", UUID.randomUUID), Node("", UUID.randomUUID), Process(UUID.randomUUID), Deployment(""), Environment("")),
    new Emitter {
      override def emit[F[_]](tc: TraceContext)(implicit F: Async[F]): F[Unit] = F.pure(())
      override val description: String = "Empty Emitter"
    }
  )
}
