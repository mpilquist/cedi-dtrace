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

import fs2.util.Applicative

import java.util.UUID

import scala.language.higherKinds

case class TraceSystem[F[_]](identity: TraceSystem.Identity, emitter: TraceSystem.Emitter[F]) {
  override def toString: String = s"[emitter=$identity] [emitter=${emitter.description}]"
}

object TraceSystem {

  case class Identity(app: Identity.Application, node: Identity.Node, process: Identity.Process, deployment: Identity.Deployment, environment: Identity.Environment) {
    override def toString: String = s"[app=${app.name}] [node=${node.name}] [process=${process.id}] [deployment=${deployment.name}] [environment=${environment.name}]"
  }

  object Identity {
    final case class Application(name: String, id: UUID)
    final case class Node(name: String, id: UUID)
    final case class Process(id: UUID)
    final case class Deployment(name: String)
    final case class Environment(name: String)
  }

  trait Emitter[F[_]] {
    def emit(tc: TraceContext[F]): F[Unit]
    def description: String
  }

  private[dtrace] def empty[F[_]](implicit F: Applicative[F]): TraceSystem[F] = {
    import Identity._
    TraceSystem[F](
      Identity(Application("", UUID.randomUUID), Node("", UUID.randomUUID), Process(UUID.randomUUID), Deployment(""), Environment("")),
      new Emitter[F] {
        override def emit(tc: TraceContext[F]): F[Unit] = F.pure(())
        override val description: String = "Empty Emitter"
      }
    )
  }
}
