# Cedi Distributed Trace

Quick links:

- [About the library](#about)
- [Examples of use](#usage)
- [Configuration](#config)
- [How to get latest version](#getit)

### <a id="about"></a>About the library

Overview
========

The Cedi Distributed Trace library provides the capability to instrument effectful programs such that logical traces can be derived and recorded across physical processes and machines.  This instrumentation is expressed in a format that is interoperable with [Comcast Money](https://github.com/Comcast/money).  This library consists of immutable data structures which represent the instrumentation and an interpreter - the `TraceAsync[F, A]` - which annotates the underlying action (represented as an `F[A]` where `F` is the effectful action and `A` is the result type).  The `TraceAsync[F, A]` can be thought of as a function from a `TraceContext` (the cursor into the active trace) to an effectful program  whose execution you wish to trace (the effectful program can be any `F` which has an instance of `fs2.util.Async[F]` in implicit scope, such as `fs2.Task`).  Because `fs2.Task` is often used as the effectful data type, this library provides a type alias `TraceTask[A]` for `TraceAsync[Task, A]` and convenience methods to work with this type alias a `TraceTask` object.

Design Constraints
==================

This library is implemented using functional data structures and techniques and is best used by similarly constructed programs.
It is non-blocking with a small footprint and incurs a reasonably low overhead.
No special thread pools or piggybacking on thread locals and the like are employed.
`dtrace` is built on Scala and its core constructs use the [Functional Streams for Scala / FS2](https://github.com/functional-streams-for-scala/fs2) library.
It is interoperable with [Comcast Money](https://github.com/Comcast/money).  `Money` is a great library and `dtrace` was created to complement it, providing a purely functional model where `Money` has to make some consessions to Java interoperability (it is certainly conceivable that `dtrace` could at some point be incorporated into `Money`).

Background
==========

A Money-compliant *Distributed Trace* is a directed graph of *Span*s. A *Span* identifies a branch of the overall *Trace* representing a logical step or action, executing within the local process.  All but the first *Span* in a *Trace* has a Parent *Span* indicating the upstream operation which triggered its child.  *Span*'s are identified by a unique *Span Identifier* (`SpanId`) along with a parent `SpanId` (and the overall *Distributed Trace* GUID).  A *Trace*'s first *Span* has a parent `SpanId` equal to its own.  Each *Span* also consists of metadata about the action, including whether its action executed successfully or failed (and if a failure, details on it), the duration of the action execution in microseconds, where the *Span* executed (in which application; on which node; in which process; within what environment, etc), and, optionally, individual `Note`s specific to the *Span* (e.g., the `Note` with the *Host Address* of a cable settop box for an action issuing an initialize command to the device).  A logical *Trace* (for example, "issue an initialize to a settop box") might originate from a business system with its transmission *Span* passed in an HTTP header to a microservice running in the cloud which executes *Span*s to query a persistent data store before making a binary RPC call (recorded in a *Span*) to a second microservice, passing the current trace information in the RPC context, before that second microservice finally issues the initialize command to the settop, ending the *Trace*.  The *dtrace library* provides a logging `Emitter` to record the *Span*s, as they are executed, to the configured logging system in both JSON and text formats but also provides the means by which custom emitters can be provided.


### <a id="usage"></a> Examples of Use

```tut:silent
import fs2.{ Strategy, Task }
import java.util.UUID
import com.ccadllc.cedi.dtrace._
import com.ccadllc.cedi.dtrace.logging.LogEmitter
import com.ccadllc.cedi.dtrace.syntax._
import TraceSystem._

/*
 * Some simple data types for our examples.
 */
case class Result(message: String)
case class Command(action: String)
case class Host(hostName: String)
case class HttpHeader(name: String, value: String)

/*
 * We create an implicit strategy for our effectful `fs2.Task`s.
 */
implicit val strategy: Strategy = Strategy.fromFixedDaemonPool(
  Runtime.getRuntime.availableProcessors * 2, "dtrace-usage-example"
)

/*
 * Near the beginning of the universe, create a `TraceSystem` object to
 * hold the top-level information about the program (application and node name,
 * process identifier, deployment and environment names, etc.)
 */
val traceSystem = TraceSystem(
  identity = Identity(
    Identity.Application("terminal-manager", UUID.randomUUID),
    Identity.Node("terminal-manager8d3w3.mydomain.com", UUID.randomUUID),
    Identity.Process(UUID.randomUUID),
    Identity.Deployment("Ashburn-DC-East"),
    Identity.Environment("production")
  ),
 /* This emitter will write a text entry for each span to "distributed-trace.txt"
  * logger and a JSON entry for each span to "distributed-trace.json" logger; however,
  * it is easy to provide your own emitter by implementing the `TraceSystem.Emitter`
  * trait, which requires providing implementations for two methods:
  *   `def description: String` to provide a description of your emitter and
  *   `def emit[F[_]: Async](tc: TraceContext): F[Unit]` to actually do the work of
  * emitting the current Span to the destination and in the format of your choosing.
  */
  emitter = LogEmitter
)

val cmd = Command("action")
val host = Host("localhost")
val httpHeader = HttpHeader("content-type", "application/json")

def encodeInitCommand(cmd: Command): Task[Array[Byte]] = Task.now(Array(20.toByte, 10.toByte, 10.toByte))

def transmitInitCommand(bytes: Array[Byte], host: Host): Task[Result] = Task.now(Result("success!"))

def writeInitializeCommandToSettop(cmd: Command, host: Host): TraceAsync[Task, Result] = for {
 /*
  * Encode the command to a byte vector and then transmit it.  Note that the import of
  * `com.ccadllc.cedi.dtrace.syntax._` enriches the `fs2.Task` type (or any `F` with an `fs2.Async[F]`
  * instance) by adding a `newSpan` method to it using an implicit class.  The two lines that follow this
  * comment would, without the syntax enrichment, be written as:
  *  bytes <- TraceAsync.toTraceAsync(encodeInitCommand(cmd)).newSpan(Span.Name("encode-init-command"), Note.string("cmd", cmd.toString))
  *  result <- TraceAsync.toTraceAsync(transmitInitCommand(bytes, host)).newSpan(
  *    Span.Name("transmit-init-command"), Note.string("settop-host", host.toString), Note.long("payload-size", bytes.size.toLong)
  *  )
  */
  bytes <- encodeInitCommand(cmd).newSpan(Span.Name("encode-init-command"), Note.string("cmd", cmd.toString))
  result <- transmitInitCommand(bytes, host).newSpan(
    Span.Name("transmit-init-command"), Note.string("settop-host", host.toString), Note.long("payload-size", bytes.size.toLong)
  )
} yield result

/*
 * Retrieve the span, in this example, in the HTTP header from the originating business system, if it exists.
 * This logic may be included an an `akka-http` directive, for example.
 */
val rootSpanMaybe = SpanId.fromHeader(httpHeader.name, httpHeader.value) map {
  spanId => Span.newChild[Task](spanId, Span.Name("business-system-init"))
}

/*
 * We add a Span to the overall `writeInitializeCommandToSettop` action,
 * showing the ability to create Span notes from the traced action result
 * with `newAnnotatedSpan`.
 */
val tracedTask: TraceAsync[Task, Result] = writeInitializeCommandToSettop(cmd, host).newAnnotatedSpan(
  Span.Name("write-initialize-command"), Note.string("command", cmd.toString), Note.string("host", host.toString)
) { case Right(result) => Vector(Note.string("result", result.toString)) }

/*
 * We convert our traced task to a task.
 */
val task: Task[Result] = for {
  /* If there was no Span originating from another system found in the HTTP Header, we create a local root Span */
  rootSpan <- rootSpanMaybe.getOrElse(Span.root[Task](Span.Name("locally-initiated-init")))
  /*
   * The tracedTask we've derived earlier around `writeInitialCommandToSettop` (which includes
   * the encode and transmit nested actions, each with their own Spans) is an instance of `TraceAsync[Task, A]`,
   * which is a data structure associating a Span (like "write-initialize-command") with its underlying `Task`
   * (reiterating that we're using `fs2.Task` in this example, but again, `Task` can be substituted with any
   * `F` which has an `fs2.Async[F]` instance in implicit scope).  When we are done building up these annotated
   * `TraceAsync` instances, we need to "tie the knot" by converting the top-level instance back into a plain
   * `Task` again before we can actually run it. This is accomplished by applying the root `Span`
   * for this process (in this example, the one we extracted from an HTTP header) using the `trace` method on
   * on our top-level `TraceAsync` instance (represented here by the `tracedTask` value).
   */
  result <- tracedTask.trace(TraceContext(rootSpan, traceSystem))
} yield result

/*
 * Now, at the end of the universe, we run the task.  This will result, in this example using the supplied logging
 * framework Emitter, in the following items logged via the `distributed-trace.txt` logger:
 *   Span: [ span-id=-4268861818882462019 ] [ trace-id=2a71fb7b-f38d-4f6a-a4d1-229c6c5bc963 ] [ parent-id=-6262761813211462065 ]
 *     [ span-name=encode-init-command] [ app-name=terminal-manager ] [ start-time=2016-09-26T00:29:14.802Z ]
 *     [ span-duration=2500 microseconds ] [ span-success=true ] [ failure-detail=N/A ][ notes=[name=command,value=INIT] ]
 *     [ node-name=terminal-manager8d3w3.mydomain.com ]
 *
 *   Span: [ span-id=-2264899918881452036 ] [ trace-id=2a71fb7b-f38d-4f6a-a4d1-229c6c5bc963 ] [ parent-id=-6262761813211462065 ]
 *     [ span-name=transmit-init-command] [ app-name=terminal-manager ] [ start-time=2016-09-26T00:29:14.799Z ]
 *     [ span-duration=2500 microseconds ] [ span-success=true ] [ failure-detail=N/A ][ notes=[name=command,value=INIT] ]
 *     [ node-name=terminal-manager8d3w3.mydomain.com ]
 *
 *   Span: [ span-id=-6262761813211462065 ] [ trace-id=2a71fb7b-f38d-4f6a-a4d1-229c6c5bc963 ] [ parent-id=-9466761813211462033 ]
 *     [ span-name=write-initialize-command] [ app-name=terminal-manager ] [ start-time=2016-09-26T00:29:14.797Z ]
 *     [ span-duration=5000 microseconds ] [ span-success=true ] [ failure-detail=N/A ][ notes=[name=command,value=INIT] ]
 *     [ node-name=terminal-manager8d3w3.mydomain.com ]
 *
 *   Span: [ span-id=-9466761813211462033 ] [ trace-id=2a71fb7b-f38d-4f6a-a4d1-229c6c5bc963 ] [ parent-id=2488084092502843745 ]
 *     [ span-name=business-system-init] [ app-name=terminal-manager ] [ start-time=2016-09-26T00:29:14.793Z ]
 *     [ span-duration=5110 microseconds ] [ span-success=true ] [ failure-detail=N/A ][ notes=[] ]
 *     [ node-name=terminal-manager8d3w3.mydomain.com ]
 */
task.unsafeRun()
```

### <a id="getit"></a>How to get latest Version

Cedi Distributed Trace supports Scala 2.10, 2.11, and 2.12. This distribution will be published to Maven Central soon and consists of two library components.

dtrace-core
===========

This is the core functionality, recording trace and span information over effectful programs, passing these recorded events to registred emitters for disposition.


```scala
libraryDependencies += "com.ccadllc.cedi" %% "dtrace-core" % "1.0.0-SNAPSHOT"
```

dtrace-logging
==============

This component provides emitters for logging the trace spans in text and/or JSON format using the `sf4j` logging framework.  It uses the `circe` library for formatting the trace span information as JSON.

```scala
libraryDependencies ++= "com.ccadllc.cedi" %% "dtrace-logging" % "1.0.0-SNAPSHOT"
```

## Copyright and License

This project is made available under the [Apache License, Version 2.0](LICENSE). Copyright information can be found in [NOTICE](NOTICE).
