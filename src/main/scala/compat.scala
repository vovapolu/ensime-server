package scala {
  package language {
    object reflectiveCalls
  }

  package object concurrent {
    val ExecutionContexts = akka.dispatch.ExecutionContexts
    type Future[T] = akka.dispatch.Future[T]
    val Future = akka.dispatch.Future
    type Promise[T] = akka.dispatch.Promise[T]
    val Promise = akka.dispatch.Promise
    val Await = akka.dispatch.Await

    type ExecutionContext = akka.dispatch.ExecutionContext
    object ExecutionContext {
      def fromExecutor(e: java.util.concurrent.Executor): ExecutionContext =
        akka.dispatch.ExecutionContext.fromExecutor(e)

      object Implicits {
        private val uncaught: Thread.UncaughtExceptionHandler = new Thread.UncaughtExceptionHandler {
          def uncaughtException(t: Thread, e: Throwable): Unit = {
            // more closely resembles 2.10 behaviour than using a Log
            System.err.println("uncaught exception in " + t.getName)
            e.printStackTrace
          }
        }

        // java 6 compatible
        implicit val global: ExecutionContext =
          akka.dispatch.ExecutionContexts.fromExecutor(
            new akka.jsr166y.ForkJoinPool(
              Runtime.getRuntime.availableProcessors,
              akka.jsr166y.ForkJoinPool.defaultForkJoinWorkerThreadFactory,
              uncaught, true)
          )
      }
    }
    // we can't do the duration stuff because of awesome differences between 2.9.2 and 2.9.3
  }

  package object reflect {
    type ClassTag[T] = ClassManifest[T]
  }
  package reflect {

    package internal {
      package object util {
        //type NoPosition = scala.tools.nsc.util.NoPosition
        val NoPosition = scala.tools.nsc.util.NoPosition
        type Position = scala.tools.nsc.util.Position
        val Position = scala.tools.nsc.util.Position
        type RangePosition = scala.tools.nsc.util.RangePosition
        type OffsetPosition = scala.tools.nsc.util.OffsetPosition
        type SourceFile = scala.tools.nsc.util.SourceFile
        type BatchSourceFile = scala.tools.nsc.util.BatchSourceFile
        val FakePos = scala.tools.nsc.util.FakePos

      }
    }
    package object io {
      type AbstractFile = scala.tools.nsc.io.AbstractFile
      //val AbstractFile = scala.tools.nsc.io.AbstractFile
    }
  }

  object Compat210 {
    def ??? : Nothing = throw new UnsupportedOperationException

  }

}
