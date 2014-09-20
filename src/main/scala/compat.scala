package scala {
  package language {
    object reflectiveCalls
  }

  package concurrent {
    // we can't put this in the concurrent package
    // because 2.9.3 introduced some of the elements
    object backport {

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
          import org.ensime.AkkaForkJoinDeprecationHack.getAkkaForkJoinPool
          implicit val global: akka.dispatch.ExecutionContext =
            akka.dispatch.ExecutionContexts.fromExecutor(getAkkaForkJoinPool(uncaught))
        }
      }

      object duration {
        type FiniteDuration = akka.util.FiniteDuration
        type Duration = akka.util.Duration
        val Duration = akka.util.Duration
        implicit def intToDuration(n: Int) = akka.util.duration.intToDurationInt(n)
        implicit def longToDuration(n: Long) = akka.util.duration.longToDurationLong(n)
      }
    }
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
      type File = scala.tools.nsc.io.File
      val File = scala.tools.nsc.io.File
      type Path = scala.tools.nsc.io.Path
      val Path = scala.tools.nsc.io.Path
      type AbstractFile = scala.tools.nsc.io.AbstractFile
      val AbstractFile = scala.tools.nsc.io.AbstractFile
      type ZipArchive = scala.tools.nsc.io.ZipArchive
      val ZipArchive = scala.tools.nsc.io.ZipArchive
    }
  }

  object Compat210 {
    def ??? : Nothing = throw new UnsupportedOperationException

  }

}
