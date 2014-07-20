package org.ensime;

import java.util.concurrent.Executor;
import java.lang.Thread.UncaughtExceptionHandler;

@SuppressWarnings("deprecation")
public class AkkaForkJoinDeprecationHack {
    public static final Executor getAkkaForkJoinPool(UncaughtExceptionHandler uncaught) {
        int cpus = Runtime.getRuntime().availableProcessors();
        akka.jsr166y.ForkJoinPool.ForkJoinWorkerThreadFactory factory = akka.jsr166y.ForkJoinPool.defaultForkJoinWorkerThreadFactory;
        return new akka.jsr166y.ForkJoinPool(cpus, factory, uncaught, true);
    }
}
