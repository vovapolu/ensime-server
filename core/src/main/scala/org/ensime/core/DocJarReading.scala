package org.ensime.core

import akka.util.ByteString
import java.util.jar.JarFile
import org.ensime.api.EnsimeConfig

import pimpathon.java.io._

trait DocJarReading {
  def config: EnsimeConfig

  def docJarContent(filename: String, entry: String): Option[ByteString] = for {
    file <- config.allDocJars.find(_.getName == filename)
    jar = new JarFile(file)
    entry <- Option(jar.getJarEntry(entry))
    stream = jar.getInputStream(entry)
  } yield ByteString(stream.toByteArray)
}
