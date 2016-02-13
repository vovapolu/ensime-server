// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime
package config

import java.net.{ JarURLConnection, URL }

object Environment {
  def info: String = s"""
    |Environment:
    |  OS : $osVersion
    |  Java : $javaVersion
    |  Scala version: $scalaVersion
    |  Ensime : $ensimeVersion
    |  Built with Scala version: ${BuildInfo.scalaVersion}
    |  Built with sbt version: ${BuildInfo.sbtVersion}
    |  Built from git sha: ${BuildInfo.gitSha}
    |  Built on: ${BuildInfo.builtAtString}
  """.trim.stripMargin

  private def osVersion: String =
    System.getProperty("os.name")

  private def javaVersion: String = {
    val vmInfo = System.getProperty("java.vm.name") + " " + System.getProperty("java.vm.version")
    val rtInfo = System.getProperty("java.runtime.name") + " " + System.getProperty("java.runtime.version")
    vmInfo + ", " + rtInfo
  }

  private def scalaVersion: String =
    scala.util.Properties.versionString

  private def ensimeVersion: String =
    BuildInfo.version

  def shutdownOnDisconnectFlag: Boolean = {
    Option(System.getProperty("ensime.explode.on.disconnect")).isDefined
  }
}
