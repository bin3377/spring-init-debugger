package com.sumologic.springdebugger

import org.apache.commons.cli.Options

object Main extends App {

  val banner = """
   . .       . .       . .    .       . .
.+'|=|`+. .+'| |`+. .+'|=|`+.=|`+. .+'|=|`+.
|  | `+.| |  | |  | |  | `+ | `+ | |  | |  |
|  | .    |  | |  | |  |  | |  | | |  | |  |
`+.|=|`+. |  | |  | |  |  | |  | | |  | |  |
.    |  | |  | |  | |  |  | |  | | |  | |  |
|`+. |  | |  | |  | |  |  | |  | | |  | |  |
`+.|=|.+' `+.|=|.+' `+.|  |.|  |+' `+.|=|.+'
"""
  println(banner)

  val cmdOptions: Options = new Options()
    .addOption("logfile", true, "path of log file contains debug log of beans loading (required)")
    .addOption("help", false, "print this message")
    .addOption("def", false, "print all beans defined in the log")
    .addOption("tree", false, "print full dependency tree of beans loading")
    .addOption("suspicious", false, "detect and print suspicious dependencies")
    .addOption("exception", false, "detect and print exceptions of beans loading")
    .addOption("verbose", false, "print verbose of beans loading")
    .addOption("nodes", true, "print dependencies of nodes which name matches patten")

  val parser = new LogParser("/Users/byi/Downloads/app.log")
  parser.loadUnits()
  parser.loadRelationships()
  println(parser.getDefinitions())
  println(parser.getDependencyTree)
  println(parser.getAllSuspiciousNodes)

}
