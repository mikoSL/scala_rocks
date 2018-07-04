/*
 * Copyright (c) 2018 PS-L. All rights reserved.
 */

package scalar.filesystem

import java.util.Scanner

import scalar.command.Command
import scalar.files.Directory

/**
  * scala practice 7.2018.
  */

object Filesystem extends App {

  val root = Directory.ROOT
  var state = State(root, root)
  val scanner = new Scanner(System.in)

  while(true) {

    state.show
    val input = scanner.nextLine()
    state = Command.from(input).apply(state)

  }


}
