/*
 * Copyright (c) 2018 PS-L. All rights reserved.
 */

package scalar.command
import scalar.filesystem.State

class UnknownCommands extends Command {

  override def apply(state: State): State =
    state.setMessage("command does not found")
}
