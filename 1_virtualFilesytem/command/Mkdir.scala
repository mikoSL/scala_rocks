/*
 * Copyright (c) 2018 PS-L. All rights reserved.
 */

package scalar.command
import scalar.files.{DirEntry, Directory}
import scalar.filesystem.State

class Mkdir(name: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)) {
      state.setMessage("Entry " + name + " already exist")
    } else if (name.contains(Directory.SEPARATOR)) {
      // mkdir separator is not allowed.
      state.setMessage(name + " must not contain separators!")
    } else if (checkIllegal(name)) {
      state.setMessage(name + ": illegal entry name")
    } else {
      doMkdir(state, name)
    }
  }

  def checkIllegal(str: String): Boolean = {
    name.contains(".")
  }

  def doMkdir(state: State, str: String): State = {

    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): DirEntry = {
      if (path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        val oldEntry = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }


    val wd = state.wd


    // all the directories in the full path
    val allDirsInPath = wd.getAllFoldersInPath

    // create new directory entry in wd
    val newDir = Directory.empty(wd.path, name)

    // update the whole directory structure starting from the root
    val newRoot = updateStructure(state.root, allDirsInPath, newDir)

    // find new wd instance given wd's full path, in the new directory structure.
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
  }
}
