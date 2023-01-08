package advent

import scala.collection.SortedMap

sealed trait File
case class Dir(name: String, files: Map[String, File]) extends File {
  def getSizeOfDir(): Int =
    files.values.toList.map {
      case SingleFile(_, size) => size
      case d: Dir              => d.getSizeOfDir()
    }.sum
}
case class SingleFile(name: String, size: Int) extends File

sealed trait ShellOutput

case class DirOutput(name: String) extends ShellOutput
case class FileOutput(name: String, size: Int) extends ShellOutput
case class Cd(dirName: String) extends ShellOutput
case class Ls() extends ShellOutput

def getLineType(line: String): ShellOutput = line match {
  case l if (l.startsWith("dir ")) =>
    val dirName = l.slice(4, l.length)
    DirOutput(dirName)
  case l if (line.startsWith("$ cd ")) =>
    Cd(line.slice(5, line.length))
  case l if (line.startsWith("$ ls")) =>
    Ls()
  case l =>
    val sizeStr :: name :: tail = l.split(" ").toList
    FileOutput(name, sizeStr.toInt)
}

object day7:
  def run() =
    val input = readFile("resources/day7.txt").map(getLineType)

    val (rootDir, output) = makeFileFromShellOutput(input)


    def accDir(tree: Map[String, File], recurse: Boolean = true): List[(Int, String)] = {
      val dirs = tree.values.toList.collect { case d: Dir =>
        d
      }

      List.empty[(Int, String)]
        ++ dirs.map(dir => (dir.getSizeOfDir() -> dir.name))
        ++ (if (recurse) (dirs.flatMap(dir => accDir(dir.files))) else List.empty)
    }

    val allDirsAndSizes = accDir(rootDir.files)
    val dirsBelow10k =
      allDirsAndSizes.filter((size, _) => size <= 100_000)

    val totalSize = accDir(rootDir.files, false).map((size, _) => size).sum
    val sizeOfDisk = 70_000_000
    val spaceLeftOnDisk = sizeOfDisk - totalSize
    val spaceNeeded = 30_000_000
    val spaceToRemove = spaceNeeded - spaceLeftOnDisk
    val dirToRemove =  allDirsAndSizes.filter((size, _) => size >= spaceToRemove).sortBy(_._1).head

    println(s"Sum of dirs < 10k: ${dirsBelow10k.sortBy((size, _) => size).map(_._1).sum}")
    println(s"Total size of dir: ${totalSize}")
    println(s"Space left on disk: ${spaceLeftOnDisk}")
    println(s"Space to clear: ${spaceToRemove}")
    println(s"Smallest directory that matches: ${dirToRemove}")

  def makeFileFromShellOutput(
      input: List[ShellOutput]
  ): (Dir, List[ShellOutput]) = input match {
    case Cd(currentDirName) :: ls :: inputToParse =>
      println(s"cd'ing into ${currentDirName}")

      assert(ls.isInstanceOf[Ls], s"Command after cd is not ls, ${ls}")

      val fileOutputs =
        inputToParse.takeWhile(_.isInstanceOf[DirOutput | FileOutput])
      var fileMap: Map[String, File] = fileOutputs.collect {
        case FileOutput(name, size) => (name -> SingleFile(name, size))
      }.toMap

      var remainingInputToParse =
        inputToParse.slice(fileOutputs.length, inputToParse.length)

      remainingInputToParse.headOption match {
        case Some(cd) =>
          var currentCd = cd
          while (currentCd != Cd("..")) {
            val (nestedDir, _remainingInput) = makeFileFromShellOutput(
              remainingInputToParse
            )
            fileMap = fileMap + (nestedDir.name -> nestedDir)
            remainingInputToParse = _remainingInput
            currentCd = remainingInputToParse.headOption.getOrElse(Cd(".."))
          }

          println(s"done with ${currentDirName}")

          val dir = Dir(currentDirName, fileMap)

          (dir, remainingInputToParse.drop(1))
        case None =>
          // No more input
          (Dir(currentDirName, fileMap), List.empty)
      }

    case line => throw new Error(s"Weird line: ${line}")
  }
