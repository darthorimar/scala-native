package scala.scalanative
package nscplugin

import java.nio.file.{Path, Paths}

import scala.tools.nsc._
import scala.tools.nsc.io.AbstractFile
import scala.scalanative.nir.serialization.{serializeBinary, serializeText}
import scala.scalanative.io.withScratchBuffer
import scala.scalanative.io.VirtualDirectory
import scala.scalanative.nir.Defn

trait NirGenFile { self: NirGenPhase =>
  import global._

  def genPathFor(cunit: CompilationUnit, sym: Symbol): Path = {
    val baseDir: AbstractFile =
      settings.outputDirs.outputDirFor(cunit.source.file)

    val id        = genTypeName(sym).id
    val pathParts = id.split("[./]")
    val dir       = (baseDir /: pathParts.init)(_.subdirectoryNamed(_))

    var filename = pathParts.last
    val file     = dir fileNamed (filename + ".nir")

    Paths.get(file.file.getAbsolutePath)
  }

  def genIRFile(path: Path, defns: Seq[nir.Defn]): Unit = {
    withScratchBuffer { buffer =>
      serializeBinary(defns, buffer)
      buffer.flip
      VirtualDirectory.local(path.getParent).write(path, buffer)
    }


    if (path.toString.toLowerCase.contains("test")) {
      println(Console.BLUE + "Hi :)" + Console.RESET)
      val textPath = Paths.get(path.toString + "nirt")
      withScratchBuffer { buffer =>
        serializeText(defns, buffer)
        buffer.flip
        VirtualDirectory.local(textPath.getParent).write(textPath, buffer)
      }
    }
  }
}
