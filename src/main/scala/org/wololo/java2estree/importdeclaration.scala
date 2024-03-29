package org.wololo.java2estree

import org.eclipse.jdt.core.dom
import java.io.File
import org.wololo.estree._
import java.nio.file.Path
import java.nio.file.Paths
import org.eclipse.core

def makeRelative(path: Path, root: Path, file: Path): String =
  var parent = file.getParent
  if (parent == null)
      parent = Path.of("");
  val folder = root.relativize(parent)
  val relPath = folder.relativize(path)
  if (!relPath.toString().startsWith(".") && !relPath.toString().startsWith("/"))
    "./" + relPath.toString
  else
    relPath.toString

def fromImportDeclaration(id: dom.ImportDeclaration, root: Path, file: Path): (String, String) =
  val orgname = id.getName.getFullyQualifiedName
  val path = orgname.replace('.', '/')
  val name = orgname.split('.').last

  //if (path.startsWith("org"))
  (name -> makeRelative(Paths.get(path), root, file))
  //else 
  //  (name -> path)

def createImport(name: String, path: String) =
  new ImportDeclaration(
    List(new ImportDefaultSpecifier(new Identifier(name))),
    new Literal(s"'${path}.js'", s"'${path}.js'"))

def builtinImports(root: Path, file: Path): Map[String, String] =
  Map(
    ("System" -> "java/lang/System"),
    ("Comparable" -> "java/lang/Comparable"),
    ("Cloneable" -> "java/lang/Cloneable"),
    ("Character" -> "java/lang/Character"),
    ("Integer" -> "java/lang/Integer"),
    ("Double" -> "java/lang/Double"),
    ("StringBuffer" -> "java/lang/StringBuffer"),
    ("StringBuilder" -> "java/lang/StringBuilder"),
    ("Exception" -> "java/lang/Exception"),
    ("RuntimeException" -> "java/lang/RuntimeException"),
    ("IllegalStateException" -> "java/lang/IllegalStateException"),
    ("IllegalArgumentException" -> "java/lang/IllegalArgumentException"),
    ("UnsupportedOperationException" -> "java/lang/UnsupportedOperationException"),
    ("hasInterface" -> "hasInterface")).transform((name: String, path: String) => makeRelative(Paths.get(path), root, file))

def importsFromName(name: String, root: Path, ignore: String = null): Map[String, String] =
  val subpath = name.replace('.', '/')
  val subname = name.split('.').last

  def isJava(file: File): Boolean =
    val split = file.getName.split('.')
    if (split.length != 2) return false
    if (split(1) == "java") return true
    return false

  val file = Paths.get(root.toString, "//", subpath)
  val files = file.toFile.listFiles
  if (files == null) return Map()

  val pairs = files.filter({ x => x.getName.split('.')(0) != ignore }).collect {
    case x if isJava(x) =>
      val name = x.getName.split('.')(0)
      val path = subpath + '/' + name
      //if (path.startsWith("org"))
      (name -> makeRelative(Paths.get(path), root, x.toPath))
      //else 
      //  (name -> path)
  }

  Map(pairs: _*)
