package org.wololo.java2estree

import org.eclipse.jdt.core.dom
import java.io.File
import org.wololo.estree._

object importdeclaration {
  def fromImportDeclaration(id: dom.ImportDeclaration, path: String): (String, String) = {
    val orgname = id.getName.getFullyQualifiedName
    val path = orgname.replace('.', '/')
    val name = orgname.split('.').last
    (name -> path)
  }
  
  def createImport(name: String, path: String) = {
    new ImportDeclaration(
      List(new ImportDefaultSpecifier(new Identifier(name))),
        new Literal(s"'${path}'", s"'${path}'"))
  }
  
  def builtinImports() : Map[String, String] = {
    Map(
      ("Comparable" -> "java/lang/Comparable"),
      ("Cloneable" -> "java/lang/Cloneable"),
      ("Character" -> "java/lang/Character"),
      ("Double" -> "java/lang/Double"),
      ("Exception" -> "java/lang/Exception"),
      ("RuntimeException" -> "java/lang/RuntimeException")
    )
  }
  
  def importsFromName(name: String, path: String, ignore: String = null) : Map[String, String] = {
    val subpath = name.replace('.', '/')
    val subname = name.split('.').last
 
    def isJava(file: File): Boolean = {
      val split = file.getName.split('.')
      if (split.length != 2) return false
      if (split(1) == "java") return true
      return false
    }
    
    val file = new File(path + '/' + subpath)
    val files = file.listFiles
    if (files == null) return Map()
    
    val pairs = files.filter({ x => x.getName.split('.')(0) != ignore }).collect { case x if isJava(x) => {
      val name = x.getName.split('.')(0)
      val path = subpath + '/' + name
      (name -> path)
    } }
    
    Map(pairs: _*)
  }
}