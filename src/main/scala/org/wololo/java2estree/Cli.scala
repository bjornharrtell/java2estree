package org.wololo.java2estree

import java.io.File
import java.io.FileReader
import org.eclipse.jdt.core.dom.AST
import org.eclipse.jdt.core.dom.ASTParser
import org.eclipse.jdt.core.dom.CompilationUnit
import org.eclipse.jface.text.Document
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.SerializationFeature
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.google.common.io.CharStreams
import com.typesafe.scalalogging.LazyLogging
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.nio.file.StandardOpenOption

object Cli extends App with LazyLogging {
  val path = new File(args(0))

  val parser = ASTParser.newParser(AST.JLS8)
  
  logger.info(s"Walking ${path}")
  
  walk(path)
  
  def walk(dir: File) : Unit = {
    dir.listFiles().foreach { 
      case x if x.isDirectory() => walk(x) 
      case x if x.getName.endsWith("java") => convert(x)
      case x => Nil
    }
  }
  
  def convert(file: File) {
    logger.info(s"Processing ${file.getName}")
    parser.setResolveBindings(true)
    parser.setBindingsRecovery(true)
    parser.setStatementsRecovery(true)
    parser.setEnvironment(null, Array(path.toString), null, true)
    val baseName = file.getName.split('.')(0)
    val doc = new Document(CharStreams.toString(new FileReader(file)))
    parser.setUnitName(file.getName)
    parser.setSource(doc.get.toCharArray)
    val cu = parser.createAST(null).asInstanceOf[CompilationUnit]
    val program = Converters.toProgram(cu, path.toString, baseName)
    val mapper = new ObjectMapper
    mapper.registerModule(DefaultScalaModule)
    mapper.enable(SerializationFeature.INDENT_OUTPUT)
    val os = Files.newOutputStream(Paths.get(file.getParent, baseName + ".ast"), StandardOpenOption.CREATE)
    mapper.writeValue(os, program)
  }
}
