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

object Cli extends App with LazyLogging {
  val path = new File(args(0)).toString
  val file = new File(args(1))
  val filename = file.getName
  
  val doc = new Document(CharStreams.toString(new FileReader(file)))
  val parser = ASTParser.newParser(AST.JLS8)
  parser.setResolveBindings(true)
  parser.setBindingsRecovery(true)
  parser.setStatementsRecovery(true)
  parser.setEnvironment(null, Array(path), null, true)
  parser.setUnitName(filename)
  parser.setSource(doc.get.toCharArray)
  val cu = parser.createAST(null).asInstanceOf[CompilationUnit]
  
  val program = Converters.toProgram(cu)
  val mapper = new ObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.enable(SerializationFeature.INDENT_OUTPUT)
  
  mapper.writeValue(System.out, program)
}
