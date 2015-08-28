package org.wololo.java2estree

import java.io.File
import java.io.InputStreamReader
import scala.collection.JavaConverters.seqAsJavaListConverter
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.SerializationFeature
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.google.common.io.CharStreams
import org.eclipse.jdt.core.JavaCore
import java.nio.file.Files
import java.io.FileReader
import org.eclipse.jface.text.Document
import org.eclipse.jdt.core.dom.ASTParser
import org.eclipse.jdt.core.dom.AST
import org.eclipse.jdt.core.dom.CompilationUnit
import org.eclipse.jdt.core.dom.IVariableBinding

object Cli extends App {
  var filename = args(0)

  val input = new File(filename)
  // val base = filename.split("\\.")(0)
  // val output = new File(base + ".ast")
  
  val doc = new Document(CharStreams.toString(new FileReader(input)));
  val parser = ASTParser.newParser(AST.JLS8)
  parser.setResolveBindings(true)
  parser.setBindingsRecovery(true)
  parser.setStatementsRecovery(true)
  parser.setEnvironment(null, null, null, true)
  parser.setUnitName("Coordinate.java")
  parser.setSource(doc.get().toCharArray())
  val cu = parser.createAST(null).asInstanceOf[CompilationUnit]
  
  val program = Converters.program(cu)
  val mapper = new ObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.enable(SerializationFeature.INDENT_OUTPUT)

  //println(mapper.writeValueAsString(program))
  
  val pb = new ProcessBuilder(List("./node_modules/astring/bin/astring", "--indent", "  ").asJava)
  val p = pb.start()

  mapper.writeValue(p.getOutputStream, program)

  println(CharStreams.toString(new InputStreamReader(p.getInputStream, "UTF-8")))
}
