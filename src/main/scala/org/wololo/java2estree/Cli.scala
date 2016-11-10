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
  
  val skipList = List(
    "/jts/awt/",
    "/jts/io/",
    "/jts/geom/prep/",
	  "/jts/util/Debug.java",
	  "/jts/util/Stopwatch.java",
	  "/jts/geom/Geometry.java",
	  "/jts/index/quadtree/DoubleBits.java",
	  "/jts/geom/TopologyException.java"
  ).map(e => ".*" + e + ".*")
  
  val root = Paths.get(args(0))
  val outpath = Paths.get(args(1))

  val parser = ASTParser.newParser(AST.JLS8)
  
  logger.info(s"Walking ${root}")
  
  walk(root.toFile)
  
  def walk(dir: File) : Unit = {
    dir.listFiles().foreach { 
      case x if x.isDirectory() => walk(x) 
      case x if x.getName.endsWith("java") => convert(x)
      case x => Nil
    }
  }
  
  def convert(file: File) {
    if (skipList.find( e => file.getPath.matches(e) ).nonEmpty) {
      logger.info(s"Skipping ${file.getPath}")
      return
    }
    
    // if (file.getName != "StringUtil") return;
    
    logger.info(s"Processing ${file.getPath}")
    parser.setResolveBindings(true)
    parser.setBindingsRecovery(true)
    parser.setStatementsRecovery(true)
    parser.setEnvironment(null, Array(root.toString), null, true)
    val name = file.getName.split('.')(0)
    val doc = new Document(CharStreams.toString(new FileReader(file)))
    parser.setUnitName(file.getName)
    parser.setSource(doc.get.toCharArray)
    val cu = parser.createAST(null).asInstanceOf[CompilationUnit]
    val program = compilationunit.fromCompilationUnit(cu, root, file.toPath, name)
    if (program == null) return
    val mapper = new ObjectMapper
    mapper.registerModule(DefaultScalaModule)
    mapper.enable(SerializationFeature.INDENT_OUTPUT)
    val fulloutpath = file.getParent.replaceAll(root.toString, outpath.toString)
    new File(fulloutpath).mkdirs()
    val os = Files.newOutputStream(Paths.get(fulloutpath, name + ".ast"), StandardOpenOption.CREATE)
    mapper.writeValue(os, program)
  }
}
