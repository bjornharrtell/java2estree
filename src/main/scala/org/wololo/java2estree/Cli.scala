package org.wololo.java2estree

import java.io.File
import java.io.FileReader
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.dom.AST
import org.eclipse.jdt.core.dom.ASTParser
import org.eclipse.jdt.core.dom.CompilationUnit
import org.eclipse.jface.text.Document
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.SerializationFeature
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.typesafe.scalalogging.LazyLogging
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import scala.concurrent.Channel
import java.nio.channels.Channels

object Cli extends App with LazyLogging {
  
  val skipList = List(
    "/jts/awt/",
    "/jts/io/",
    "/jts/geom/impl/PackedCoordinateSequence.java",
	  "/jts/util/Debug.java",
	  "/jts/util/Stopwatch.java",
	  "/jts/geom/Geometry.java",
	  "/jts/index/quadtree/DoubleBits.java",
	  "/jts/geom/TopologyException.java",
	  "/jts/precision/CommonBits.java"
  ).map(e => ".*" + e + ".*")
  
  val root = Paths.get(args(0))
  val outpath = Paths.get(args(1))

  val parser = ASTParser.newParser(AST.JLS11)
  
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
    //if (!file.getPath.matches(".*/jts/index/intervalrtree/SortedPackedIntervalRTree.java.*")) return
    //if (!file.getPath.matches(".*PrecisionReducerCoordinateOperation.*")) return
    //if (!file.getPath.matches(".*GeometryEditor.*")) return
    //if (!file.getPath.matches(".*CGAlgorithms.java*")) return
    //if (!file.getPath.matches(".*InputExtracter.java*")) return
    //if (!file.getPath.matches(".*NumberUtil.java*")) return
    
    if (skipList.find( e => file.getPath.matches(e) ).nonEmpty) {
      logger.info(s"Skipping ${file.getPath}")
      return
    }
    
    logger.info(s"Processing ${file.getPath}")
    val options = JavaCore.getOptions()
    options.put(JavaCore.COMPILER_SOURCE, "11")
    parser.setCompilerOptions(options)
    parser.setResolveBindings(true)
    parser.setBindingsRecovery(true)
    parser.setStatementsRecovery(true)
    parser.setEnvironment(null, Array(root.toString), null, true)
    val name = file.getName.split('.')(0)
    parser.setUnitName(file.getName)
    val bytes = Files.readAllBytes(file.toPath()).map(b => b.toChar)
    parser.setSource(bytes)
    val cu = parser.createAST(null).asInstanceOf[CompilationUnit]
    val program = compilationunit.fromCompilationUnit(cu, root, file.toPath, name)
    if (program == null) {
      logger.info(s"Turned up empty for ${file.getPath}")
      return
    }
    val mapper = new ObjectMapper
    mapper.registerModule(DefaultScalaModule)
    mapper.enable(SerializationFeature.INDENT_OUTPUT)
    val fulloutpath = file.getParent.replaceAll(root.toString, outpath.toString)
    new File(fulloutpath).mkdirs()
    val os = Files.newOutputStream(Paths.get(fulloutpath, name + ".ast"), StandardOpenOption.CREATE)
    mapper.writeValue(os, program)
  }
}
