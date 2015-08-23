package org.wololo.java2estree

import java.io.File
import java.io.InputStreamReader

import scala.collection.JavaConverters.seqAsJavaListConverter

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.SerializationFeature
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.github.javaparser.JavaParser
import com.google.common.io.CharStreams

object cli extends App {  
  var filename = args(0)
  
  var input = new File(filename)
  //val base = filename.split("\\.")(0)
  //val output = new File(base + ".ast")
  
  val cu = JavaParser.parse(input)
  val program = Converters.asProgram(cu)
  val mapper = new ObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.enable(SerializationFeature.INDENT_OUTPUT)
  
  val pb = new ProcessBuilder(List("./node_modules/astring/bin/astring", "--indent", "  ").asJava)
  val p = pb.start()
  
  mapper.writeValue(p.getOutputStream, program)
  
  println(CharStreams.toString(new InputStreamReader(p.getInputStream, "UTF-8")))
}