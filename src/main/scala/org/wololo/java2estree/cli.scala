package org.wololo.java2estree

import com.github.javaparser.JavaParser
import java.io.StringReader
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.core.JsonGenerator.Feature
import com.fasterxml.jackson.databind.SerializationFeature
import java.io.File
import com.typesafe.scalalogging.LazyLogging

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
  
  mapper.writeValue(System.out, program)
  //mapper.writeValue(output, program)
}