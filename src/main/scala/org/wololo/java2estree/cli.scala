package org.wololo.java2estree

import com.github.javaparser.JavaParser
import java.io.StringReader
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.core.JsonGenerator.Feature
import com.fasterxml.jackson.databind.SerializationFeature

object cli extends App {
  val java = """
    package test;
    
    class Test {
      void add(int x, int y) {
        return x + y;
      }
    }
  """
  
  val sr = new StringReader(java)
  val cu = JavaParser.parse(sr, true)
  val program = Converters.asProgram(cu)
  val mapper = new ObjectMapper
  mapper.registerModule(DefaultScalaModule)
  //mapper.enable(SerializationFeature.INDENT_OUTPUT)
  val s = mapper.writeValueAsString(program)
  
  println(s)
}