import java.io.InputStreamReader
import java.io.StringReader

import scala.collection.JavaConverters.seqAsJavaListConverter

import org.wololo.java2estree.Converters

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.github.javaparser.JavaParser
import com.google.common.io.CharStreams

object Utils {
  import org.wololo.java2estree.Converters
  
  def java2js(java: String) : String = {
    val cu = JavaParser.parse(new StringReader(java), true)
    val program = Converters.asProgram(cu)
    val mapper = new ObjectMapper
    mapper.registerModule(DefaultScalaModule)
    val pb = new ProcessBuilder(List("./node_modules/astring/bin/astring", "--indent", "  ", "--startingIndentLevel", "2").asJava)
    val p = pb.start()
    mapper.writeValue(p.getOutputStream, program)
    "\n" + CharStreams.toString(new InputStreamReader(p.getInputStream, "UTF-8"))
  }
}