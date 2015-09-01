import java.io.InputStreamReader
import java.io.StringReader

import scala.collection.JavaConverters.seqAsJavaListConverter

import org.wololo.java2estree.Converters

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.google.common.io.CharStreams
import org.eclipse.jface.text.Document
import org.eclipse.jdt.core.dom.ASTParser
import org.eclipse.jdt.core.dom.AST
import org.eclipse.jdt.core.dom.CompilationUnit

object Utils {
  import org.wololo.java2estree.Converters
  
  def java2js(java: String) : String = {
    val doc = new Document(java);
    val parser = ASTParser.newParser(AST.JLS8)
    parser.setResolveBindings(true)
    parser.setBindingsRecovery(true)
    parser.setStatementsRecovery(true)
    parser.setEnvironment(null, null, null, true)
    parser.setUnitName("Test.java")
    parser.setSource(doc.get().toCharArray())
    val cu = parser.createAST(null).asInstanceOf[CompilationUnit]
    val program = Converters.toProgram(cu)
    val mapper = new ObjectMapper
    mapper.registerModule(DefaultScalaModule)
    val pb = new ProcessBuilder(List("./node_modules/astring/bin/astring", "--indent", "  ", "--startingIndentLevel", "2").asJava)
    val p = pb.start()
    mapper.writeValue(p.getOutputStream, program)
    "\n" + CharStreams.toString(new InputStreamReader(p.getInputStream, "UTF-8"))
  }
}