import java.io.InputStreamReader
import java.io.StringReader
import scala.jdk.CollectionConverters._
import org.wololo.java2estree.compilationunit
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.apache.commons.io.IOUtils
import org.eclipse.jface.text.Document
import org.eclipse.jdt.core.dom.ASTParser
import org.eclipse.jdt.core.dom.AST
import org.eclipse.jdt.core.dom.CompilationUnit
import java.nio.file.Paths
import org.eclipse.jdt.core.JavaCore
import java.io.OutputStream

object Utils {
  import org.wololo.java2estree.compilationunit
  
  def java2js(java: String) : String = {
    val doc = new Document(java)
    val parser = ASTParser.newParser(AST.getJLSLatest())
    val options = JavaCore.getOptions()
    options.put(JavaCore.COMPILER_SOURCE, "17")
    parser.setCompilerOptions(options)
    parser.setResolveBindings(true)
    parser.setBindingsRecovery(true)
    parser.setStatementsRecovery(true)
    parser.setEnvironment(null, null, null, true)
    parser.setUnitName("Test.java")
    parser.setSource(java.toCharArray())
    val cu = parser.createAST(null).asInstanceOf[CompilationUnit]
    val program = compilationunit.fromCompilationUnit(cu, Paths.get(""), Paths.get("Test.java"), "Test")
    val mapper = new ObjectMapper
    mapper.registerModule(DefaultScalaModule)
    val pb = new ProcessBuilder(List("./node_modules/astring/bin/astring", "--indent", "  ", "--starting-indent-level", "2").asJava)
    val p = pb.start()
    mapper.writeValue(p.getOutputStream, program)
    var output = IOUtils.toString(p.getInputStream, "UTF-8")
    "\n" + output
  }
}