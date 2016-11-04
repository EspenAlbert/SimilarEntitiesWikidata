package rdf

import java.io.PrintWriter

/**
  * Created by Espen on 04.11.2016.
  */
object CreateRdfFile {
  def createRDFFile(triples: List[SimpleRDF], filename : String) = {
    val writer = new PrintWriter("output/" + filename + ".nt")
    for(statement <- triples) {
      writer.write(statement.getStatementNt() + ".\n")
    }
    writer.close()
  }
}
