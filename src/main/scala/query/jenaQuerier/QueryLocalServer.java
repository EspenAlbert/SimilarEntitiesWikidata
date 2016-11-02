package query.jenaQuerier;

import org.apache.jena.query.*;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.engine.binding.Binding;
import org.apache.jena.sparql.resultset.RDFOutput;

import java.io.OutputStream;
import java.util.Iterator;

/**
 * Created by Espen on 10.10.2016.
 */
public class QueryLocalServer {
    public static void main(String[] args) {
        String selectAllQuery =
                "PREFIX  :       <.>\n" +
                        "SELECT (count(*) as ?count)\n" +
                        "{\n" +
                        "    { ?s ?p ?o } UNION { GRAPH ?g { ?s ?p ?o } }\n" +
                        "}";
        //query(selectAllQuery);
        query(findAllStatementsForEntity("Q76"), System.out);
    }
//    private Pattern whereClause = Pattern.compile("")
    public static String getWhereClause(String query) {
        String select = query.substring(query.indexOf("select"));
        return select.substring(select.indexOf("\n"));
    }
    public static String convertToMultipleGraphQuery(String query) {
        String whereClause = getWhereClause(query);
        String unionStatement = "UNION \n" +
                " { GRAPH ?g";
        String defaultGraphQuery = query.replaceFirst("\\{", "\\{\\{");
        return defaultGraphQuery + unionStatement + whereClause + "}}";
    }

    private static String findAllStatementsForEntity(String entityId) {

        String queryString = String.format("prefix w: <http://www.wikidata.org/entity/>\n" +
                "select *\n" +
                "{ {\n" +
                "?s ?p w:%s . } UNION {\n" +
                "w:%s ?p ?o . } \n" +
                "UNION \n" +
                " { GRAPH ?g {{ ?s1 ?p1 w:%s . } \n" +
                "    UNION{\n" +
                "w:%s ?p1 ?o1 . } \n" +
                "    } } \n" +
                "}", entityId, entityId, entityId, entityId);
        return queryString;
    }

    public static void query(String queryString, OutputStream outputStream) {
        Query query = QueryFactory.create(queryString);
        QueryExecution qexec = QueryExecutionFactory.sparqlService("http://localhost:3030/ds/query", queryString);
        try {
            ResultSet results = qexec.execSelect();
            ResultSetFormatter.out(outputStream, results, query);
            Model model = RDFOutput.encodeAsModel(results);
            //model.write(System.out, "N3");
            //model.write(outputStream, "N3");

            while(results.hasNext()) {
                Binding binding = results.nextBinding();
                Iterator<Var> vars = binding.vars();
                while(vars.hasNext()) {
                    Var var = vars.next();
                    System.out.println(var);
                }

            }
        } catch (Exception ex) {
            System.out.println(ex.getMessage());
        } finally {
            qexec.close();
        }
    }
}
