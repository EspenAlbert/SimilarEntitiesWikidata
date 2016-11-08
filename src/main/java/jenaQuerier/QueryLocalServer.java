package jenaQuerier;

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

    public static boolean ask(String queryString, String dataset) throws Exception {
        QueryExecution qexec = QueryExecutionFactory.sparqlService("http://localhost:3030/" + dataset +"/query", queryString);
        try {
            return qexec.execAsk();
        } catch (Exception ex) {
            System.out.println(ex.getMessage());
        } finally {
            qexec.close();
        }
        throw new Exception("failed to ask:" + queryString);
    }
    public static void query(String queryString, OutputStream outputStream) {
        query(queryString, outputStream, "ds");
    }
    public static void query(String queryString, OutputStream outputStream, String dataset ) {
        Query query = QueryFactory.create(queryString);
        QueryExecution qexec = QueryExecutionFactory.sparqlService("http://localhost:3030/" + dataset +"/query", queryString);
        try {
            ResultSet results = qexec.execSelect();

            ResultSetFormatter.out(outputStream, results, query);
        } catch (Exception ex) {
            System.out.println(ex.getMessage());
        } finally {
            qexec.close();
        }
    }
}
