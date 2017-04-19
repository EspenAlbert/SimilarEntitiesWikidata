package jenaQuerier;

import org.apache.jena.query.*;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.reasoner.rulesys.OWLFBRuleReasoner;
import org.apache.jena.reasoner.rulesys.OWLFBRuleReasonerFactory;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.engine.binding.Binding;
import org.apache.jena.sparql.resultset.RDFOutput;
import org.apache.jena.update.UpdateExecutionFactory;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateProcessor;
import org.apache.jena.update.UpdateRequest;

import java.io.OutputStream;
import java.util.Iterator;

/**
 * Created by Espen on 10.10.2016.
 */
public class QueryLocalServer {

    public static void main(String[] args) {
        testReasonerCapabilities();
    }
    public static String convertToMutlipleGraphQueryWithoutSelect(String query) {
        String whereClause = query.substring(query.indexOf("{"));
        return createMultipleGraphQuery(query, whereClause);
    }
    public static String getWhereClause(String query) {
        String select = query.substring(query.indexOf("select"));
        return select.substring(select.indexOf("\n"));
    }
    public static String convertToMultipleGraphQuery(String query) {
        String whereClause = getWhereClause(query);
        return createMultipleGraphQuery(query, whereClause);
    }

    private static String createMultipleGraphQuery(String query, String whereClause) {
        String unionStatement = "UNION \n" +
                " { GRAPH ?g";
        String defaultGraphQuery = query.replaceFirst("\\{", "\\{\\{");
        return defaultGraphQuery + unionStatement + whereClause + "}}";
    }

    public static void testReasonerCapabilities() {
        Model capabilities = OWLFBRuleReasonerFactory.theInstance().getCapabilities();
//        ResultSetFormatter.out(System.out, capabilities
        int a = 2;
    }
    public static boolean ask(String queryString, String dataset) throws Exception {
        QueryExecution qexec = QueryExecutionFactory.sparqlService("http://localhost:3030/" + dataset +"/query", queryString);
        try {
            return qexec.execAsk();
        } catch (Exception ex) {
            System.out.println(ex.getMessage());
            System.out.println("failed to ask:" + queryString);
            return false;
        } finally {
            qexec.close();
        }
    }
    public static void query(String queryString, OutputStream outputStream) {
        query(queryString, outputStream, "ds");
    }
    public static void query(String queryString, OutputStream outputStream, String dataset ) {
//        System.out.println("about to execute query");
        Query query = QueryFactory.create(queryString);
        QueryExecution qexec = QueryExecutionFactory.sparqlService("http://localhost:3030/" + dataset +"/query", queryString);
        try {
            ResultSet results = qexec.execSelect();
            ResultSetFormatter.out(outputStream, results, query);
        } finally {
            qexec.close();

        }
    }
    public static void updateLocalData(String query, String dataset) {
        UpdateRequest update = UpdateFactory.create(query);
        UpdateProcessor remote = UpdateExecutionFactory.createRemote(update, "http://localhost:3030/" + dataset + "/update");
        try {
            remote.execute();
        } catch(Exception e) {
            System.out.println(e);
        }
    }
    public static void deleteLocalData() {
        deleteLocalData("valueMatch");
    }
    public static void deleteLocalData(String dataset) {
        UpdateRequest update = UpdateFactory.create("delete { ?s ?p ?o } where { ?s ?p ?o}");
        UpdateProcessor remote = UpdateExecutionFactory.createRemote(update, "http://localhost:3030/" + dataset + "/update");
        try {
            remote.execute();
//            System.out.println(remote.getDatasetGraph());
        } catch(Exception e) {
//            System.out.println(e);
        }
    }
    public static void deleteLocalData(String dataset, String where) {
        UpdateRequest update = UpdateFactory.create("delete { "+ where + " } where { " + where + "}");
        UpdateProcessor remote = UpdateExecutionFactory.createRemote(update, "http://localhost:3030/" + dataset + "/update");
        try {
            remote.execute();
//            System.out.println(remote.getDatasetGraph());
        } catch(Exception e) {
            System.out.println(e);
        }
    }
    public static void queryOnlineWikidata(String queryString, OutputStream outputStream ) {
        Query query = QueryFactory.create(queryString);
        QueryExecution qexec = QueryExecutionFactory.sparqlService("https://query.wikidata.org/sparql", queryString);
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
