package tdbModel;

import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.tdb.TDBFactory;
import org.apache.jena.util.FileManager;


/**
 * Created by Espen on 12.10.2016.
 */
public class CreateWikidataTDBModel {

    public static void main(String[] args) {
        store();
    }
    public static void store(){
        String directory = "C:/dataset/tdbWikidata3";
        Dataset dataset = openTDB(directory);
        int filesPerDataSet = 400;
        int graphNumber = 1;
        String graphName = "http://www.espenalbert.com/rdf/wikidata/localGraph";
        for (int fileNumber = 1; fileNumber < 1389; fileNumber++) {
            if(fileNumber % filesPerDataSet == 0) graphNumber++;
            String source = String.format("C:/dataset/smallFiles2/wikidata-simple-statements%s.nt", fileNumber);
            Model tdb = loadModel(source, dataset);
            dataset.addNamedModel(String.format(graphName +"%d", graphNumber), tdb);
            tdb.close();
            System.out.println("Finished file #: " + fileNumber);
        }
        dataset.close();
    }


    public static Dataset openTDB(String directory){
        // open TDB dataset
        Dataset dataset = TDBFactory.createDataset(directory);
        return dataset;
    }

    public static Model loadModel(String source, Dataset dataset){

        Model tdb = ModelFactory.createDefaultModel();
        FileManager.get().readModel( tdb, source, "NT" );
        return tdb;
    }
}
