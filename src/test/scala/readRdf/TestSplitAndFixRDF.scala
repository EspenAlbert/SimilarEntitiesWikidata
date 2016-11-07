package readRdf

import org.scalatest.FunSuite

/**
  * Created by Espen on 07.11.2016.
  */
class TestSplitAndFixRDF extends FunSuite {


  test("Various tests of the fixer") {
    val error1 = "<http://www.wikidata.org/entity/Q8479> <http://www.wikidata.org/entity/P1543> <http://commons.wikimedia.org/wiki/File:Imperial_Monogram_of_Tsar_Peter_I_\\\"The_Great\\\"_of_Russia,_Variant_2.svg> ."
    val expected1 = SplitAndFixRDF.NON_FIXABLE_URI
    val error2 = "<http://www.wikidata.org/entity/Q568312> <http://www.wikidata.org/entity/P18c> <http://commons.wikimedia.org/wiki/File:4^Kharkov_1943.jpg> ."
    val expected2 = "<http://www.wikidata.org/entity/Q568312> <http://www.wikidata.org/entity/P18> <http://commons.wikimedia.org/wiki/File:4%5EKharkov_1943.jpg> ."
    val error3 = "<http://www.wikidata.org/entity/Q588632> <http://www.wikidata.org/entity/P856c> <http://site.dimmu-borgir.com|titre=Dimmu> ."
    val expected3 = SplitAndFixRDF.NON_FIXABLE_URI
    val error4 = "<http://www.wikidata.org/entity/Q601080> <http://www.wikidata.org/entity/P18c> <http://commons.wikimedia.org/wiki/File:Scotland's_Shortest_Motorway^_(Part_1)_-_geograph.org.uk_-_1756356.jpg> ."
    val expected4 = "<http://www.wikidata.org/entity/Q601080> <http://www.wikidata.org/entity/P18> <http://commons.wikimedia.org/wiki/File:Scotland's_Shortest_Motorway%5E_(Part_1)_-_geograph.org.uk_-_1756356.jpg> ."
    val error5 = "<http://www.wikidata.org/entity/Q638445> <http://www.wikidata.org/entity/P856c> <http://www.chersonesos.org/}> ."
    val expected5= "<http://www.wikidata.org/entity/Q638445> <http://www.wikidata.org/entity/P856> <http://www.chersonesos.org/> ."
    val error6 = "<http://www.wikidata.org/entity/Q970519> <http://www.wikidata.org/entity/P18c> <http://commons.wikimedia.org/wiki/File:L`administration_de_la_Daira_de_Batna.jpg> ."
    val expected6 = "<http://www.wikidata.org/entity/Q970519> <http://www.wikidata.org/entity/P18> <http://commons.wikimedia.org/wiki/File:L%60administration_de_la_Daira_de_Batna.jpg> ."
    val error7 = "<http://www.wikidata.org/entity/Q1137248> <http://www.wikidata.org/entity/P856c> <http://www.rcd.tn/}}{{> ."
    val expected7 ="<http://www.wikidata.org/entity/Q1137248> <http://www.wikidata.org/entity/P856> <http://www.rcd.tn/> ."
    val error8 = "<http://www.wikidata.org/entity/Q18923687> <http://www.wikidata.org/entity/P973c> <http://cir.campania.beniculturali.it/museodicapodimonte/thematic-views/image-gallery/OA900331?set_language=en\\n> ."
    val expected8 = "<http://www.wikidata.org/entity/Q18923687> <http://www.wikidata.org/entity/P973> <http://cir.campania.beniculturali.it/museodicapodimonte/thematic-views/image-gallery/OA900331?set_language=en> ."
    val error9 = "<http://www.wikidata.org/entity/Q202324> <http://www.wikidata.org/entity/P856c> <http://www.fibaeurope.com/default.asp?cid={5179A822-D4B1-4476-AD04-EDBC445D6DC6}&compID={0D93D753-CAD5-4604-A251-1402A6361BF3}> ."
    val expected9 = "<http://www.wikidata.org/entity/Q202324> <http://www.wikidata.org/entity/P856> <http://www.fibaeurope.com/default.asp?cid=5179A822-D4B1-4476-AD04-EDBC445D6DC6&compID=0D93D753-CAD5-4604-A251-1402A6361BF3> ."
    val error10 = "<http://www.wikidata.org/entity/Q1040991> <http://www.wikidata.org/entity/P856c> <http://www.box-corporation.com/\\\\?portfolios=%E7%9B%B8%E6%AD%A6%E7%B4%97%E5%AD%A3> ."
    val expected10= "<http://www.wikidata.org/entity/Q1040991> <http://www.wikidata.org/entity/P856> <http://www.box-corporation.com/?portfolios=%E7%9B%B8%E6%AD%A6%E7%B4%97%E5%AD%A3> ."
    val error11 = "<http://www.wikidata.org/entity/Q2120110> <http://www.wikidata.org/entity/P646-freebase> <http://rdf.freebase.com/ns/HARDAH MUNICIPALITY HOSPITAL> ."
    val expected11 = "<http://www.wikidata.org/entity/Q2120110> <http://www.wikidata.org/entity/P646> <http://rdf.freebase.com/ns/HARDAH%20MUNICIPALITY%20HOSPITAL> ."
    val error12 = "<http://www.wikidata.org/entity/Q4537983> <http://www.wikidata.org/entity/P1687c> <http://www.wikidata.org/entity/P1553>"
    val expected12 = "<http://www.wikidata.org/entity/Q4537983> <http://www.wikidata.org/entity/P1687> <http://www.wikidata.org/entity/P1553>"
    var errors = List(error1, error2, error3, error4, error5, error6, error7, error8, error9, error10, error11, error12)
    var expecteds = List(expected1, expected2, expected3, expected4, expected5, expected6, expected7, expected8, expected9, expected10, expected11, expected12)

    for((error, expected) <- errors.zip(expecteds)) {
      val fixedLine: String = SplitAndFixRDF.cleanUpLine(error)
      println(fixedLine)
      println(expected)
      assert(fixedLine == expected )
    }

  }
}
