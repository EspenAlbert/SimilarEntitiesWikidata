name := "SimilarEntitiesWikidata"

version := "1.0"

scalaVersion := "2.11.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" %  "test"
libraryDependencies += "org.w3" %% "banana-jena" % "0.8.1"
libraryDependencies += "org.apache.jena" % "apache-jena-libs" % "3.1.0"
libraryDependencies +=  "com.lambdaworks" %% "jacks" % "2.3.3"
libraryDependencies +=  "org.scalanlp" %% "breeze" % "0.12"
libraryDependencies +=  "org.scalanlp" %% "breeze-natives" % "0.12"
libraryDependencies +=  "com.typesafe.akka" %% "akka-stream-experimental" % "2.0.1"
libraryDependencies += "org.apache.commons" % "commons-compress" % "1.5"