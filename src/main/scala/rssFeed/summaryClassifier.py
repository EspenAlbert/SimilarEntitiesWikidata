import os

f = []
for (dirpath, dirnames, filenames) in os.walk("/home/espen/prog/scala/SimilarEntitiesWikidata/input/news/bbc"):
    for name in filenames:
        f.append(dirpath + "/" + name)


print([x.split("/")[-1].replace("_", " ")[:-4] for x in f])
import http.client

conn = http.client.HTTPConnection("entityclassifier.eu")
headers = {
    'cache-control': "no-cache",
    'postman-token': "6766955e-f670-013e-ea25-e0bc774a1515"
}
jsonBaseDir = "/home/espen/prog/scala/SimilarEntitiesWikidata/input/news/bbcJson/"

def createClassification(summary, path):
    filename = path.split("/")[-1]
    payload = "\"%s\""%summary
    jsonFilePath = jsonBaseDir + filename
    conn.request("POST", "/thd/api/v2/extraction?format=json&apikey=180430d9ca594be2bf4b82bba97d67e8", payload, headers)
    res = conn.getresponse()
    data = res.read()
    jsonResponse = data.decode("utf-8")
    f = open(jsonFilePath, 'w', encoding="utf-8")
    f.write(jsonResponse)
    f.close()

# for articleFilenamePath in f:
#     file = open(articleFilenamePath, 'r', encoding="utf-8")
#     summary = file.readlines()[3:]#First 3 lines: title, published date and link.
#     oneLineSummary = " ".join(summary)
#     createClassification(oneLineSummary, articleFilenamePath)