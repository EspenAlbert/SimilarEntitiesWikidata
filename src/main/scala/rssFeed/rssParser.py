import feedparser

bbc = feedparser.parse("http://feeds.bbci.co.uk/news/world/rss.xml")
print(bbc['feed']['title'])
baseDirectory = "/home/espen/prog/scala/SimilarEntitiesWikidata/input/news/bbc/"
for e in bbc.entries:
    summary = e.summary
    title = e.title
    titleFilename = e.title.replace("\n", " ").replace(" ", "_") + ".txt"
    link = e.link
    published = e.published
    f = open(baseDirectory + titleFilename, 'w', encoding="utf-8")
    lines = [title, link, published, summary]
    f.writelines("\n".join(lines))
    f.close()

# import http.client
#
# conn = http.client.HTTPConnection("entityclassifier.eu")
#
# payload = "\"The Charles Bridge is a famous historic bridge that crosses the Vltava river in Prague, Czech Republic.\""
#
# headers = {
#     'cache-control': "no-cache",
#     'postman-token': "6766955e-f670-013e-ea25-e0bc774a1515"
# }
#
# conn.request("POST", "/thd/api/v2/extraction?format=json&apikey=180430d9ca594be2bf4b82bba97d67e8", payload, headers)
#
# res = conn.getresponse()
# data = res.read()
#
# jsonResponse = data.decode("utf-8")
# filename = "/home/espen/prog/scala/SimilarEntitiesWikidata/input/news/test.txt"
# f = open(filename, 'w', encoding="utf-8").write(jsonResponse)
# print(jsonResponse)