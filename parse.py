import json
from html.parser import HTMLParser
from pprint import pprint


class MyHTMLParser(HTMLParser):
    poses = []
    current = {}

    def handle_starttag(self, tag, attrs):
        if tag == "tr":
            self.current = {}
        elif tag == "td":
            if "benefits" not in self.current:
                self.current["benefits"] = []
        elif tag == "a":
            if "image" not in self.current:
                self.current["image"] = attrs[0][1]

    def handle_endtag(self, tag):
        if tag == "tr":
            self.poses.append(self.current)

    def handle_data(self, data):
        data = data.strip()
        if not data:
            return
        if len(self.current["benefits"]) == 0 and "pose" not in self.current:
            self.current["pose"] = data
        elif (
            len(self.current["benefits"]) == 0
            and "pose" in self.current
            and "asana" not in self.current
            and not data.startswith("Level:")
        ):
            self.current["asana"] = data[1:-1]
        elif data.startswith("Level"):
            self.current["level"] = data[7:]
        elif data and "benefits" in self.current:
            self.current["benefits"].append(data)


parser = MyHTMLParser()
with open("yoga.html", "r") as f:
    parser.feed(f.read())

with open("poses.json", "w") as j:
    j.write(json.dumps({"poses": parser.poses}))

pprint(parser.poses)
