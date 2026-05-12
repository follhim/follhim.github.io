import urllib.request
import xml.etree.ElementTree as ET
import json
import re

FEED_URL = "https://follhim.substack.com/feed"

try:
    req = urllib.request.Request(FEED_URL, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=15) as response:
        xml_data = response.read()
except Exception as e:
    print(f"Warning: Could not fetch Substack feed: {e}")
    # Write empty array so the page still loads cleanly
    with open("substack_posts.json", "w") as f:
        json.dump([], f)
    exit(0)

ns = {"media": "http://search.yahoo.com/mrss/"}

try:
    root = ET.fromstring(xml_data)
except ET.ParseError as e:
    print(f"Warning: Could not parse RSS feed: {e}")
    with open("substack_posts.json", "w") as f:
        json.dump([], f)
    exit(0)

channel = root.find("channel")
if channel is None:
    with open("substack_posts.json", "w") as f:
        json.dump([], f)
    exit(0)

posts = []
for item in channel.findall("item"):
    title = item.findtext("title", "").strip()
    link = item.findtext("link", "").strip()
    pub_date = item.findtext("pubDate", "").strip()
    description = item.findtext("description", "")

    # Try media:content for thumbnail
    thumbnail = None
    media_content = item.find("media:content", ns)
    if media_content is not None:
        thumbnail = media_content.get("url")

    # Try enclosure
    if not thumbnail:
        enclosure = item.find("enclosure")
        if enclosure is not None:
            thumbnail = enclosure.get("url")

    # Parse first <img> from description HTML
    if not thumbnail and description:
        match = re.search(r'<img[^>]+src=["\']([^"\']+)["\']', description)
        if match:
            thumbnail = match.group(1)

    if title and link:
        posts.append({
            "title": title,
            "link": link,
            "pubDate": pub_date,
            "thumbnail": thumbnail
        })

with open("substack_posts.json", "w") as f:
    json.dump(posts, f, indent=2)

print(f"Fetched {len(posts)} Substack posts → substack_posts.json")
