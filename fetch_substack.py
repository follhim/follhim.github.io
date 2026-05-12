import urllib.request
import xml.etree.ElementTree as ET
import json
import re
import html
import ssl

FEED_URL = "https://follhim.substack.com/feed"

# Mac Python doesn't bundle system SSL certs — bypass verification for this public feed
ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

try:
    req = urllib.request.Request(FEED_URL, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=15, context=ctx) as response:
        xml_data = response.read()
except Exception as e:
    print(f"Warning: Could not fetch Substack feed: {e}")
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

def strip_html(raw):
    """Strip HTML tags and decode entities, collapse whitespace."""
    text = re.sub(r'<[^>]+>', ' ', raw)
    text = html.unescape(text)
    text = re.sub(r'\s+', ' ', text).strip()
    return text

def truncate(text, max_chars=180):
    if len(text) <= max_chars:
        return text
    truncated = text[:max_chars].rsplit(' ', 1)[0]
    return truncated + '…'

posts = []
for item in channel.findall("item"):
    title    = (item.findtext("title") or "").strip()
    link     = (item.findtext("link") or "").strip()
    pub_date = (item.findtext("pubDate") or "").strip()
    desc     = item.findtext("description") or ""

    # Thumbnail: media:content → enclosure → first <img> in description
    thumbnail = None
    media_content = item.find("media:content", ns)
    if media_content is not None:
        thumbnail = media_content.get("url")
    if not thumbnail:
        enclosure = item.find("enclosure")
        if enclosure is not None:
            thumbnail = enclosure.get("url")
    if not thumbnail and desc:
        m = re.search(r'<img[^>]+src=["\']([^"\']+)["\']', desc)
        if m:
            thumbnail = m.group(1)

    # Excerpt: strip HTML from description, remove the thumbnail img line
    excerpt = ""
    if desc:
        # Remove img tags before stripping so they don't leave blank gaps
        clean_desc = re.sub(r'<img[^>]+>', '', desc)
        plain = strip_html(clean_desc)
        excerpt = truncate(plain)

    if title and link:
        posts.append({
            "title":     title,
            "link":      link,
            "pubDate":   pub_date,
            "thumbnail": thumbnail,
            "excerpt":   excerpt,
        })

with open("substack_posts.json", "w") as f:
    json.dump(posts, f, indent=2, ensure_ascii=False)

print(f"Fetched {len(posts)} Substack posts → substack_posts.json")
