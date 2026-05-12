import subprocess
import xml.etree.ElementTree as ET
import json
import re
import html

FEED_URL = "https://follhim.substack.com/feed"

print(f"[substack] Fetching {FEED_URL} ...")

try:
    result = subprocess.run(
        ["curl", "-v", "-L", "--max-time", "15", FEED_URL],
        capture_output=True, text=True
    )
    print(f"[substack] curl exit code: {result.returncode}")
    print(f"[substack] curl stderr:\n{result.stderr[:800]}")
    print(f"[substack] curl stdout (first 300 chars): {result.stdout[:300]}")

    if result.returncode != 0 or not result.stdout.strip():
        raise RuntimeError(f"curl failed (exit {result.returncode})")

    xml_data = result.stdout.encode("utf-8")
except Exception as e:
    print(f"[substack] ERROR: {e}")
    with open("substack_posts.json", "w") as f:
        json.dump([], f)
    exit(0)

ns = {"media": "http://search.yahoo.com/mrss/"}

print(f"[substack] Parsing XML ({len(xml_data)} bytes)...")
try:
    root = ET.fromstring(xml_data)
    print(f"[substack] XML root tag: {root.tag}")
except ET.ParseError as e:
    print(f"[substack] XML parse error: {e}")
    with open("substack_posts.json", "w") as f:
        json.dump([], f)
    exit(0)

channel = root.find("channel")
if channel is None:
    print("[substack] ERROR: No <channel> element found in feed")
    with open("substack_posts.json", "w") as f:
        json.dump([], f)
    exit(0)

print(f"[substack] Found channel, scanning items...")

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

print(f"[substack] Done — wrote {len(posts)} posts to substack_posts.json")
