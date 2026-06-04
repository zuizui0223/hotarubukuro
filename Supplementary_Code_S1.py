{\rtf1\ansi\ansicpg932\cocoartf2820
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 import xml.etree.ElementTree as ET\
from datetime import datetime\
import pytz\
\
# ==============================\
# Load the GPX file\
# ==============================\
gpx_file_path = "/content/yamap_2023-08-20_12_06.gpx"  # \uc0\u12501 \u12449 \u12452 \u12523 \u12497 \u12473 \
tree = ET.parse(gpx_file_path)\
root = tree.getroot()\
\
# ==============================\
# Define GPX namespace\
# ==============================\
ns = \{'default': 'http://www.topografix.com/GPX/1/1'\}\
\
# ==============================\
# Extract all track points with time, latitude, longitude, elevation\
# ==============================\
track_points = []\
\
for trkpt in root.findall('.//default:trkpt', ns):\
    lat = float(trkpt.get('lat'))\
    lon = float(trkpt.get('lon'))\
    \
    ele_elem = trkpt.find('default:ele', ns)\
    time_elem = trkpt.find('default:time', ns)\
    \
    if ele_elem is not None and time_elem is not None:\
        ele = float(ele_elem.text)\
        \
        # Parse time and convert to datetime object (UTC)\
        time = datetime.fromisoformat(time_elem.text.replace("Z", "+00:00"))\
        \
        track_points.append(\{\
            'time': time,\
            'lat': lat,\
            'lon': lon,\
            'ele': ele\
        \})\
\
# ==============================\
# Convert to JST (UTC+9)\
# ==============================\
jst = pytz.timezone('Asia/Tokyo')\
\
for point in track_points:\
    point['time_jst'] = point['time'].astimezone(jst)\
\
# ==============================\
# Find the point closest to 2023-08-20 15:27 JST\
# ==============================\
\
# Target time in JST\
target_time_str = "2023-08-20 15:27"\
target_time = datetime.strptime(target_time_str, "%Y-%m-%d %H:%M")\
\
# Localize target time as JST\
target_time_jst = jst.localize(target_time)\
\
# Find closest point\
closest_point = min(\
    track_points,\
    key=lambda p: abs(p['time_jst'] - target_time_jst)\
)\
\
# ==============================\
# Output result\
# ==============================\
print("Closest point to target time:")\
print(f"Time (JST): \{closest_point['time_jst']\}")\
print(f"Latitude: \{closest_point['lat']\}")\
print(f"Longitude: \{closest_point['lon']\}")\
print(f"Elevation: \{closest_point['ele']\} m")}