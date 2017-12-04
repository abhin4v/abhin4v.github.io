#!/bin/sh

set -x #echo on
mkdir -p files
curl -X GET "https://www.strava.com/api/v3/athlete/activities?per_page=200&page=1" -H "Authorization: Bearer $STRAVA_KEY" -o 1.json
curl -X GET "https://www.strava.com/api/v3/athlete/activities?per_page=200&page=2" -H "Authorization: Bearer $STRAVA_KEY" -o 2.json
cat 1.json 2.json | jq '.[] | {type, duration: .moving_time, distance, date: .start_date}' | jq -c -s '.' | sed 's/type":"Walk"/type":"Run"/g' > files/activities.json
rm 1.json 2.json
git add files/activities.json
