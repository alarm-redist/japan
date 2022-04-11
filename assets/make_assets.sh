#!/usr/bin/env bash
echo "Converting..."
rsvg-convert -h 1200 alarm.svg          > alarm_1200_tr.png
rsvg-convert -h 1200 alarm.svg -b white > alarm_1200_wh.png
rsvg-convert -h 256 alarm.svg          > alarm_256_tr.png
rsvg-convert -h 256 alarm.svg -b white > alarm_256_wh.png
rsvg-convert -h 64 alarm.svg          > alarm_64_tr.png
rsvg-convert -h 64 alarm.svg -b white > alarm_64_wh.png
rsvg-convert -h 64 favicon.svg > favicon.png
rsvg-convert -h 1200 alarm_crop.svg -b white > alarm_1200_crop.png

convert -background none -gravity center alarm_1200_tr.png -resize 1200x1200 -extent 1200x1200 alarm_1200_tr.png
convert -background none -gravity center alarm_256_tr.png -resize 256x256 -extent 256x256 alarm_256_tr.png
convert -background none -gravity center alarm_64_tr.png -resize 64x64 -extent 64x64 alarm_64_tr.png
convert -background white -gravity center alarm_1200_wh.png -resize 1200x1200 -extent 1200x1200 alarm_1200_wh.png
convert -background white -gravity center alarm_256_wh.png -resize 256x256 -extent 256x256 alarm_256_wh.png
convert -background white -gravity center alarm_64_wh.png -resize 64x64 -extent 64x64 alarm_64_wh.png
convert -background none -gravity center favicon.png -resize 64x64 -extent 64x64 favicon.png
convert -background white -gravity center alarm_1200_crop.png -resize 1200x1200 -extent 1200x1200 alarm_1200_crop.png

rsvg-convert -h 1200 fifty_states.svg          > fifty_states_1200_tr.png
rsvg-convert -h 1200 fifty_states.svg -b white > fifty_states_1200_wh.png
rsvg-convert -h 256 fifty_states.svg          > fifty_states_256_tr.png
rsvg-convert -h 256 fifty_states.svg -b white > fifty_states_256_wh.png
convert -background none -gravity center fifty_states_1200_tr.png -resize 1200x1200 -extent 1200x1200 fifty_states_1200_tr.png
convert -background none -gravity center fifty_states_256_tr.png -resize 256x256 -extent 256x256 fifty_states_256_tr.png
convert -background white -gravity center fifty_states_1200_wh.png -resize 1200x1200 -extent 1200x1200 fifty_states_1200_wh.png
convert -background white -gravity center fifty_states_256_wh.png -resize 256x256 -extent 256x256 fifty_states_256_wh.png
echo "Done."
