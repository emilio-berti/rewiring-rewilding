#!/bin/bash

# cd ../Data/interactions

curl -s -n --output $1_$2.txt https://api.globalbioticinteractions.org/taxon/$1%20$2/preysOn?includeObservations=true

cat $1_$2.txt | tail -n 2 | tr [ '\n' | cut -d , -f 1,3 | tr -d } | sed 1,/^$data/d | sed /^$/d | sort | tr -d ']' | uniq >> ../Data/interactions/$1_$2.csv

rm $1_$2.txt

echo 'GLOBI_TAXON completed for' $1 $2
