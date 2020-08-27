#!/bin/bash

# remove all files from the data
rm ../Data/interactions/*

# select Binomial, Family, and Terrestrial from PHYLACINE
# and filter for Terrestrial == 1
cut ../Data/PHYLACINE_traits.txt -d , -f 1,3,6  | tr , '\t' | awk '$3 == 1' | tr '\t' , > tmp.csv

cut -d , -f 2 < tmp.csv > families.csv
cut -d , -f 1 < tmp.csv > species.csv
cut -d '_' -f 1 < species.csv > Genus.txt
cut -d '_' -f 2 < species.csv > Species.txt

Binomial=$(paste Genus.txt Species.txt)

while read -r line
do
	Genus=$(echo $line | cut -d ' ' -f 1)
	Species=$(echo $line | cut -d ' ' -f 2)
	./GLOBI_TAXON.sh $Genus $Species
done <<< $Binomial 

rm tmp*
rm *.txt

cat ../Data/interactions/* | uniq > ../Data/interactions/all_mammals.csv
