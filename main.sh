#!/bin/bash
min=("0" "1" "2" "5");
sel=("0" "1");
per=("1" "0.5" "0.1" "0.05");
nei=("0" "1");


count=0;

for m in {0..3}; do
for s in {0..1}; do
for p in {0..3}; do
for n in {0..1}; do

#string="$m $s $p $n"
echo $m $s $p $n;
echo "$((min[$m])), $((sel[$s])), $((per[$p])), $((nei[$n]))"
#exec ./script.R $string > "out_$count.txt"
#./script.R $m $s $p $n > "out_$count.txt"
./script.R $((min[$m])) $((sel[$s])) $((per[$p])) $((nei[$n])) > "out_$count.txt"
count=$($count+1);
done;
done;
done;
done;
