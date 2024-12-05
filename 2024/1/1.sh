#!/bin/bash -e

function total_distance {
    x=($(sed 's/ \+[0-9]\+//' $1 | sort -n))
    y=($(sed 's/[0-9]\+ \+//' $1 | sort -n))

    sum=0
    for ((i=0; i < ${#x[@]}; i++)); do
        let d=${x[$i]}-${y[$i]} || true
        let sum=$sum+${d#-} || true
    done
    echo $sum
}

test $(total_distance test.txt) -eq 11
total_distance input.txt

function total_similarity_score {
    sum=0
    for x in $(sed 's/ \+[0-9]\+//' $1); do
        count=$(grep -P '\s+'$x'$' $1 | wc -l)
        let sum=$sum+$x*$count || true
    done
    echo $sum
}

test $(total_similarity_score test.txt) -eq 31
total_similarity_score input.txt
