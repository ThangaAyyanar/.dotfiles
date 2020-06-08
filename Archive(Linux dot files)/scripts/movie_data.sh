#!/bin/sh

movie_name=$1

if [[ -z "$movie_name" ]]; then 
    echo "Empty Arguments"
    exit
fi

movie_name=$(echo $movie_name | sed "s/ /+/g")

curl --silent "http://www.omdbapi.com/?apikey=5e540903&t=$movie_name" | jq
