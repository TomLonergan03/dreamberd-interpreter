#!/bin/bash

while getopts p: flag
do
    case "${flag}" in
        p) program=${OPTARG};;
    esac
done

cd typechecker/
npm start --program=../$program --spec=../test.txt

if [ $? -eq 0 ]; then
    echo "[>] Typecheck passed"
else
    echo "[!] Typecheck failed"
    exit 1
fi

cd ..