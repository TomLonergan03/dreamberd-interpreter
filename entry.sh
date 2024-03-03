#!/bin/bash

while getopts p: flag
do
    case "${flag}" in
        p) program=${OPTARG};;
    esac
done

cd ../typechecker/
$SPEC=${cat ../DreamBerdSpec.md}
$SPEC=${perl -e "print join( q//, map { s/\\s+/_/g; lc } split /[^\\s\\w]+/, $SPEC )"}
$TYPECHECKS=${curl -X POST -H "Content-Type: application/json" -d "{\"program\": \"$program\", \"spec\": \"$SPEC\"}" http://localhost:6969}
if [ $TYPECHECKS == "true" ]; then
    echo "[>] Typecheck passed"
else
    echo "[!] Typecheck failed"
    exit 1
fi

cd ../parser/
perl Main.pl ../command.berd
if [ $? -eq 0 ]; then
    echo "[>] Parsing passed"
else
    echo "[!] Parsing failed"
    exit 1
fi
cd ..

