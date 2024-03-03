#!/usr/bin/bash

cd typechecker/
docker build -f Dockerfile.typechecker -t typechecker_image .
docker run -p 6969:6969 typechecker_image &
cd ../repl/
go run repl.go