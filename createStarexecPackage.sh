#!/bin/bash

echo "Setting up dependencies..."

source ./sourceRequirements.sh

echo "Building Vampire + VampLean..."

make build-deps

echo "Building Static Versions..."

cd vampire && mkdir -p build && cd build && cmake -DBUILD_SHARED_LIBS=0 .. && make -j 8 && cd ../../

echo "Building VaLeaDate..."

make native-static

echo "Package stuff:..."

mkdir -p starexecDist/bin
mkdir -p starexecDist/requirements

cp valeadate-musl starexecDist/bin/valeadate-musl
cp vampire/build/vampire starexecDist/bin/vampire
cp starexec/* starexecDist/bin/
cp -r requirements/elan starexecDist/requirements/elan
cp -r vamplean starexecDist/vamplean

echo "Creating tarball..."
tar -czf starexecDist.tar.gz -C starexecDist .
echo "Cleaning up..."
rm -rf starexecDist
