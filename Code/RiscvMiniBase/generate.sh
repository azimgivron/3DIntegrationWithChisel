#!/bin/bash

simple=$(cat ./src/main/scala/Config.scala | grep -c SimpleDatapath)
double=$(cat ./src/main/scala/Config.scala | grep -c DoubleAluDatapath)
if [[ "$simple" != 0 ]]
then
	mkdir -p ./generated-src/verilog
	make
	mv ./generated-src/Tile.v ./generated-src/verilog/Original.v
	sed -i 's/SimpleDatapath/DoubleAluDatapath/g' ./src/main/scala/Config.scala
	make
	mv ./generated-src/Tile.v ./generated-src/verilog/DoubleAlus.v
elif [[ "$double" != 0 ]]
then
	mkdir -p ./generated-src/verilog
	make
	mv ./generated-src/Tile.v ./generated-src/verilog/DoubleAlus.v
	sed -i 's/DoubleAluDatapath/SimpleDatapath/g' ./src/main/scala/Config.scala
	make
	mv ./generated-src/Tile.v ./generated-src/verilog/Original.v
fi