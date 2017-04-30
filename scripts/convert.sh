#!/usr/bin/env sh

set -e

HERE=$(cd `dirname $0`; pwd)
VERSION=v4
SRC_DIR=$HERE/../mattermost-api-reference/$VERSION/source/
OUT_DIR=$HERE/../reference/$VERSION/

mkdir -p $OUT_DIR

for y in $SRC_DIR/*.yaml
do
  out_name=$(basename -s .yaml $y).json
  echo $y =\> $OUT_DIR/$out_name
  cat $y | $HERE/yaml2json.py > $OUT_DIR/$out_name 
done
