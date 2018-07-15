#!/bin/bash
find /home/bjorn/code/jsts/src/org -type f -name "*.ast" -delete
mvn package -DskipTests
java -jar target/java2estree-0.2.0.jar /home/bjorn/code/jts/modules/core/src/main/java /home/bjorn/code/jsts/src
for f in $(find /home/bjorn/code/jsts/src/org -name '*.ast')
do
  echo "Processing $f" 
  ./node_modules/astring/bin/astring --indent '	' $f > "${f/%.ast/.js}"
done

