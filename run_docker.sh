#!/usr/bin/env sh
export PATH=~/scala-$SCALA_VERSION/bin:$PATH
scala -classpath evvo.jar push.jar benchmark.json redis 6379 100
