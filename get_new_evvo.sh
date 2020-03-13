#!/usr/bin/env bash
pushd ../evvo
mvn clean compile assembly:single && cp target/evvo_2.13-0.1.0-jar-with-dependencies.jar ../push/lib
popd
