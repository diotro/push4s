#!/usr/bin/env bash
pushd ../evvo
mvn clean package && cp target/evvo_2.13-0.1.0.jar ../push/lib
popd
