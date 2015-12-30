#!/bin/sh
if [ "$(dist/build/brain/brain test/helloWorld.bf)" != "Hello World!" ]
  then
    exit 1
fi
exit 0
