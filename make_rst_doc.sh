#!/bin/sh

rm -rf doc/rst && mkdir doc/rst
make doc
pandoc --read=html --write=rst doc/leo_pod.html -o doc/rst/leo_pod.rst
pandoc --read=html --write=rst doc/leo_pod_manager.html -o doc/rst/leo_pod_manager.rst
