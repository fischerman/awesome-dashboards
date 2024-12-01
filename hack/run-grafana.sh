#!/bin/bash

set -x

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

docker rm -f grafana
docker run -d \
    -v $SCRIPT_DIR/../grafana/datasources.yaml:/etc/grafana/provisioning/datasources/datasources.yaml \
    -v $SCRIPT_DIR/../grafana/source1.yaml:/etc/grafana/provisioning/dashboards/source1.yaml \
    -v $SCRIPT_DIR/../grafana/dashboards/:/dashboards \
    --name grafana --net host grafana/grafana
