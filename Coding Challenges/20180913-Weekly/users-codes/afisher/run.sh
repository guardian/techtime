#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
cd $DIR

echo $(date)
nice -n 10 timeout 45m /home/afisher/bin/sbt run
echo $(date)
