#!/bin/bash
cat resources/day08_input_real.txt | sed -e 's/.* | //' | sed -e 's/ /\n/g' | grep -v "^......\?$" | wc -l
