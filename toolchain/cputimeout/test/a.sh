#!/bin/bash
# Bilal Syed Hussain
set -o nounset

/usr/bin/time -p ruby -e 'puts "ruby pid", Process.pid; (1..14000).each { |i| Math.sqrt(i ** i+1  / i * i ); puts i  if i % 1000 == 0 }' 2>_$$.time 

