#!/usr/bin/env sh
set -e
set -u
rm -f *.hp *.aux *.ps

clear; ghc --make -Wall -O2 -prof -caf-all -auto-all -fforce-recomp brainfk_interpreter.hs 
time ./brainfk_interpreter r +RTS -s -h < samples/99botles.bf 
hp2ps brainfk_interpreter.hp
open brainfk_interpreter.ps
