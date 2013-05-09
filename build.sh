#!/usr/bin/env sh
set -e
set -u
rm -f *.hp *.aux *.ps

clear
set +e
hlint *.hs --report -q
set -e
ghc -o Main --make -Wall -O2 -prof -caf-all -auto-all -fforce-recomp brainfk_interpreter.hs 
