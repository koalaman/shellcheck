#!/bin/bash
# SC2422: ZSH math commands zcalc, zstat

zcalc 2 + 2 # [SC2422]
zstat -A info +mtime file.txt # [SC2422]
