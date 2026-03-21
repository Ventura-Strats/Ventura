#!/bin/bash
# Trim_Machine_Status.sh
# Keeps only the last 3000 lines of each .txt file in Machine_Status directory.
# 3000 lines ≈ 2 days at 1 line/minute. For multi-line metrics (e.g. heat with 4 cores),
# this covers roughly half a day — still more than enough.
# Intended to run daily via crontab.

MAX_LINES=3000
DIR="/home/fls/Data/Glenorchy/SD/Machine_Status"

find "$DIR" -name "*.txt" -type f | while read -r file; do
    lines=$(wc -l < "$file")
    if [ "$lines" -gt "$MAX_LINES" ]; then
        tail -n "$MAX_LINES" "$file" > "${file}.tmp" && mv "${file}.tmp" "$file"
    fi
done
