---
name: crontab
description: Generating and deploying Ventura crontabs across machines. Use when the user asks about crontab, scheduled jobs, machine distribution, or task scheduling.
---

# Crontab Management

## Source File (IMPORTANT: In Glenorchy project, NOT Ventura)

**Generator source**: `/home/fls/Models/Glenorchy/HD/Scripts/Crontab/Crontab_Generator_user.txt`

Contains entries for ALL projects. Ventura section starts around line 208.

**Generator script**: `Scripts/Crontab/Generate_Crontab.R`
- Runs every 15 minutes on machine H (crontab line 32)
- Calls `B.generateCrontabUser("user")` and `B.generateCrontabUser("su")`

## Line Format

```
H 30 03 * * 1-6 RScriptVentura.sh Model/Train.R 1
```
First character = machine letter, rest = standard cron format (minute hour day month weekday command).

## Machine Letters and IPs

```
H = 192.168.0.34  (Home, dev machine, largest RAM)
I = 192.168.0.42  (IB Gateway)
V = 192.168.0.40  (VENTURA1)
W = 192.168.0.39  (VENTURA2)
X = 192.168.0.37  (Database server - MySQL only)
Y = 192.168.0.41  (VENTURA4)
Z = 192.168.0.38  (VENTURA5)
```

## Helper Scripts (in /usr/local/bin/)

- `RScriptVentura.sh <script>` - Run R script
- `PScriptVentura.sh <script>` - Run Python script
- `KillScriptVentura.sh <script>` - Kill running script

## Time Zone

All times in **Europe/London**.

## Strategy Distribution (Predict.R, as of 2026-03-24)

**Without weights** (script_arg 0-3):
- H: 1, 4, 5, 6, 9, 10, 11
- Y: 3, 12, 14
- Z: 2, 7, 8, 13

**With weights** (script_arg 4-7):
- I: 2, 7, 8, 13
- V: 3, 12, 14
- W: 4, 9, 11
- Z: 1, 5, 6, 10

## Predict Schedule Pattern

- Kill+start merged: `KillScriptVentura.sh Predict.R N && RScriptVentura.sh Predict.R N`
- Final kill at :05 next hour (not :59) for extra runtime
- Each batch runs during specific hours matching market sessions

## Model Copy (Local Loading)

Each machine copies only needed models, staggered:
- model_*.RData copies at 03:30-03:55
- model_weights_*.RData copies at 04:01-04:25
- Uses brace expansion: `/bin/bash -c 'cp .../model_{1,4,5}.RData .../Models_Local/'`

## Common Tasks

### Adding a new cron job
1. Edit `/home/fls/Models/Glenorchy/HD/Scripts/Crontab/Crontab_Generator_user.txt`
2. Add line with machine letter prefix
3. Wait 15 min for Generate_Crontab.R or run manually

### Checking current crontab
```bash
crontab -l
```
