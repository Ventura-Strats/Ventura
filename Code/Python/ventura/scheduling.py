"""Execution scheduling utilities.

Ports findExecutionTime() and waitTillPreviousJobHasFinished() from
the legacy execution_utils.py, using the ventura package DB/config.
"""

from __future__ import annotations

import logging
import time
from datetime import datetime, timedelta
from typing import Optional

from ventura.db import Database
from ventura.utils import print_banner

logger = logging.getLogger(__name__)

HOUR_NEW_DAY = 6
TIME_THRESHOLD_MINUTES = 5


def find_execution_time(db: Database, data_dir: str, start_time: datetime = None) -> Optional[int]:
    """Determine the current execution_time_id from schedule + current time.

    Looks at the schedule_execution table, finds the execution time closest
    to now (within TIME_THRESHOLD_MINUTES), and returns its ID.

    Returns None if no matching execution time found.
    """
    if start_time is None:
        start_time = datetime.now()

    dat = db.load_table_local("schedule_execution", data_dir)
    today_str = start_time.strftime("%Y-%m-%d")

    # Convert DB time strings to datetime objects
    exec_times = []
    for t in dat["execution_time"]:
        time_str = today_str + " " + str(t)[-8:]
        dt = datetime.strptime(time_str, "%Y-%m-%d %H:%M:%S")
        # Times before HOUR_NEW_DAY belong to "next day"
        if dt.hour < HOUR_NEW_DAY:
            dt += timedelta(days=1)
        exec_times.append(dt)

    # Calculate time differences vs now
    diffs = []
    for et in exec_times:
        diff_minutes = (start_time - et).total_seconds() / 60
        if 0 < diff_minutes < TIME_THRESHOLD_MINUTES:
            diffs.append(diff_minutes)
        else:
            diffs.append(None)

    # Find closest acceptable time
    valid_diffs = [d for d in diffs if d is not None]
    if not valid_diffs:
        return None

    min_diff = min(valid_diffs)
    min_pos = diffs.index(min_diff)
    return int(dat["execution_time_id"].values[min_pos])


def wait_for_previous_job(
    db: Database,
    data_dir: str,
    previous_script_name: str,
    last_completed_stage: int = 1,
    nb_jobs_total: int = 1,
    max_lag_prices_minutes: int = 5,
    max_wait_time_minutes: int = 5,
) -> None:
    """Wait until a previous script has completed its stage.

    Polls status_script table until the previous script shows as complete
    or the max wait time is exceeded.
    """
    wait_seconds = 5

    scripts = db.load_table_local("script", data_dir)
    script_row = scripts[scripts["script"] == previous_script_name]
    if script_row.empty:
        logger.warning("Script '%s' not found in script table", previous_script_name)
        return
    script_id = int(script_row["script_id"].values[0])

    print_banner(f"Waiting for {previous_script_name} to complete...")

    start_limit = (
        datetime.now() - timedelta(minutes=max_lag_prices_minutes)
    ).strftime("%Y-%m-%d %H:%M:%S")
    stop_time = datetime.now() + timedelta(minutes=max_wait_time_minutes)

    nb_jobs_done = 0
    while True:
        print_banner(f"Waiting... jobs done: {nb_jobs_done}/{nb_jobs_total}", False)
        time.sleep(wait_seconds)

        sql = (
            f"SELECT * FROM status_script "
            f"WHERE script_id = {script_id} "
            f"AND start >= '{start_limit}' "
            f"AND last_completed_stage >= {last_completed_stage}"
        )
        dat = db.select(sql)
        nb_jobs_done = len(dat)

        if nb_jobs_done >= nb_jobs_total:
            break
        if datetime.now() > stop_time:
            print_banner(
                "WARNING: previous job hasn't finished — continuing with what we have"
            )
            break

    print_banner(f"Wait finished — jobs done: {nb_jobs_done}/{nb_jobs_total}")
