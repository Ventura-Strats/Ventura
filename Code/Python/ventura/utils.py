"""Ventura utilities: date/timezone helpers, logging, and legacy compatibility."""

from __future__ import annotations

import logging
import os
from datetime import date, datetime, timedelta

import pytz

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------

BANNER_SIZE = 100
_HASH_BANNER = "#" * BANNER_SIZE


def setup_logging(name: str, level: int = logging.INFO) -> logging.Logger:
    """Return a logger with a consistent format."""
    logger = logging.getLogger(name)
    if not logger.handlers:
        handler = logging.StreamHandler()
        fmt = logging.Formatter(
            "%(asctime)s | %(name)-20s | %(levelname)-7s | %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S",
        )
        handler.setFormatter(fmt)
        logger.addHandler(handler)
    logger.setLevel(level)
    return logger


def print_banner(msg: str, big: bool = True) -> None:
    """Print a timestamped banner line (mirrors legacy printBanner)."""
    msg = str(msg)
    time_now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    ts_len = len(time_now)
    chars_used = len(msg) + 9 + ts_len

    if chars_used <= BANNER_SIZE:
        fill = " " * (BANNER_SIZE - chars_used)
        middle = f"### {msg}{fill} {time_now} ###"
    else:
        middle = f"### {msg} - {time_now}"

    if big:
        print(_HASH_BANNER)
    print(middle)
    if big:
        print(_HASH_BANNER)


# ---------------------------------------------------------------------------
# Date / timezone helpers
# ---------------------------------------------------------------------------

TZ_ASIA = "Asia/Hong_Kong"
TZ_EUROPE = "Europe/London"
TZ_AMERICA = "America/New_York"


def attach_tz(dt: datetime, tz: str) -> datetime:
    """Localize a naive datetime to *tz*."""
    return pytz.timezone(tz).localize(dt)


def convert_tz(dt: datetime, tz_from: str, tz_to: str) -> datetime:
    """Convert a naive datetime assumed to be in *tz_from* to *tz_to*."""
    return attach_tz(dt, tz_from).astimezone(pytz.timezone(tz_to))


def date_today() -> date:
    """Return the current trading date.

    Logic: uses Hong Kong wall-clock to get a calendar date, then checks
    whether New York is still on the previous calendar day.  If NY hasn't
    passed 17:00 yet, we roll back one day (the "NY day" hasn't ended).
    Weekends are pushed to the next Monday.
    """
    time_now = datetime.now()
    time_hk = attach_tz(time_now, TZ_ASIA)
    time_ny = time_hk.astimezone(pytz.timezone(TZ_AMERICA))
    d = time_hk.date()
    d_ny = time_ny.date()

    if d_ny != d and d.weekday() != 0:
        if time_ny.hour <= 17:
            d -= timedelta(days=1)

    wd = d.weekday()
    if wd == 5:
        d += timedelta(days=2)
    elif wd == 6:
        d += timedelta(days=1)

    return d


def calc_previous_date(d: date) -> date:
    """Return the previous trading date (skip weekends)."""
    wd = d.weekday()
    if wd == 0:       # Monday -> Friday
        return d - timedelta(days=3)
    elif wd == 6:     # Sunday -> Friday
        return d - timedelta(days=2)
    return d - timedelta(days=1)


def datetime_export_stamp() -> str:
    """Return a filename-safe timestamp string."""
    return datetime.now().strftime("%Y-%m-%d_%Hh%Mm%Ss")


# ---------------------------------------------------------------------------
# Numeric helpers
# ---------------------------------------------------------------------------

def round_to_multiple(x: float, base: float) -> float:
    """Round *x* to the nearest multiple of *base*."""
    return base * round(x / base)


def sign(x: float) -> int:
    """Return 1, -1, or 0."""
    if x > 0:
        return 1
    elif x < 0:
        return -1
    return 0


# ---------------------------------------------------------------------------
# Filesystem
# ---------------------------------------------------------------------------

def ensure_dir(path: str) -> None:
    """Create directory if it doesn't exist."""
    os.makedirs(path, exist_ok=True)


# ---------------------------------------------------------------------------
# DataFrame helpers
# ---------------------------------------------------------------------------

def left_join(df_left, df_right, cols):
    """Convenience wrapper around pandas merge (left join)."""
    return df_left.merge(df_right, on=cols, how="left")


def force_column_type(df, cols, type_fn):
    """Try to coerce *cols* in *df* to *type_fn*, ignoring failures."""
    for col in cols:
        try:
            df[col] = df[col].map(type_fn)
        except Exception:
            pass
    return df


# ---------------------------------------------------------------------------
# Legacy compatibility decorators
# ---------------------------------------------------------------------------
# The old codebase uses @trySimpleNone(), @tryDiagnosticNone() etc. to
# silently swallow exceptions.  These are kept for backward compat during
# migration.  New code should use proper try/except with logging.

def _try_decorator(print_diag: bool, return_arg0: bool):
    """Build an error-swallowing decorator (legacy pattern)."""
    def decorator(fn):
        def wrapper(*args, **kwargs):
            name = fn.__name__
            if print_diag:
                print(_HASH_BANNER)
                print_banner(f"{name} - Starting", False)
            result = args[0] if (return_arg0 and args) else None
            try:
                result = fn(*args, **kwargs)
            except Exception as exc:
                if print_diag:
                    print_banner(f"{name} - Error:", False)
                    print(exc)
            finally:
                if print_diag:
                    print_banner(f"{name} - Done. Result:", False)
                    print(result)
                    print(_HASH_BANNER)
                return result
        return wrapper
    return decorator


def try_simple_none():
    return _try_decorator(False, False)

def try_simple_dat():
    return _try_decorator(False, True)

def try_diagnostic_none():
    return _try_decorator(True, False)

def try_diagnostic_dat():
    return _try_decorator(True, True)
