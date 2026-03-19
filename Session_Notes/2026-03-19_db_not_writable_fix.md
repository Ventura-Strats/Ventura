# Session Notes: 2026-03-19 - Fix "DB not writable" in Python scripts

## Summary

Fixed the long-standing "DB not writable" / "Good to start: False" issue affecting all Python scripts. The root cause was a SQLAlchemy 2.x API breaking change that silently broke `executeSQL()` in `db.py`.

## Root Cause

SQLAlchemy 2.0 (installed version: 2.0.34) removed support for passing raw SQL strings to `connection.execute()`. It now requires wrapping in `sqlalchemy.text()`. The `executeSQL()` function was raising `ObjectNotExecutableError` on every call, but the error was swallowed by:
- The `@ut.trySimpleNone()` decorator on `executeSQL()` itself
- The bare `except: pass` in `testWriteDB()`

Additionally, SQLAlchemy 2.x changed from autocommit to "commit as you go" — so even if the execute had worked, writes wouldn't have been persisted without an explicit `connection.commit()`.

## What Was Broken

Every Python script that writes to MySQL via `executeSQL()`:
- `testWriteDB()` (db.py) - DB writability check always returned False
- `addScriptInitialDetailsToDB()` (init.py) - status_script INSERT never ran
- `scriptFinish()` (init.py) - status_script UPDATE never ran
- `IB_Account_Data.py` - book_nav INSERT
- `Price_IB_Exec.py` - live_px_exec TRUNCATE

The issue has been present since at least March 11, 2026 (and likely since SQLAlchemy was upgraded to 2.x).

Scripts continued to function because `Price_IB.py` proceeds past the check regardless, but no Python script status was ever recorded in `status_script`.

## Changes

### Scripts/Python/db.py
- Added `text` to SQLAlchemy import: `from sqlalchemy import create_engine, text`
- Wrapped query in `executeSQL()`: `connection.execute(text(sql_query))`
- Added `connection.commit()` after execute for write persistence

## Compatibility

The fix is backwards compatible:
- `text()` exists in SQLAlchemy 1.x (optional there, required in 2.x)
- `connection.commit()` is a no-op in 1.x autocommit mode, required in 2.x
- Code runs from shared NAS, so all machines get the fix automatically
- No Python or package upgrades needed on any machine

## Investigation: Job Failures Today

Also investigated apparent job failures/stalling on 2026-03-19. Findings:
- MySQL restart at 06:35 (from tuning session) caused one-time disruption to 06:35 batch; self-healed by 07:35
- Predict failures at 20:33-20:47 were cascade from slow model loading + Live job timing, not MySQL-related
- Intermittent Price_Live stage 2/4 failures match yesterday's pattern (likely IB Gateway connectivity)
- MySQL tuning changes did NOT cause ongoing issues

## Testing

```python
# Before fix:
>>> db.testWriteDB()
False

# After fix:
>>> db.testWriteDB()
True
```

## Orphaned Temp Tables

19 orphaned `z*` tables exist in the database from failed `testWriteDB()` runs (CREATE succeeded but INSERT/read-back/DROP failed). These are harmless and can be cleaned up manually:
```sql
SHOW TABLES LIKE 'z%';
-- DROP TABLE `table_name`; for each
```

## Next Steps

- Monitor that Python scripts now correctly register in `status_script`
- The "DB not writable" warning should no longer appear in Price_Live logs
