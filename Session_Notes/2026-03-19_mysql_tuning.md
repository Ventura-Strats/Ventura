# Session: MySQL Server Tuning (2026-03-19)

## Summary

Audited and tuned the MySQL 8.0.45 server on ventura3 (192.168.0.37) to fix high memory usage and iowait issues. Also created a proper swap file as OOM protection.

**Key discovery**: Server runs MySQL 8.0.45, not MariaDB (MariaDB client library is installed for R's `RMariaDB` package, but the server is MySQL Community Server).

## Changes Made

### MySQL Configuration (`/etc/mysql/mariadb.cnf`)

| Setting | Before | After | Why |
|---|---|---|---|
| `innodb_buffer_pool_size` | 32G (running, config said 24G) | **20G** | Was reserving all 32GB RAM for a pool only using 7.6GB. 20G fits the entire working set with room for OS |
| `innodb_buffer_pool_instances` | 32 | **4** | 32 instances on 4 CPU cores adds unnecessary mutex overhead |
| `innodb_flush_method` | fsync | **O_DIRECT** | Eliminates double-buffering (InnoDB buffer pool + OS page cache) |
| `innodb_redo_log_capacity` | 100MB | **1GB** | Reduces checkpoint flush storms (main iowait cause) |
| `innodb_read_io_threads` | 64 | **4** | 128 I/O threads on 4 cores causes context-switch thrashing |
| `innodb_write_io_threads` | 64 | **4** | Same as above |
| `tmp_table_size` | 16MB | **128MB** | Prevents temp table spills to disk during complex queries |
| `max_heap_table_size` | 16MB | **128MB** | Must match tmp_table_size |
| `log_bin` | ON (30-day retention) | **OFF** (`skip-log-bin`) | No replication needed; saves ~30-50% write I/O |

### System Changes

| Change | Detail |
|---|---|
| Swap file | Recreated at 8GB (was 351MB from a previous attempt) |
| `vm.swappiness` | Set to 10 (persistent via `/etc/sysctl.conf`) |

### Impact

- **RAM free**: 9GB → 17GB (freed 8GB by reducing buffer pool)
- **RAM used by MySQL**: ~20.8GB → ~2.9GB (will grow as buffer pool warms up to ~8GB working set)
- **Expected iowait reduction**: Significant, from O_DIRECT + larger redo log + binlog removal

## Backup

- Config backup: `/etc/mysql/mariadb.cnf.bak.20260319`

## Server Specs (for reference)

- **CPU**: Intel i5-6600T (4 cores, 2.7GHz)
- **RAM**: 32GB
- **Disk**: 234GB NVMe SSD (161GB used)
- **OS**: Ubuntu 22.04
- **DB**: MySQL 8.0.45 (not MariaDB)
- **Database size**: Ventura schema ~22.5GB
- **Largest tables**: `histo_technicals_dbl` (11.6GB), `histo_technicals_int` (4.8GB), `archive_predict` (2.8GB)

## Testing Checklist

- [x] MySQL restarts successfully after config change
- [x] All settings verified via `SHOW GLOBAL VARIABLES`
- [x] Swap file active at 8GB
- [x] Existing R connections work (DB credentials unchanged)
- [ ] Monitor iowait over next few days: `ssh ventura3 "vmstat 5 60"`
- [ ] Check buffer pool hit rate after warmup: `mysql -e "SHOW GLOBAL STATUS LIKE 'Innodb_buffer_pool_read%'"`

## Next Steps

- Monitor iowait and memory usage over the coming days to confirm improvements
- If iowait persists, investigate NFS mounts (several NAS mounts to 192.168.0.11) as a secondary source
