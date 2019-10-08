# hs-backup

[![hs-backup Build Status](https://travis-ci.org/prikhi/hs-backup.svg?branch=master)](https://travis-ci.org/prikhi/hs-backup)

A service to backup remote files with interruption handling and archive
management.

This will use rsync to backup remote folders on an hourly, daily, monthly, &
yearly basis.

```
Haskell service that manages our fileserver backups. It should keep track of
the backup sources/destinations & existing backups. At certain time it should
use rsync to make a new backup using the last backup as the link destination.
It should track in-progress backups, and not start the next one until the
current is finished. If the network connection fails during a backup, it should
restart it. If a backup takes longer than a day, it should skip the next backup
when it is queued. Use `cp -alr` to automatically make monthly & yearly
backups if enabled for the Backup. Configuration should happen via YAML
file(location configurable by CLI flag). Program state should be serialized to
a file & read on startup so that backups can resume after a shutdown. See the
Workflow & TCache packages for persistence.
```

## License

GPL-3.0+
