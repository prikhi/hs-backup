# hs-backup

[![hs-backup Build Status](https://travis-ci.org/prikhi/hs-backup.svg?branch=master)](https://travis-ci.org/prikhi/hs-backup)

hs-backup is a service that backups up remote folders using `rsync`.

* Configured with YAML.
* Safely handles program restarts, system reboots, and network interruptions -
  retries a backup until it has completed.
* Skips enqueuing new backups if syncing takes longer than the backup period.
* Uses the link destination `rsync` flag to compare files from previous
  backups, saving bandwidth & disk space.
* Allows limiting of bandwidth usage to prevent network congestion during
  backup process.
* Allows ignoring files & directories during backup process using rsync's
  filter patterns.
* Uses hardlinks to archive backups for longer periods of time, turning hourly
  backups into daily, monthly, & yearly backups.
* Automatically culls old backups.

## Build / Install

You can build & install the service using [stack][stack]:

```
stack install
```

This will put the `hs-backup` executable in your `~/.local/share/` folder.


Developers working on `hs-backup` probably want to start a file-watching build
server:

```
stack build --pedantic --file-watch --fast
```


## Configuration

By default, `hs-backup` will attempt to read a configuration file from
`/etc/hs-backup.yaml`. You can use the `--config-file` or `-c` flag to pass a
custom configuration file to the server.

See the `example-hs-backup.yaml` file in this repository for an example of a
configuration file as well as documentation on each field.


## TODO

While the server is completely usable, there are some improvements we'd like to
implement:

* More configuration options:
    * Backup folder format strings
    * BackupRate folder names
    * Maximum backups to retain
    * Delay time for checking for backups
    * Delay time for retrying a backup


## License

GPL-3.0+


[stack]: https://docs.haskellstack.org/en/stable/README/
