# Server notes for pursuit.purescript.org

This file documents the state that lives only on the server or in the
DigitalOcean account, which no deploy touches and which would otherwise be
tribal knowledge. Last verified: 2026-07-05.

## The droplet

- DigitalOcean droplet 177127298 (`pursuit.purescript.org`), s-2vcpu-4gb,
  nyc1, IP 206.189.189.151.
- Ubuntu 24.04
- DNS for purescript.org is managed at Namecheap, not DigitalOcean.
- The CI runner in `.github/workflows/ci.yml` must build on the same Ubuntu
  release the server runs, or the binary's glibc requirement won't be
  satisfiable. Bump both together.

## Server-only state (not managed by deploys)

- **Swap**: 4GB `/swapfile`, registered in `/etc/fstab`, with
  `vm.swappiness=10` set in `/etc/sysctl.d/99-swappiness.conf`.
- **Service enablement**: `pursuit` and `nginx` are `systemctl enable`d.
- **Backups**: root crontab runs `deploy/backup.sh` daily at 13:00 UTC,
  copying `data/verified/` (the actual database) to `/var/www/pursuit-backups`
  and pushing to the `purescript/pursuit-backups` GitHub repo. The page cache
  (`data/cache/`) is *not* backed up; it is pure derived data.
- **nginx reload cron**: root crontab reloads nginx daily at 12:00 UTC to pick
  up renewed TLS certificates.
- **TLS**: certbot (apt package) with the systemd `certbot.timer`; webroot
  under `/var/www/letsencrypt-webroot`.
- **Cache eviction**: `/etc/cron.weekly/pursuit-cache-eviction` (installed by
  `remote.sh` from `deploy/cache-eviction.sh`) deletes cache files not
  accessed in 90 days so the cache cannot fill the disk.

## DigitalOcean account state

- **Uptime check** `pursuit-search-backend` targets
  `https://pursuit.purescript.org/search?q=maybe` from us_east and eu_west.
  The target is deliberately a search URL: search always exercises the
  backend, whereas package pages are served by nginx from the page cache
  even when the backend is down (which is how the 2025-26 OOM crash loop
  went unnoticed for months). One alert: down for 2+ minutes.
- **Alert policy**: droplet memory utilization above 90% sustained for one
  hour. This is the early warning that the package database is outgrowing
  the droplet again.
- Alerts email *****@thomashoneyman.com.

## Memory background

The whole-package docs JSON is the unit of storage; rendering any single
documentation page decodes the entire file for that package version, which
transiently needs tens of times the file size in heap. A few generated
packages (react-icons, elmish-html, deku, google-apps, ...) have 10-25MB
files, so concurrent uncached requests for their pages can exhaust the heap.
`Handler.Database.lookupPackage` therefore serialises decodes of files over
5MB, and the search index is built one package at a time
(`createSearchIndexFromDatabase`). If heap-exhaustion restarts reappear in
the journal (`journalctl -u pursuit | grep "Heap exhausted"`), start there.
