#!/bin/sh

# Evict pursuit page-cache files that nobody has accessed in 90 days, so that
# the cache (which otherwise grows without bound as crawlers walk the long
# tail of package documentation pages) cannot fill the disk.
#
# This is safe: the cache is pure derived data. Everything in it is rendered
# from /var/www/pursuit/data/verified, nginx treats a missing cache file as
# "proxy to the backend", and an evicted page is simply re-rendered and
# re-cached on the next request for it. Backups do not include the cache.
#
# The finds tolerate errors: package uploads clear cache directories
# concurrently (Handler.Caching.clearCache), so files can vanish mid-walk.
#
# Installed to /etc/cron.weekly by deploy/remote.sh.

CACHE_DIR=/var/www/pursuit/data/cache

[ -d "$CACHE_DIR" ] || exit 0

find "$CACHE_DIR" -type f -atime +90 -delete 2>/dev/null || true
find "$CACHE_DIR" -mindepth 1 -type d -empty -delete 2>/dev/null || true

exit 0
