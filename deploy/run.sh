#! /usr/bin/env bash

set -ex

# This script installs or updates the Pursuit server. It does not attempt to
# take care of data migrations; where they are needed, they must be done
# manually.

if [ $(id --user) -ne 0 ]
then
  echo >&2 "This script must be run as root"
  exit 1
fi

# Hardcoded for now
pursuit_github_auth_token="placeholder"
pursuit_db_backup_ssh_key="placeholder"
pursuit_version="v0.7.3"

download_url="https://github.com/purescript/pursuit/releases/download/${pursuit_version}/pursuit.tar.gz"

echo "[$(date)] $0: starting pursuit install"

# set up directories for deploying into
if [ ! -d /var/www/pursuit ]; then
  mkdir -p /var/www/pursuit
  chown -R www-data:www-data /var/www/pursuit
fi

# clone database files if not present
if [ ! -d /var/www/pursuit/data/verified ]; then
  sudo -u www-data sh -c 'cd /var/www/pursuit && git clone https://github.com/purescript/pursuit-backups data/verified'
fi

# create diffie-helman parameters (for TLS) if not present
if [ ! -f /etc/nginx/ssl_dhparam ]; then
  openssl dhparam 4096 > /etc/nginx/ssl_dhparam
fi

# download release
tmpdir="$(sudo -u www-data mktemp -d)"
pushd "$tmpdir"
sudo -u www-data wget "$download_url"
sudo -u www-data tar xzf pursuit.tar.gz -C /var/www/pursuit
popd
rm -r "$tmpdir"

# install nginx config
cp /var/www/pursuit/deploy/nginx.conf /etc/nginx/sites-enabled/pursuit.conf
systemctl reload nginx

# install systemd service confing
cp /var/www/pursuit/deploy/pursuit.service /etc/systemd/system/pursuit.service
systemctl daemon-reload
systemctl restart pursuit.service

echo "[$(date)] $0: done pursuit install"
