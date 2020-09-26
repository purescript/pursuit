#! /usr/bin/env bash

set -ex

# This script should be run on the Pursuit server to deploy a new version. It
# does not attempt to take care of any of the following:
#
# - configuration of secrets/credentials
# - data migrations,
# - nginx SSL configuration,
# - periodic running of the backup script
#
# so whenever any of these are needed, they must be done manually.

if [ $(id --user) -ne 0 ]
then
  echo >&2 "This script must be run as root"
  exit 1
fi

pursuit_version="$1"

if [ "$pursuit_version" = "" ]
then
  echo >&2 "Need to provide a version"
  exit 1
fi

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

# download release
tmpdir="$(sudo -u www-data mktemp -d)"
pushd "$tmpdir"
sudo -u www-data wget "$download_url"
sudo -u www-data tar xzf pursuit.tar.gz -C /var/www/pursuit --overwrite
# We install the binary to a location outside of /var/www/pursuit so that we
# can extract tar.gz files into /var/www/pursuit safely in future deploys.
install /var/www/pursuit/pursuit /usr/local/bin/pursuit
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
