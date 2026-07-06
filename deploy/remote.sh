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

# install anubis, the challenge proxy that nginx routes uncached package page
# requests through (see SERVER.md). Installed before the nginx config so the
# config never points at a proxy that isn't there yet.
anubis_version="1.25.0"
anubis_sha256="93e083461f43c8fd92b95a9d6b2a88a80131ecfbef15e894a3fd576cdc5749f3"
if [ "$(dpkg-query --showformat='${Version}' --show anubis 2>/dev/null)" != "$anubis_version" ]
then
  anubis_tmpdir="$(mktemp -d)"
  wget -O "$anubis_tmpdir/anubis.deb" "https://github.com/TecharoHQ/anubis/releases/download/v${anubis_version}/anubis_${anubis_version}_amd64.deb"
  echo "$anubis_sha256 $anubis_tmpdir/anubis.deb" | sha256sum --check
  apt-get install --yes "$anubis_tmpdir/anubis.deb"
  rm -r "$anubis_tmpdir"
fi

# The env file holds the instance's signing key (challenge-pass cookies are
# invalidated whenever it changes), so it is generated once and kept.
if [ ! -f /etc/anubis/pursuit.env ]
then
  touch /etc/anubis/pursuit.env
  chmod 600 /etc/anubis/pursuit.env
  cat > /etc/anubis/pursuit.env <<EOF
BIND=127.0.0.1:8923
BIND_NETWORK=tcp
METRICS_BIND=127.0.0.1:9823
METRICS_BIND_NETWORK=tcp
TARGET=http://127.0.0.1:3000
POLICY_FNAME=/etc/anubis/pursuit.botPolicies.yaml
ED25519_PRIVATE_KEY_HEX=$(openssl rand -hex 32)
EOF
fi

cp /var/www/pursuit/deploy/anubis.botPolicies.yaml /etc/anubis/pursuit.botPolicies.yaml
systemctl enable anubis@pursuit
systemctl restart anubis@pursuit

# A broken policy file makes anubis exit shortly after starting (restart
# reports success regardless), and on a redeploy nginx is already routing
# package pages to it. Fail the deploy loudly here rather than leaving that
# to be discovered as 502s.
sleep 2
systemctl is-active --quiet anubis@pursuit

# install nginx config
cp /var/www/pursuit/deploy/nginx.conf /etc/nginx/sites-enabled/pursuit.conf
systemctl reload nginx

# install cache eviction cron job
install -m 755 /var/www/pursuit/deploy/cache-eviction.sh /etc/cron.weekly/pursuit-cache-eviction

# install systemd service confing
cp /var/www/pursuit/deploy/pursuit.service /etc/systemd/system/pursuit.service
systemctl daemon-reload
systemctl restart pursuit.service

echo "[$(date)] $0: done pursuit install"
