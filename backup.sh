#!/bin/bash
# IMPORTANT:
# - assumes that the db volume was mounted to "db-backup" in docker-compose.
# - assumes that the docker container was started from the docker-compose where
# the name of the service is 'postgres' and the folder of the compose is
# 'workout-tracker'
# Both are default in this repo.

APP_NAME="workout-tracker"

set -ex

cd /


echo "$(date -Iseconds) $APP_NAME-backup: starting backup" >> /var/log/custom-backup

# remove previous backup files
rm -f "tmp-backup-$APP_NAME"
rm -f "tmp-backup-$APP_NAME.tar.gz"
docker container exec workout-tracker-postgres-1 /bin/sh -c 'rm -f /db-backup/*'

# create backup of current data
docker container exec workout-tracker-postgres-1 /bin/sh -c 'pg_dump -U psql todo-app > "/db-backup/$(date -I).sql"'

LATEST_BACKUP="$(ls -t1 /var/lib/docker/volumes/workout-tracker_db-backup/_data | head -n 1)"
cp "/var/lib/docker/volumes/workout-tracker_db-backup/_data/$LATEST_BACKUP" "tmp-backup-$APP_NAME"

# compress backup
tar -cvzf "tmp-backup-$APP_NAME.tar.gz" "tmp-backup-$APP_NAME"

# upload to aws s3
echo "$(date -Iseconds) $APP_NAME-backup: uploading to aws" >> /var/log/custom-backup
/usr/local/bin/aws s3 cp "tmp-backup-$APP_NAME.tar.gz" "s3://xddq-$APP_NAME-bucket/$(date -I).tar.gz" --storage-class STANDARD_IA

echo "$(date -Iseconds) $APP_NAME-backup: finished backup successfully." >> /var/log/custom-backup

