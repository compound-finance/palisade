#!/bin/bash

set -eo pipefail

bucket="$1"

function print_usage() {
	echo "usage: ./scripts/deploy-gcloud.sh {DESTINATION_BUCKET_NAME}"
	echo ""
	exit 1
}

if [ -z "$bucket" ]; then
	print_usage
fi

command -v gsutil >/dev/null 2>&1 || { echo >&2 "I require Google Cloud SDK to be installed, please visit https://cloud.google.com/sdk/ to continue."; exit 1; }

echo "Deploying to bucket $bucket..."

if [ -z "$skip_build" ]; then
	# Grab yarn or npm, preferring yarn
	yarn_or_npm=$(which yarn || which npm || { echo >&2 "I require yarn or npm to be installed. Aborting."; exit 1; })

	gcloud_cache_seconds="300"

	if [ -z "$skip_css" ]; then
		$yarn_or_npm run build-css
	fi
	$yarn_or_npm run build
else
	gcloud_cache_seconds="300"
fi

cp ./build/index.html ./build/dapp.html

# Human-readable version stamp
date | tee ./build/.__v.txt
# Machine-readable version stamp (using epoch seconds) as JSON to support easy extendability.
date +'{ "version": %s}' | tee ./build/.__v.json

# If we don't specify max-age, it defaults to 3600 seconds.
# Run `gsutil help metadata` for more info
gsutil -m -h "Cache-Control:public, max-age=$gcloud_cache_seconds" rsync -R ./build/static gs://$bucket/static
gsutil -m -h "Cache-Control:public, max-age=$gcloud_cache_seconds" rsync -R ./build gs://$bucket

echo "Deployment complete, you may check bucket $bucket"
