#!/bin/bash
# Uploads given directory (default: dist) to gh-pages branch under directory <branch name>
# usage: upload-github-pages.sh [directory]

set -e
set +x

if [ -z "$TRAVIS" -o "$TRAVIS_PULL_REQUEST" != "false" ]; then
  echo "This is not a push Travis-ci build, doing nothing..."
  exit 0
else
  echo "Deploying to Github pages..."
fi

DOCDIR=.gh-pages
DEPLOY_SRC=${1:-dist}

DEPLOY_TARGET=${TRAVIS_TAG:+tag/$TRAVIS_TAG}
DEPLOY_TARGET=${DEPLOY_TARGET:-branch/$TRAVIS_BRANCH}

REPO_USER=${TRAVIS_REPO_SLUG%/*}
REPO_NAME=${TRAVIS_REPO_SLUG#*/}

REDACT_PATTERN=""

# Error out if $GH_TOKEN and $DH_DEPLOY_KEY are empty or unset
if [ -n "$GH_TOKEN" ]; then
  REPO="https://${GH_TOKEN}@github.com/${TRAVIS_REPO_SLUG}.git"
  REDACT_PATTERN="s/$GH_TOKEN/!REDACTED!/g"
elif [ -n "$GH_DEPLOY_KEY" ]; then
  # $GH_DEPLOY_KEY should be inserted into project settings quoted and with newlines escaped as \n
  eval `ssh-agent`
  printf "%b" "$GH_DEPLOY_KEY" | ssh-add /dev/stdin
  REPO="git@github.com:${TRAVIS_REPO_SLUG}.git"
else
  echo "GH_TOKEN or GH_DEPLOY_KEY variable must be set"
  exit 1
fi

git clone $REPO $DOCDIR 2>&1 | sed -e "$REDACT_PATTERN"
git -C $DOCDIR checkout gh-pages ||
(git -C $DOCDIR checkout --orphan gh-pages &&
  find $DOCDIR \! \( -path "*/.git/*" -o -path "*/.git" -o -path "$DOCDIR" \) -delete)

rm -Rf $DOCDIR/$DEPLOY_TARGET || true
mkdir -p $DOCDIR/$DEPLOY_TARGET
cp -rT $DEPLOY_SRC $DOCDIR/$DEPLOY_TARGET

git -C $DOCDIR add -A .
git -C $DOCDIR config user.email "travis@travis-ci.org"
git -C $DOCDIR config user.name "Travis"

git -C $DOCDIR commit --allow-empty \
  -m "Travis build $TRAVIS_BUILD_NUMBER for $TRAVIS_BRANCH (${TRAVIS_COMMIT:0:8})" \
  -m "Original commit: $TRAVIS_COMMIT
Publication location: https://${REPO_USER}.github.io/${REPO_NAME}/${DEPLOY_TARGET}
Travis job: https://travis-ci.org/${TRAVIS_REPO_SLUG}/jobs/${TRAVIS_JOB_ID}"

git -C $DOCDIR push origin gh-pages 2>&1 | sed -e "$REDACT_PATTERN"

if [ -n "$GH_DEPLOY_KEY" ]; then eval `ssh-agent -k`; fi
