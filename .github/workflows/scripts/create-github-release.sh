#!/usr/bin/env bash
set -euo pipefail

# create-github-release.sh
# Create a GitHub release with all Nghệ Skills package zip files
# Usage: create-github-release.sh <version>

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <version>" >&2
  exit 1
fi

VERSION="$1"

# Remove 'v' prefix from version for release title
VERSION_NO_V=${VERSION#v}

gh release create "$VERSION" \
  .genreleases/nghe-skills-copilot-sh-"$VERSION".zip \
  .genreleases/nghe-skills-copilot-ps-"$VERSION".zip \
  .genreleases/nghe-skills-claude-sh-"$VERSION".zip \
  .genreleases/nghe-skills-claude-ps-"$VERSION".zip \
  .genreleases/nghe-skills-gemini-sh-"$VERSION".zip \
  .genreleases/nghe-skills-gemini-ps-"$VERSION".zip \
  .genreleases/nghe-skills-cursor-agent-sh-"$VERSION".zip \
  .genreleases/nghe-skills-cursor-agent-ps-"$VERSION".zip \
  .genreleases/nghe-skills-opencode-sh-"$VERSION".zip \
  .genreleases/nghe-skills-opencode-ps-"$VERSION".zip \
  .genreleases/nghe-skills-qwen-sh-"$VERSION".zip \
  .genreleases/nghe-skills-qwen-ps-"$VERSION".zip \
  .genreleases/nghe-skills-windsurf-sh-"$VERSION".zip \
  .genreleases/nghe-skills-windsurf-ps-"$VERSION".zip \
  .genreleases/nghe-skills-codex-sh-"$VERSION".zip \
  .genreleases/nghe-skills-codex-ps-"$VERSION".zip \
  .genreleases/nghe-skills-kilocode-sh-"$VERSION".zip \
  .genreleases/nghe-skills-kilocode-ps-"$VERSION".zip \
  .genreleases/nghe-skills-auggie-sh-"$VERSION".zip \
  .genreleases/nghe-skills-auggie-ps-"$VERSION".zip \
  .genreleases/nghe-skills-roo-sh-"$VERSION".zip \
  .genreleases/nghe-skills-roo-ps-"$VERSION".zip \
  .genreleases/nghe-skills-codebuddy-sh-"$VERSION".zip \
  .genreleases/nghe-skills-codebuddy-ps-"$VERSION".zip \
  .genreleases/nghe-skills-amp-sh-"$VERSION".zip \
  .genreleases/nghe-skills-amp-ps-"$VERSION".zip \
  .genreleases/nghe-skills-shai-sh-"$VERSION".zip \
  .genreleases/nghe-skills-shai-ps-"$VERSION".zip \
  .genreleases/nghe-skills-q-sh-"$VERSION".zip \
  .genreleases/nghe-skills-q-ps-"$VERSION".zip \
  .genreleases/nghe-skills-bob-sh-"$VERSION".zip \
  .genreleases/nghe-skills-bob-ps-"$VERSION".zip \
  --title "Nghệ Skills - $VERSION_NO_V" \
  --notes-file release_notes.md
