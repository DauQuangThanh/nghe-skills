#!/usr/bin/env bash
set -euo pipefail

# generate-release-notes.sh
# Generate release notes from git history
# Usage: generate-release-notes.sh <new_version> <last_tag>

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 <new_version> <last_tag>" >&2
  exit 1
fi

NEW_VERSION="$1"
LAST_TAG="$2"

# Get commits since last tag
if [ "$LAST_TAG" = "v0.0.0" ]; then
  # Check how many commits we have and use that as the limit
  COMMIT_COUNT=$(git rev-list --count HEAD)
  if [ "$COMMIT_COUNT" -gt 10 ]; then
    COMMITS=$(git log --oneline --pretty=format:"- %s" HEAD~10..HEAD)
  else
    COMMITS=$(git log --oneline --pretty=format:"- %s" HEAD~$COMMIT_COUNT..HEAD 2>/dev/null || git log --oneline --pretty=format:"- %s")
  fi
else
  COMMITS=$(git log --oneline --pretty=format:"- %s" $LAST_TAG..HEAD)
fi

# Create release notes
cat > release_notes.md << EOF
Nghệ Skills - A comprehensive collection of 40+ AI agent skills for software development, covering the complete SDLC from requirements gathering to deployment and migration.

Compatible with 17+ AI assistants including GitHub Copilot, Claude Code, Gemini CLI, Cursor, Windsurf, Amazon Q, and more.

Use the Nghệ CLI to scaffold your projects with agent skills, or download and manage them independently.

## Changelog

$COMMITS

EOF

echo "Generated release notes:"
cat release_notes.md
