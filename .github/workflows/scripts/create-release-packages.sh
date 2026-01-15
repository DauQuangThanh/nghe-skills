#!/usr/bin/env bash
set -euo pipefail

# create-release-packages.sh (workflow-local)
# Build Nghệ Skills release archive with all skills.
# 
# This script packages skills from my-skills/skills/ folder into a single universal package.
# Each skill follows the Agent Skills standard (https://github.com/agentskills/agentskills):
#   - Each skill has its own directory (e.g., backend-coding/)
#   - Each skill contains a SKILL.md file with YAML frontmatter
#   - Skills can include scripts, examples, and resources in subdirectories
#
# The package contains all skills in a skills/ folder. The CLI tool (nghe) will copy
# these skills to the appropriate agent-specific folders when installing:
#   - Claude: .claude/skills/{skill-name}/SKILL.md
#   - GitHub Copilot: .github/skills/{skill-name}/SKILL.md (official Agent Skills standard)
#   - Gemini: .gemini/skills/{skill-name}/SKILL.md
#   - And more...
#
# For GitHub Copilot, see: https://docs.github.com/en/copilot/concepts/agents/about-agent-skills
#
# Usage: .github/workflows/scripts/create-release-packages.sh <version>
#   Version argument should include leading 'v'.
#   Example: $0 v0.2.0

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <version-with-v-prefix>" >&2
  exit 1
fi
NEW_VERSION="$1"
if [[ ! $NEW_VERSION =~ ^v[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "Version must look like v0.0.0" >&2
  exit 1
fi

echo "Building release package for $NEW_VERSION"

# Create and use .genreleases directory for all build artifacts
GENRELEASES_DIR=".genreleases"
mkdir -p "$GENRELEASES_DIR"
rm -rf "$GENRELEASES_DIR"/* || true

# Build single universal package
build_package() {
  local base_dir="$GENRELEASES_DIR/nghe-skills"
  echo "Building universal skills package..."
  mkdir -p "$base_dir"
  
  # Copy all skills to the package
  # The CLI tool will copy these to appropriate agent folders during installation
  if [[ -d my-skills/skills ]]; then
    mkdir -p "$base_dir/skills"
    cp -r my-skills/skills/* "$base_dir/skills/"
    echo "Copied $(ls -1 my-skills/skills | wc -l) skills to package"
  else
    echo "Error: my-skills/skills directory not found" >&2
    exit 1
  fi
  
  # Create the ZIP archive
  ( cd "$base_dir" && zip -r "../nghe-skills.zip" . )
  echo "Created $GENRELEASES_DIR/nghe-skills.zip"
}

build_package

echo "Release archive created:"
ls -lh "$GENRELEASES_DIR"/nghe-skills.zip

