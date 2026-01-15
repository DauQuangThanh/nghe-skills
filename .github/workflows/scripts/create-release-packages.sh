#!/usr/bin/env bash
set -euo pipefail

# create-release-packages.sh (workflow-local)
# Build Nghệ Skills release archives for each supported AI assistant and script type.
# 
# This script packages skills from my-skills/skills/ folder for distribution to various AI agents.
# Each skill follows the Agent Skills standard (https://github.com/agentskills/agentskills):
#   - Each skill has its own directory (e.g., backend-coding/)
#   - Each skill contains a SKILL.md file with YAML frontmatter
#   - Skills can include scripts, examples, and resources in subdirectories
#
# Directory structure after packaging:
#   - Claude: .claude/skills/{skill-name}/SKILL.md
#   - GitHub Copilot: .github/skills/{skill-name}/SKILL.md (official Agent Skills standard)
#   - Gemini: .gemini/skills/{skill-name}/SKILL.md
#   - And more...
#
# For GitHub Copilot, see: https://docs.github.com/en/copilot/concepts/agents/about-agent-skills
#
# Usage: .github/workflows/scripts/create-release-packages.sh <version>
#   Version argument should include leading 'v'.
#   Optionally set AGENTS and/or SCRIPTS env vars to limit what gets built.
#     AGENTS  : space or comma separated subset of: claude gemini copilot cursor-agent qwen opencode windsurf codex amp shai bob (default: all)
#     SCRIPTS : space or comma separated subset of: sh ps (default: both)
#   Examples:
#     AGENTS=claude SCRIPTS=sh $0 v0.2.0
#     AGENTS="copilot,gemini" $0 v0.2.0
#     SCRIPTS=ps $0 v0.2.0

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <version-with-v-prefix>" >&2
  exit 1
fi
NEW_VERSION="$1"
if [[ ! $NEW_VERSION =~ ^v[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "Version must look like v0.0.0" >&2
  exit 1
fi

echo "Building release packages for $NEW_VERSION"

# Create and use .genreleases directory for all build artifacts
GENRELEASES_DIR=".genreleases"
mkdir -p "$GENRELEASES_DIR"
rm -rf "$GENRELEASES_DIR"/* || true

# NOTE: rewrite_paths() function removed as scripts and templates folders no longer exist
# All skills are self-contained in my-skills/skills/ directory

# DEPRECATED: This function is no longer used as commands folder has been removed
# Skills are now in my-skills folder and copied directly
generate_commands() {
  local agent=$1 ext=$2 arg_format=$3 output_dir=$4 script_variant=$5
  mkdir -p "$output_dir"
  for template in commands/*.md; do
    [[ -f "$template" ]] || continue
    local name description script_command agent_script_command body
    name=$(basename "$template" .md)
    
    # Normalize line endings
    file_content=$(tr -d '\r' < "$template")
    
    # Extract description and script command from YAML frontmatter
    description=$(printf '%s\n' "$file_content" | awk '/^description:/ {sub(/^description:[[:space:]]*/, ""); print; exit}')
    script_command=$(printf '%s\n' "$file_content" | awk -v sv="$script_variant" '/^[[:space:]]*'"$script_variant"':[[:space:]]*/ {sub(/^[[:space:]]*'"$script_variant"':[[:space:]]*/, ""); print; exit}')
    
    if [[ -z $script_command ]]; then
      echo "Warning: no script command found for $script_variant in $template" >&2
      script_command="(Missing script command for $script_variant)"
    fi
    
    # Extract agent_script command from YAML frontmatter if present
    agent_script_command=$(printf '%s\n' "$file_content" | awk '
      /^agent_scripts:$/ { in_agent_scripts=1; next }
      in_agent_scripts && /^[[:space:]]*'"$script_variant"':[[:space:]]*/ {
        sub(/^[[:space:]]*'"$script_variant"':[[:space:]]*/, "")
        print
        exit
      }
      in_agent_scripts && /^[a-zA-Z]/ { in_agent_scripts=0 }
    ')
    
    # Replace {SCRIPT} placeholder with the script command
    body=$(printf '%s\n' "$file_content" | sed "s|{SCRIPT}|${script_command}|g")
    
    # Replace {AGENT_SCRIPT} placeholder with the agent script command if found
    if [[ -n $agent_script_command ]]; then
      body=$(printf '%s\n' "$body" | sed "s|{AGENT_SCRIPT}|${agent_script_command}|g")
    fi
    
    # Remove the scripts: and agent_scripts: sections from frontmatter while preserving YAML structure
    body=$(printf '%s\n' "$body" | awk '
      /^---$/ { print; if (++dash_count == 1) in_frontmatter=1; else in_frontmatter=0; next }
      in_frontmatter && /^scripts:$/ { skip_scripts=1; next }
      in_frontmatter && /^agent_scripts:$/ { skip_scripts=1; next }
      in_frontmatter && /^[a-zA-Z].*:/ && skip_scripts { skip_scripts=0 }
      in_frontmatter && skip_scripts && /^[[:space:]]/ { next }
      { print }
    ')
    
    # Apply other substitutions
    body=$(printf '%s\n' "$body" | sed "s/{ARGS}/$arg_format/g" | sed "s/__AGENT__/$agent/g" | rewrite_paths)
    
    case $ext in
      toml)
        body=$(printf '%s\n' "$body" | sed 's/\\/\\\\/g')
        { echo "description = \"$description\""; echo; echo "prompt = \"\"\""; echo "$body"; echo "\"\"\""; } > "$output_dir/nghe.$name.$ext" ;;
      md)
        echo "$body" > "$output_dir/nghe.$name.$ext" ;;
      agent.md)
        echo "$body" > "$output_dir/nghe.$name.$ext" ;;
    esac
  done
}

# DEPRECATED: This function is no longer used as agents folder has been removed
# Skills are now in my-skills folder and copied directly
generate_agents() {
  local agent=$1 ext=$2 arg_format=$3 output_dir=$4
  mkdir -p "$output_dir"
  for agent_file in agents/*.md; do
    [[ -f "$agent_file" ]] || continue
    local name body
    name=$(basename "$agent_file" .md)

    # Read and process agent file
    file_content=$(tr -d '\r' < "$agent_file")

    # Apply path rewriting and agent substitution
    body=$(printf '%s\n' "$file_content" | sed "s/__AGENT__/$agent/g" | rewrite_paths)

    case $ext in
      toml)
        # Extract description for TOML format
        description=$(printf '%s\n' "$file_content" | awk '/^description:/ {sub(/^description:[[:space:]]*/, ""); gsub(/"/, ""); print; exit}')
        body=$(printf '%s\n' "$body" | sed 's/\\/\\\\/g')
        { echo "description = \"$description\""; echo; echo "prompt = \"\"\""; echo "$body"; echo "\"\"\""; } > "$output_dir/hanoi.$name.$ext" ;;
      md)
        echo "$body" > "$output_dir/hanoi.$name.$ext" ;;
      agent.md)
        echo "$body" > "$output_dir/hanoi.$name.$ext" ;;
    esac
  done
}

# DEPRECATED: This function is no longer used as .github/agents structure has changed
generate_copilot_prompts() {
  local agents_dir=$1 prompts_dir=$2
  mkdir -p "$prompts_dir"

  # Generate a .prompt.md file for each .agent.md file
  for agent_file in "$agents_dir"/hanoi.*.agent.md; do
    [[ -f "$agent_file" ]] || continue

    local basename=$(basename "$agent_file" .agent.md)
    local prompt_file="$prompts_dir/${basename}.prompt.md"

    # Create prompt file with agent frontmatter
    cat > "$prompt_file" <<EOF
---
agent: ${basename}
---
EOF
  done
}

build_variant() {
  local agent=$1 script=$2
  local base_dir="$GENRELEASES_DIR/nghe-skills-${agent}-${script}"
  echo "Building $agent ($script) package..."
  mkdir -p "$base_dir"
  
  # Copy my-skills folder with agent-specific naming convention
  # Note: my-skills/skills/ contains individual skill folders, each with SKILL.md
  # We copy the contents to match each agent's expected directory structure
  if [[ -d my-skills/skills ]]; then
    case $agent in
      claude)
        # Claude supports both .claude/skills and project root
        mkdir -p "$base_dir/.claude/skills"
        cp -r my-skills/skills/* "$base_dir/.claude/skills/"
        echo "Copied my-skills/skills/* -> .claude/skills/"
        ;;
      gemini)
        mkdir -p "$base_dir/.gemini/skills"
        cp -r my-skills/skills/* "$base_dir/.gemini/skills/"
        echo "Copied my-skills/skills/* -> .gemini/skills/"
        ;;
      copilot)
        # GitHub Copilot Agent Skills standard: .github/skills/
        # Each skill should be a subdirectory with SKILL.md
        mkdir -p "$base_dir/.github/skills"
        cp -r my-skills/skills/* "$base_dir/.github/skills/"
        echo "Copied my-skills/skills/* -> .github/skills/"
        ;;
      cursor-agent)
        mkdir -p "$base_dir/.cursor/skills"
        cp -r my-skills/skills/* "$base_dir/.cursor/skills/"
        echo "Copied my-skills/skills/* -> .cursor/skills/"
        ;;
      qwen)
        mkdir -p "$base_dir/.qwen/skills"
        cp -r my-skills/skills/* "$base_dir/.qwen/skills/"
        echo "Copied my-skills/skills/* -> .qwen/skills/"
        ;;
      opencode)
        mkdir -p "$base_dir/.opencode/skills"
        cp -r my-skills/skills/* "$base_dir/.opencode/skills/"
        echo "Copied my-skills/skills/* -> .opencode/skills/"
        ;;
      windsurf)
        mkdir -p "$base_dir/.windsurf/skills"
        cp -r my-skills/skills/* "$base_dir/.windsurf/skills/"
        echo "Copied my-skills/skills/* -> .windsurf/skills/"
        ;;
      codex)
        mkdir -p "$base_dir/.codex/skills"
        cp -r my-skills/skills/* "$base_dir/.codex/skills/"
        echo "Copied my-skills/skills/* -> .codex/skills/"
        ;;
      kilocode)
        mkdir -p "$base_dir/.kilocode/skills"
        cp -r my-skills/skills/* "$base_dir/.kilocode/skills/"
        echo "Copied my-skills/skills/* -> .kilocode/skills/"
        ;;
      auggie)
        mkdir -p "$base_dir/.augment/skills"
        cp -r my-skills/skills/* "$base_dir/.augment/skills/"
        echo "Copied my-skills/skills/* -> .augment/skills/"
        ;;
      roo)
        mkdir -p "$base_dir/.roo/skills"
        cp -r my-skills/skills/* "$base_dir/.roo/skills/"
        echo "Copied my-skills/skills/* -> .roo/skills/"
        ;;
      codebuddy)
        mkdir -p "$base_dir/.codebuddy/skills"
        cp -r my-skills/skills/* "$base_dir/.codebuddy/skills/"
        echo "Copied my-skills/skills/* -> .codebuddy/skills/"
        ;;
      amp)
        mkdir -p "$base_dir/.agents/skills"
        cp -r my-skills/skills/* "$base_dir/.agents/skills/"
        echo "Copied my-skills/skills/* -> .agents/skills/"
        ;;
      shai)
        mkdir -p "$base_dir/.shai/skills"
        cp -r my-skills/skills/* "$base_dir/.shai/skills/"
        echo "Copied my-skills/skills/* -> .shai/skills/"
        ;;
      q)
        mkdir -p "$base_dir/.amazonq/skills"
        cp -r my-skills/skills/* "$base_dir/.amazonq/skills/"
        echo "Copied my-skills/skills/* -> .amazonq/skills/"
        ;;
      bob)
        mkdir -p "$base_dir/.bob/skills"
        cp -r my-skills/skills/* "$base_dir/.bob/skills/"
        echo "Copied my-skills/skills/* -> .bob/skills/"
        ;;
    esac
  fi
  
  # NOTE: scripts, commands, agents, memory, and templates folders have been removed.
  # The project now focuses on Agent Skills in my-skills/skills/ directory.
  # All required content is already copied above.
  
  ( cd "$base_dir" && zip -r "../nghe-skills-${agent}-${script}-${NEW_VERSION}.zip" . )
  echo "Created $GENRELEASES_DIR/nghe-skills-${agent}-${script}-${NEW_VERSION}.zip"
}

# Determine agent list
ALL_AGENTS=(claude gemini copilot cursor-agent qwen opencode windsurf codex kilocode auggie roo codebuddy amp shai q bob)
ALL_SCRIPTS=(sh ps)

norm_list() {
  # convert comma+space separated -> line separated unique while preserving order of first occurrence
  tr ',\n' '  ' | awk '{for(i=1;i<=NF;i++){if(!seen[$i]++){printf((out?"\n":"") $i);out=1}}}END{printf("\n")}'
}

validate_subset() {
  local type=$1; shift; local -n allowed=$1; shift; local items=("$@")
  local invalid=0
  for it in "${items[@]}"; do
    local found=0
    for a in "${allowed[@]}"; do [[ $it == "$a" ]] && { found=1; break; }; done
    if [[ $found -eq 0 ]]; then
      echo "Error: unknown $type '$it' (allowed: ${allowed[*]})" >&2
      invalid=1
    fi
  done
  return $invalid
}

if [[ -n ${AGENTS:-} ]]; then
  mapfile -t AGENT_LIST < <(printf '%s' "$AGENTS" | norm_list)
  validate_subset agent ALL_AGENTS "${AGENT_LIST[@]}" || exit 1
else
  AGENT_LIST=("${ALL_AGENTS[@]}")
fi

if [[ -n ${SCRIPTS:-} ]]; then
  mapfile -t SCRIPT_LIST < <(printf '%s' "$SCRIPTS" | norm_list)
  validate_subset script ALL_SCRIPTS "${SCRIPT_LIST[@]}" || exit 1
else
  SCRIPT_LIST=("${ALL_SCRIPTS[@]}")
fi

echo "Agents: ${AGENT_LIST[*]}"
echo "Scripts: ${SCRIPT_LIST[*]}"

for agent in "${AGENT_LIST[@]}"; do
  for script in "${SCRIPT_LIST[@]}"; do
    build_variant "$agent" "$script"
  done
done

echo "Archives in $GENRELEASES_DIR:"
ls -1 "$GENRELEASES_DIR"/nghe-skills-*-"${NEW_VERSION}".zip

