"""Configuration constants and agent definitions for Nghệ CLI."""

from pathlib import Path

# Agent configuration with name, folder, install URL, and CLI tool requirement
# The 'folder' field contains the complete path including subdirectory (e.g., .github/skills/, .cursor/rules/)
AGENT_CONFIG = {
    "copilot": {
        "name": "GitHub Copilot",
        "folder": ".github/skills/",
        "install_url": None,
        "requires_cli": False,
    },
    "claude": {
        "name": "Claude Code",
        "folder": ".claude/skills/",
        "install_url": "https://docs.anthropic.com/en/docs/claude-code/setup",
        "requires_cli": True,
    },
    "gemini": {
        "name": "Gemini CLI",
        "folder": ".gemini/extensions/",
        "install_url": "https://github.com/google-gemini/gemini-cli",
        "requires_cli": True,
    },
    "cursor-agent": {
        "name": "Cursor",
        "folder": ".cursor/rules/",
        "install_url": None,
        "requires_cli": False,
    },
    "qwen": {
        "name": "Qwen Code",
        "folder": ".qwen/skills/",
        "install_url": "https://github.com/QwenLM/qwen-code",
        "requires_cli": True,
    },
    "opencode": {
        "name": "opencode",
        "folder": ".opencode/skill/",
        "install_url": "https://opencode.ai",
        "requires_cli": True,
    },
    "codex": {
        "name": "Codex CLI",
        "folder": ".codex/skills/",
        "install_url": "https://github.com/openai/codex",
        "requires_cli": True,
    },
    "windsurf": {
        "name": "Windsurf",
        "folder": ".windsurf/skills/",
        "install_url": None,
        "requires_cli": False,
    },
    "kilocode": {
        "name": "Kilo Code",
        "folder": ".kilocode/skills/",
        "install_url": None,
        "requires_cli": False,
    },
    "auggie": {
        "name": "Auggie CLI",
        "folder": ".augment/rules/",
        "install_url": "https://docs.augmentcode.com/cli/setup-auggie/install-auggie-cli",
        "requires_cli": True,
    },
    "codebuddy": {
        "name": "CodeBuddy",
        "folder": ".codebuddy/skills/",
        "install_url": "https://www.codebuddy.ai/cli",
        "requires_cli": True,
    },
    "roo": {
        "name": "Roo Code",
        "folder": ".roo/skills/",
        "install_url": None,
        "requires_cli": False,
    },
    "q": {
        "name": "Amazon Q Developer CLI",
        "folder": ".amazonq/cli-agents/",
        "install_url": "https://aws.amazon.com/developer/learning/q-developer-cli/",
        "requires_cli": True,
    },
    "amp": {
        "name": "Amp",
        "folder": ".agents/skills/",
        "install_url": "https://ampcode.com/manual#install",
        "requires_cli": True,
    },
    "shai": {
        "name": "SHAI",
        "folder": ".shai/commands/",
        "install_url": "https://github.com/ovh/shai",
        "requires_cli": True,
    },
    "bob": {
        "name": "IBM Bob",
        "folder": ".bob/skills/",
        "install_url": None,
        "requires_cli": False,
    },
    "antigravity": {
        "name": "Google Antigravity",
        "folder": ".agent/skills/",
        "install_url": "https://antigravity.google/docs/skills",
        "requires_cli": False,
    },
    "jules": {
        "name": "Jules",
        "folder": "skills/",
        "install_url": "https://www.jules.ai",
        "requires_cli": False,
    },
    "qoder": {
        "name": "Qoder CLI",
        "folder": ".qoder/skills/",
        "install_url": "https://qoder.dev",
        "requires_cli": True,
    },
}

BANNER = """
███╗   ██╗ ██████╗ ██╗  ██╗███████╗    ███████╗██╗  ██╗██╗██╗     ██╗     ███████╗
████╗  ██║██╔════╝ ██║  ██║██╔════╝    ██╔════╝██║ ██╔╝██║██║     ██║     ██╔════╝
██╔██╗ ██║██║  ███╗███████║█████╗      ███████╗█████╔╝ ██║██║     ██║     ███████╗
██║╚██╗██║██║   ██║██╔══██║██╔══╝      ╚════██║██╔═██╗ ██║██║     ██║     ╚════██║
██║ ╚████║╚██████╔╝██║  ██║███████╗    ███████║██║  ██╗██║███████╗███████╗███████║
╚═╝  ╚═══╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝    ╚══════╝╚═╝  ╚═╝╚═╝╚══════╝╚══════╝╚══════╝
"""

TAGLINE = "Nghệ Skills - Drive Quality Together with Agent Skills"

# GitHub repository information
GITHUB_REPO_OWNER = "dauquangthanh"
GITHUB_REPO_NAME = "nghe-skills"
