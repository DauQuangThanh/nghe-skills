"""Agent Skills operations for Nghệ CLI."""

import json
import shutil
import tempfile
import zipfile
from pathlib import Path
from typing import TYPE_CHECKING

import httpx

from .config import AGENT_CONFIG
from .github import download_template_from_github
from .ui import console

if TYPE_CHECKING:
    from .ui import StepTracker


def handle_vscode_settings(sub_item, dest_file, rel_path, verbose=False, tracker=None) -> None:
    """Handle merging or copying of .vscode/settings.json files."""
    def log(message, color="green"):
        if verbose and not tracker:
            console.print(f"[{color}]{message}[/] {rel_path}")

    try:
        with open(sub_item, 'r', encoding='utf-8') as f:
            new_settings = json.load(f)

        if dest_file.exists():
            merged = merge_json_files(dest_file, new_settings, verbose=verbose and not tracker)
            with open(dest_file, 'w', encoding='utf-8') as f:
                json.dump(merged, f, indent=4)
                f.write('\n')
            log("Merged:", "green")
        else:
            shutil.copy2(sub_item, dest_file)
            log("Copied (no existing settings.json):", "blue")

    except Exception as e:
        log(f"Warning: Could not merge, copying instead: {e}", "yellow")
        shutil.copy2(sub_item, dest_file)


def merge_json_files(existing_path: Path, new_content: dict, verbose: bool = False) -> dict:
    """Merge new JSON content into existing JSON file.

    Performs a deep merge where:
    - New keys are added
    - Existing keys are preserved unless overwritten by new content
    - Nested dictionaries are merged recursively
    - Lists and other values are replaced (not merged)

    Args:
        existing_path: Path to existing JSON file
        new_content: New JSON content to merge in
        verbose: Whether to print merge details

    Returns:
        Merged JSON content as dict
    """
    try:
        with open(existing_path, 'r', encoding='utf-8') as f:
            existing_content = json.load(f)
    except (FileNotFoundError, json.JSONDecodeError):
        # If file doesn't exist or is invalid, just use new content
        return new_content

    def deep_merge(base: dict, update: dict) -> dict:
        """Recursively merge update dict into base dict."""
        result = base.copy()
        for key, value in update.items():
            if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                # Recursively merge nested dictionaries
                result[key] = deep_merge(result[key], value)
            else:
                # Add new key or replace existing value
                result[key] = value
        return result

    merged = deep_merge(existing_content, new_content)

    if verbose:
        console.print(f"[cyan]Merged JSON file:[/cyan] {existing_path.name}")

    return merged


def copy_local_template(
    project_path: Path,
    source_path: Path,
    ai_assistant: str,
    is_current_dir: bool = False,
    verbose: bool = True,
    tracker: "StepTracker | None" = None,
    is_first_agent: bool = True
) -> Path:
    """Copy local Agent Skills to the project directory.

    Args:
        is_first_agent: Currently unused (legacy parameter for backward compatibility).
                       All agents now get their own skills folder.
    """

    # Path to skills directory (my-skills/skills/)
    skills_dir = source_path / "my-skills" / "skills"

    # Check if skills directory exists
    if not skills_dir.exists():
        raise FileNotFoundError(f"Skills directory not found: {skills_dir}")

    if verbose and not tracker:
        console.print(f"[cyan]Copying Agent Skills from:[/cyan] {skills_dir}")

    # Create project directory if needed
    if not is_current_dir:
        project_path.mkdir(parents=True, exist_ok=True)

    # Determine target structure based on AI assistant
    agent_config = AGENT_CONFIG.get(ai_assistant)
    if not agent_config:
        raise ValueError(f"Unknown AI assistant: {ai_assistant}")

    agent_folder = agent_config["folder"]

    # Create agent-specific skills directory (e.g., .github/skills/, .claude/skills/)
    agent_skills_path = project_path / agent_folder / "skills"
    agent_skills_path.mkdir(parents=True, exist_ok=True)

    # Copy all skill directories from my-skills/skills/ to agent's skills folder
    skill_count = 0
    for skill_dir in skills_dir.iterdir():
        if skill_dir.is_dir():
            dest_skill_dir = agent_skills_path / skill_dir.name
            if dest_skill_dir.exists():
                shutil.rmtree(dest_skill_dir)
            shutil.copytree(skill_dir, dest_skill_dir)
            skill_count += 1

    if verbose and not tracker:
        console.print(f"[green]✓[/green] Copied {skill_count} Agent Skills to {agent_folder}skills/")

    if tracker:
        tracker.complete(f"copy-{ai_assistant}", f"{skill_count} skills copied")

    return project_path


def download_and_extract_template(
    project_path: Path,
    ai_assistant: str,
    is_current_dir: bool = False,
    *,
    verbose: bool = True,
    tracker: "StepTracker | None" = None,
    client: httpx.Client = None,
    debug: bool = False,
    github_token: str = None,
    local_installation: bool = False,
    nghe_skills_path: str = None,
    is_first_agent: bool = True,
    downloaded_zip_path: Path = None
) -> Path:
    """Download the latest agent skills release and set up for a new project.
    Returns project_path. Uses tracker if provided (with keys: fetch, download, extract, cleanup)
    If local_installation is True, copies agent skills from local nghe_skills_path instead of downloading.
    If downloaded_zip_path is provided, uses that instead of downloading again.

    Args:
        is_first_agent: Currently unused (legacy parameter for backward compatibility).
        downloaded_zip_path: Optional pre-downloaded agent skills ZIP file path to reuse.
    """
    current_dir = Path.cwd()

    # Handle local source
    if local_installation:
        if tracker:
            tracker.start(f"copy-{ai_assistant}")

        # Determine template source path
        if nghe_skills_path:
            source_path = Path(nghe_skills_path).resolve()
        else:
            # Default to repo root (assume we're in src/nghe_cli)
            source_path = Path(__file__).parent.parent.parent.resolve()

        if not source_path.exists():
            error_msg = f"Agent skills path does not exist: {source_path}"
            if tracker:
                tracker.error(f"copy-{ai_assistant}", error_msg)
            raise FileNotFoundError(error_msg)

        if verbose and not tracker:
            console.print(f"[cyan]Using local source from:[/cyan] {source_path}")

        # Copy agent skills with structure similar to release package
        return copy_local_template(project_path, source_path, ai_assistant, is_current_dir, verbose, tracker, is_first_agent)

    # Use provided ZIP path or download new one
    if downloaded_zip_path and downloaded_zip_path.exists():
        # Reuse downloaded agent skills
        zip_path = downloaded_zip_path
        if tracker and verbose:
            console.print(f"[cyan]Reusing downloaded agent skills for {ai_assistant}[/cyan]")
    else:
        # Download agent skills (should not happen if called correctly from commands.py)
        if tracker:
            tracker.start(f"fetch-{ai_assistant}", "contacting GitHub API")
        try:
            zip_path, meta = download_template_from_github(
                current_dir,
                verbose=verbose and tracker is None,
                show_progress=(tracker is None),
                client=client,
                debug=debug,
                github_token=github_token
            )
            if tracker:
                tracker.complete(f"fetch-{ai_assistant}", f"release {meta['release']} ({meta['size']:,} bytes)")
                tracker.add(f"download-{ai_assistant}", "Download agent skills")
                tracker.complete(f"download-{ai_assistant}", meta['filename'])
        except Exception as e:
            if tracker:
                tracker.error(f"fetch-{ai_assistant}", str(e))
            else:
                if verbose:
                    console.print(f"[red]Error downloading agent skills:[/red] {e}")
            raise
        raise

    if tracker:
        tracker.add(f"extract-{ai_assistant}", "Extract agent skills")
        tracker.start(f"extract-{ai_assistant}")
    elif verbose:
        console.print("Extracting agent skills...")

    try:
        if not is_current_dir:
            project_path.mkdir(parents=True, exist_ok=True)

        # Determine target agent-specific folder
        agent_config = AGENT_CONFIG.get(ai_assistant)
        if not agent_config:
            raise ValueError(f"Unknown AI assistant: {ai_assistant}")
        
        agent_folder = agent_config["folder"]
        agent_skills_path = project_path / agent_folder / "skills"

        with zipfile.ZipFile(zip_path, 'r') as zip_ref:
            zip_contents = zip_ref.namelist()
            if tracker:
                tracker.start("zip-list")
                tracker.complete("zip-list", f"{len(zip_contents)} entries")
            elif verbose:
                console.print(f"[cyan]ZIP contains {len(zip_contents)} items[/cyan]")

            # Extract to temp directory and copy skills to agent-specific folder
            with tempfile.TemporaryDirectory() as temp_dir:
                temp_path = Path(temp_dir)
                zip_ref.extractall(temp_path)

                extracted_items = list(temp_path.iterdir())
                if tracker:
                    tracker.start("extracted-summary")
                    tracker.complete("extracted-summary", f"temp {len(extracted_items)} items")
                elif verbose:
                    console.print(f"[cyan]Extracted {len(extracted_items)} items to temp location[/cyan]")

                # Handle nested directory structure
                source_dir = temp_path
                if len(extracted_items) == 1 and extracted_items[0].is_dir():
                    source_dir = extracted_items[0]
                    if tracker:
                        tracker.add("flatten", "Flatten nested directory")
                        tracker.complete("flatten")
                    elif verbose:
                        console.print(f"[cyan]Found nested directory structure[/cyan]")

                # Look for 'skills' directory in the extracted content
                skills_source = source_dir / "skills"
                if not skills_source.exists():
                    # If no 'skills' subdirectory, assume all directories are skills
                    skills_source = source_dir

                # Create agent-specific skills directory
                agent_skills_path.mkdir(parents=True, exist_ok=True)

                # Copy skill directories to agent-specific skills folder
                skill_count = 0
                for item in skills_source.iterdir():
                    if item.is_dir() and not item.name.startswith('.'):
                        # This is a skill directory
                        dest_skill_dir = agent_skills_path / item.name
                        if dest_skill_dir.exists():
                            if is_current_dir and verbose and not tracker:
                                console.print(f"[yellow]Merging skill:[/yellow] {item.name}")
                            shutil.rmtree(dest_skill_dir)
                        shutil.copytree(item, dest_skill_dir)
                        skill_count += 1

                if verbose and not tracker:
                    console.print(f"[green]✓[/green] Copied {skill_count} skills to {agent_folder}skills/")
                elif tracker:
                    if verbose:
                        console.print(f"[cyan]Copied {skill_count} skills to {agent_folder}skills/[/cyan]")

    except Exception as e:
        if tracker:
            tracker.error(f"extract-{ai_assistant}", str(e))
        else:
            if verbose:
                console.print(f"[red]Error extracting template:[/red] {e}")
                if debug:
                    from rich.panel import Panel
                    console.print(Panel(str(e), title="Extraction Error", border_style="red"))

        if not is_current_dir and project_path.exists():
            shutil.rmtree(project_path)
        # Re-raise the original exception instead of typer.Exit to preserve error details
        raise
    else:
        if tracker:
            tracker.complete(f"extract-{ai_assistant}", f"{skill_count} skills")
    finally:
        # Only cleanup ZIP if we downloaded it (not if it was provided to reuse)
        if not downloaded_zip_path and zip_path and zip_path.exists():
            if tracker:
                tracker.add("cleanup", "Remove temporary archive")
            zip_path.unlink()
            if tracker:
                tracker.complete("cleanup")
            elif verbose:
                console.print(f"Cleaned up: {zip_path.name}")

    return project_path
