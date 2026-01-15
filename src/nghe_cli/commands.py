"""CLI commands for Nghệ CLI."""

import importlib.metadata
import os
import platform
import shlex
import shutil
import ssl
import sys
from datetime import datetime
from pathlib import Path

import httpx
import typer
from rich.live import Live
from rich.panel import Panel
from rich.table import Table

from .config import AGENT_CONFIG, GITHUB_REPO_NAME, GITHUB_REPO_OWNER
from .github import _github_auth_headers, ssl_context
from .system_utils import check_tool, ensure_executable_scripts, init_git_repo, is_git_repo
from .templates import download_and_extract_template
from .ui import (
    StepTracker,
    app,
    console,
    multi_select_with_arrows,
    select_with_arrows,
    show_banner,
)

# Create HTTP client with SSL context
client = httpx.Client(verify=ssl_context)


@app.command()
def init(
    project_name: str = typer.Argument(None, help="Name for your new project directory (optional if using --here, or use '.' for current directory)"),
    ai_assistant: str = typer.Option(None, "--ai", help="AI agent(s) to use. Can be a single agent or comma-separated list (e.g., 'claude,gemini,copilot'). Valid options: claude, gemini, copilot, cursor-agent, qwen, opencode, codex, windsurf, kilocode, auggie, codebuddy, amp, shai, q, bob. If not specified, an interactive multi-select menu will appear (default: copilot pre-selected)"),
    ignore_agent_tools: bool = typer.Option(False, "--ignore-agent-tools", help="Skip checks for AI agent tools like Claude Code"),
    no_git: bool = typer.Option(False, "--no-git", help="Skip git repository initialization"),
    here: bool = typer.Option(False, "--here", help="Initialize project in the current directory instead of creating a new one"),
    force: bool = typer.Option(False, "--force", help="Force merge/overwrite when using --here (skip confirmation)"),
    upgrade: bool = typer.Option(False, "--upgrade", help="Upgrade existing Nghệ project by replacing agent skills folders with latest versions"),
    skip_tls: bool = typer.Option(False, "--skip-tls", help="Skip SSL/TLS verification (not recommended)"),
    debug: bool = typer.Option(False, "--debug", help="Show verbose diagnostic output for network and extraction failures"),
    github_token: str = typer.Option(None, "--github-token", help="GitHub token to use for API requests (or set GH_TOKEN or GITHUB_TOKEN environment variable)"),
    local_installation: bool = typer.Option(False, "--local-templates", help="Use local templates from repository instead of downloading from GitHub (for development)"),
    nghe_skills_path: str = typer.Option(None, "--template-path", help="Path to local template directory (defaults to repo root if --local-templates is used)"),
):
    """
    Initialize a new Nghệ Skills project from the latest template.

    This command will:
    1. Check that required tools are installed (git is optional)
    2. Let you choose your AI assistant(s) - supports multiple agents
    3. Download the appropriate template(s) from GitHub (or use local templates)
    4. Extract the template to a new project directory or current directory
    5. Initialize a fresh git repository (if not --no-git and no existing repo)
    6. Set up AI assistant commands for all selected agents

    Examples:
        # Basic project initialization (interactive multi-agent selection)
        nghe init my-project

        # Initialize with specific AI assistant
        nghe init my-project --ai claude

        # Initialize with multiple AI assistants (comma-separated)
        nghe init my-project --ai claude,gemini,copilot

        # Initialize in current directory (interactive selection)
        nghe init .
        nghe init --here

        # Force merge into current (non-empty) directory
        nghe init . --force
        nghe init --here --force

        # Upgrade existing project (prompts for AI selection)
        nghe init --upgrade
        nghe init --upgrade --ai claude
        nghe init --here --upgrade

        # Use local skills for development
        nghe init demo --local-templates --ai claude
        nghe init demo --local-templates --template-path /path/to/nghe-skills

        # With environment variables
        export NGHE_USE_LOCAL_INSTALLATION=1
        export NGHE_SKILL_PATH=/path/to/nghe-skills/my-skills
        nghe init demo --ai copilot
    """

    show_banner()

    # Check for environment variable to use local templates
    if not local_installation:
        local_installation_env = os.getenv("NGHE_USE_LOCAL_INSTALLATION", "").lower() in ("1", "true", "yes")
        if local_installation_env:
            local_installation = True
            console.print("[cyan]NGHE_USE_LOCAL_INSTALLATION detected - using local templates[/cyan]")

    # Check for template path from environment
    if nghe_skills_path is None:
        env_nghe_skills_path = os.getenv("NGHE_SKILL_PATH")
        if env_nghe_skills_path:
            nghe_skills_path = env_nghe_skills_path
            console.print(f"[cyan]Using NGHE_SKILL_PATH: {nghe_skills_path}[/cyan]")

    if project_name == ".":
        here = True
        project_name = None  # Clear project_name to use existing validation logic

    if here and project_name:
        console.print("[red]Error:[/red] Cannot specify both project name and --here flag")
        raise typer.Exit(1)

    if not here and not project_name and not upgrade:
        console.print("[red]Error:[/red] Must specify either a project name, use '.' for current directory, or use --here flag")
        raise typer.Exit(1)

    # For upgrade mode without explicit location, default to current directory
    if upgrade and not here and not project_name:
        here = True
        project_name = None

    # Track whether we're merging into an existing directory
    merge_into_existing = False

    if here:
        project_name = Path.cwd().name
        project_path = Path.cwd()

        existing_items = list(project_path.iterdir())
        if existing_items and not upgrade:
            console.print(f"[yellow]Warning:[/yellow] Current directory is not empty ({len(existing_items)} items)")
            console.print("[yellow]Skill files will be merged with existing content and may overwrite existing files[/yellow]")
            if force:
                console.print("[cyan]--force supplied: skipping confirmation and proceeding with merge[/cyan]")
            else:
                response = typer.confirm("Do you want to continue?")
                if not response:
                    console.print("[yellow]Operation cancelled[/yellow]")
                    raise typer.Exit(0)
    else:
        project_path = Path(project_name).resolve()
        if project_path.exists() and not upgrade:
            # Check if Nghệ is already installed in this directory by looking for agent folders
            has_agent_folders = any(
                (project_path / agent_config["folder"]).exists()
                for agent_config in AGENT_CONFIG.values()
            )
            
            if has_agent_folders:
                # Nghệ is installed, suggest upgrade
                error_panel = Panel(
                    f"Directory '[cyan]{project_name}[/cyan]' already exists with Nghe Agent Skills installed\n\n"
                    "To upgrade the existing Nghe-Skills installation, use:\n"
                    f"  [cyan]nghe init {project_name} --upgrade[/cyan]\n\n"
                    "Or choose a different project name.",
                    title="[yellow]Nghe-Skills Already Installed[/yellow]",
                    border_style="yellow",
                    padding=(1, 2)
                )
                console.print()
                console.print(error_panel)
                raise typer.Exit(1)
            else:
                # Directory exists but no Nghệ installation
                error_panel = Panel(
                    f"Directory '[cyan]{project_name}[/cyan]' already exists\n\n"
                    "Nghệ is not installed in this directory. You can:\n"
                    "  • Use [cyan]--force[/cyan] to merge Skill files into the existing directory\n"
                    f"  • Use [cyan]nghe init . --force[/cyan] from within the directory\n"
                    "  • Choose a different project name",
                    title="[yellow]Directory Already Exists[/yellow]",
                    border_style="yellow",
                    padding=(1, 2)
                )
                console.print()
                console.print(error_panel)

                # Ask user if they want to proceed with force
                if not force and sys.stdin.isatty():
                    response = typer.confirm("\nDo you want to proceed and merge Skill files into this directory?")
                    if response:
                        console.print("[cyan]Proceeding with merge into existing directory[/cyan]")
                        merge_into_existing = True
                        # Continue with installation
                    else:
                        console.print("[yellow]Operation cancelled[/yellow]")
                        raise typer.Exit(0)
                else:
                    if force:
                        console.print("[cyan]--force supplied: proceeding with merge into existing directory[/cyan]")
                        merge_into_existing = True
                    else:
                        raise typer.Exit(1)

    current_dir = Path.cwd()

    # Upgrade mode validation and setup
    is_upgrade_mode = False
    backup_paths = {}
    existing_agents = []

    if upgrade:
        # Detect existing agent folders
        for agent_key, agent_config in AGENT_CONFIG.items():
            agent_folder = project_path / agent_config["folder"]
            if agent_folder.exists():
                existing_agents.append((agent_key, agent_config["name"], agent_config["folder"]))
        
        if not existing_agents:
            error_panel = Panel(
                f"No Nghệ Agent Skills found in '[cyan]{project_path}[/cyan]'\n"
                "No agent folders detected. Cannot upgrade.\n\n"
                "Use 'nghe init' without --upgrade to initialize a new project.",
                title="[red]Upgrade Failed[/red]",
                border_style="red",
                padding=(1, 2)
            )
            console.print()
            console.print(error_panel)
            raise typer.Exit(1)

        is_upgrade_mode = True

        # Show upgrade warning
        upgrade_lines = [
            "[yellow]⚠️  Upgrade Mode[/yellow]\n",
            "The following will be [bold red]completely replaced[/bold red]:",
        ]

        if existing_agents:
            upgrade_lines.append("  • Detected agent folders:")
            for _, agent_name, agent_folder in existing_agents:
                upgrade_lines.append(f"    - {agent_folder}")

        upgrade_lines.extend([
            "",
            "[cyan]Backups will be created with timestamp.[/cyan]",
            "[dim]User content (docs/, project files) will be preserved.[/dim]"
        ])

        console.print()
        console.print(Panel("\n".join(upgrade_lines), border_style="yellow", padding=(1, 2)))

        if not force:
            response = typer.confirm("\nDo you want to continue with the upgrade?")
            if not response:
                console.print("[yellow]Upgrade cancelled[/yellow]")
                raise typer.Exit(0)

    setup_lines = [
        f"[cyan]Nghệ Project {'Upgrade' if is_upgrade_mode else 'Setup'}[/cyan]",
        "",
        f"{'Project':<15} [green]{project_path.name}[/green]",
        f"{'Working Path':<15} [dim]{current_dir}[/dim]",
    ]

    if not here or merge_into_existing:
        setup_lines.append(f"{'Target Path':<15} [dim]{project_path}[/dim]")

    if is_upgrade_mode:
        setup_lines.append(f"{'Mode':<15} [yellow]UPGRADE[/yellow]")
    elif merge_into_existing:
        setup_lines.append(f"{'Mode':<15} [yellow]MERGE[/yellow]")

    console.print(Panel("\n".join(setup_lines), border_style="cyan", padding=(1, 2)))

    should_init_git = False
    if not no_git:
        should_init_git = check_tool("git")
        if not should_init_git:
            console.print("[yellow]Git not found - will skip repository initialization[/yellow]")

    if ai_assistant:
        # Parse comma-separated list of AI assistants
        selected_ais = [ai.strip() for ai in ai_assistant.split(',')]

        # Validate each AI assistant
        invalid_ais = [ai for ai in selected_ais if ai not in AGENT_CONFIG]
        if invalid_ais:
            console.print(f"[red]Error:[/red] Invalid AI assistant(s): {', '.join(invalid_ais)}")
            console.print(f"[yellow]Valid options:[/yellow] {', '.join(AGENT_CONFIG.keys())}")
            raise typer.Exit(1)
    else:
        # Create options dict for selection (agent_key: display_name)
        ai_choices = {key: config["name"] for key, config in AGENT_CONFIG.items()}
        selected_ais = multi_select_with_arrows(
            ai_choices,
            "Choose your AI assistants (Space to toggle, Enter to confirm):",
            ["copilot"]
        )

    if not ignore_agent_tools:
        for selected_ai in selected_ais:
            agent_config = AGENT_CONFIG.get(selected_ai)
            if agent_config and agent_config["requires_cli"]:
                install_url = agent_config["install_url"]
                if not check_tool(selected_ai):
                    error_panel = Panel(
                        f"[cyan]{selected_ai}[/cyan] not found\n"
                        f"Install from: [cyan]{install_url}[/cyan]\n"
                        f"{agent_config['name']} is required to continue with this project type.\n\n"
                        "Tip: Use [cyan]--ignore-agent-tools[/cyan] to skip this check",
                        title="[red]Agent Detection Error[/red]",
                        border_style="red",
                        padding=(1, 2)
                    )
                    console.print()
                    console.print(error_panel)
                    raise typer.Exit(1)

    console.print(f"[cyan]Selected AI assistant(s):[/cyan] {', '.join(selected_ais)}")

    tracker = StepTracker("Upgrade Nghệ Project" if is_upgrade_mode else "Initialize Nghệ Project")

    sys._specify_tracker_active = True

    tracker.add("precheck", "Check required tools")
    tracker.complete("precheck", "ok")
    tracker.add("ai-select", "Select AI assistant(s)")
    tracker.complete("ai-select", f"{', '.join(selected_ais)}")

    # Add backup step for upgrade mode
    if is_upgrade_mode:
        tracker.add("backup", "Backup existing files")

    # Add steps for each AI assistant template download (only if not using local templates)
    if not local_installation:
        # Single template download for all agents
        tracker.add("fetch-release", "Fetch latest release")
        tracker.add("download-template", "Download template")
        for selected_ai in selected_ais:
            tracker.add(f"extract-{selected_ai}", f"Setup {selected_ai} skills")
    else:
        # For local templates, simpler steps
        for selected_ai in selected_ais:
            tracker.add(f"copy-{selected_ai}", f"Copy {selected_ai} templates")

    # Add common steps
    for key, label in [
        ("chmod", "Ensure scripts executable"),
        ("cleanup", "Cleanup"),
        ("git", "Initialize git repository"),
        ("final", "Finalize")
    ]:
        tracker.add(key, label)

    # Track git error message outside Live context so it persists
    git_error_message = None

    with Live(tracker.render(), console=console, refresh_per_second=8, transient=True) as live:
        tracker.attach_refresh(lambda: live.update(tracker.render()))
        try:
            verify = not skip_tls
            local_ssl_context = ssl_context if verify else False
            local_client = httpx.Client(verify=local_ssl_context)

            # Perform backup in upgrade mode
            if is_upgrade_mode:
                tracker.start("backup")
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

                # Backup existing agent folders
                for agent_key, agent_name, agent_folder in existing_agents:
                    source_folder = project_path / agent_folder
                    if source_folder.exists():
                        # Remove trailing slash for backup name
                        folder_name = agent_folder.rstrip('/')
                        backup_folder = project_path / f"{folder_name}.backup.{timestamp}"
                        shutil.copytree(source_folder, backup_folder)
                        backup_paths[agent_folder] = backup_folder

                backup_count = len(backup_paths)
                tracker.complete("backup", f"{backup_count} folder{'s' if backup_count != 1 else ''} backed up")

            # Download and extract template once, then copy to each AI agent folder
            zip_path_to_cleanup = None
            if not local_installation:
                # Download template once
                from .github import download_template_from_github
                
                tracker.start("fetch-release", "contacting GitHub API")
                try:
                    current_dir = Path.cwd()
                    zip_path, meta = download_template_from_github(
                        current_dir,
                        verbose=False,
                        show_progress=False,
                        client=local_client,
                        debug=debug,
                        github_token=github_token
                    )
                    zip_path_to_cleanup = zip_path  # Save for cleanup later
                    tracker.complete("fetch-release", f"release {meta['release']}")
                    tracker.add("download-template", "Download template")
                    tracker.complete("download-template", f"{meta['size']:,} bytes")
                except Exception as e:
                    tracker.error("fetch-release", str(e))
                    raise
                
                # Extract to each agent folder
                for selected_ai in selected_ais:
                    download_and_extract_template(
                        project_path, selected_ai, here or merge_into_existing,
                        verbose=False, tracker=tracker, client=local_client,
                        debug=debug, github_token=github_token,
                        local_installation=False, nghe_skills_path=None,
                        is_first_agent=False, downloaded_zip_path=zip_path
                    )
                
                # Cleanup downloaded ZIP after all agents processed
                if zip_path_to_cleanup and zip_path_to_cleanup.exists():
                    tracker.start("cleanup")
                    zip_path_to_cleanup.unlink()
                    tracker.complete("cleanup", "removed archive")
            else:
                # Local installation - copy for each agent
                for selected_ai in selected_ais:
                    download_and_extract_template(
                        project_path, selected_ai, here or merge_into_existing,
                        verbose=False, tracker=tracker, client=local_client,
                        debug=debug, github_token=github_token,
                        local_installation=True, nghe_skills_path=nghe_skills_path,
                        is_first_agent=False
                    )

            ensure_executable_scripts(project_path, tracker=tracker)

            if not no_git:
                tracker.start("git")
                if is_git_repo(project_path):
                    tracker.complete("git", "existing repo detected")
                elif should_init_git:
                    success, error_msg = init_git_repo(project_path, quiet=True)
                    if success:
                        tracker.complete("git", "initialized")
                    else:
                        tracker.error("git", "init failed")
                        git_error_message = error_msg
                else:
                    tracker.skip("git", "git not available")
            else:
                tracker.skip("git", "--no-git flag")

            tracker.complete("final", "project ready")
        except Exception as e:
            tracker.error("final", str(e))
            console.print(Panel(f"Initialization failed: {e}", title="Failure", border_style="red"))
            if debug:
                _env_pairs = [
                    ("Python", sys.version.split()[0]),
                    ("Platform", sys.platform),
                    ("CWD", str(Path.cwd())),
                ]
                _label_width = max(len(k) for k, _ in _env_pairs)
                env_lines = [f"{k.ljust(_label_width)} → [bright_black]{v}[/bright_black]" for k, v in _env_pairs]
                console.print(Panel("\n".join(env_lines), title="Debug Environment", border_style="magenta"))
            if not here and project_path.exists():
                shutil.rmtree(project_path)
            raise typer.Exit(1)
        finally:
            pass

    console.print(tracker.render())
    console.print("\n[bold green]Project ready.[/bold green]")

    # Show backup information for upgrade mode
    if is_upgrade_mode and backup_paths:
        console.print()
        backup_lines = ["[cyan]Backups created:[/cyan]"]
        for original, backup in backup_paths.items():
            backup_lines.append(f"  • {original} → {backup.name}")
        backup_lines.append("\n[dim]To restore from backup, remove the current folder and rename the backup.[/dim]")
        backup_panel = Panel(
            "\n".join(backup_lines),
            title="[cyan]Backup Information[/cyan]",
            border_style="cyan",
            padding=(1, 2)
        )
        console.print(backup_panel)

    # Show git error details if initialization failed
    if git_error_message:
        console.print()
        git_error_panel = Panel(
            f"[yellow]Warning:[/yellow] Git repository initialization failed\n\n"
            f"{git_error_message}\n\n"
            f"[dim]You can initialize git manually later with:[/dim]\n"
            f"[cyan]cd {project_path if not here else '.'}[/cyan]\n"
            f"[cyan]git init[/cyan]\n"
            f"[cyan]git add .[/cyan]\n"
            f"[cyan]git commit -m \"Initial commit\"[/cyan]",
            title="[red]Git Initialization Failed[/red]",
            border_style="red",
            padding=(1, 2)
        )
        console.print(git_error_panel)

    # Agent folder security notice
    agent_config = AGENT_CONFIG.get(selected_ais[-1])  # Use last selected AI for the notice
    if agent_config:
        agent_folder = agent_config["folder"]
        security_notice = Panel(
            f"Some agents may store credentials, auth tokens, or other identifying and private artifacts in the agent folder within your project.\n"
            f"Consider adding [cyan]{agent_folder}[/cyan] (or parts of it) to [cyan].gitignore[/cyan] to prevent accidental credential leakage.",
            title="[yellow]Agent Folder Security[/yellow]",
            border_style="yellow",
            padding=(1, 2)
        )
        console.print()
        console.print(security_notice)

    # Agent Skills enablement reminder
    enable_lines = [
        "[bold yellow]⚠️  Important Configuration[/bold yellow]",
        "",
        "Make sure [bold]Agent Skills[/bold] (or Custom Instructions/Skills) are enabled in your AI assistant settings:",
        "",
        "  • [cyan]GitHub Copilot:[/cyan] Settings → Copilot → Enable 'Agent Skills' or 'Custom Instructions'",
        "  • [cyan]Claude:[/cyan] Check project settings to ensure skills are recognized",
        "  • [cyan]Cursor:[/cyan] Settings → Features → Enable 'Agent Skills' or '.cursorrules'",
        "  • [cyan]Other agents:[/cyan] Check your agent's documentation for skill configuration",
        "",
        "[dim]Without this enabled, the skills won't be activated automatically.[/dim]"
    ]
    enable_panel = Panel("\n".join(enable_lines), title="Enable Agent Skills", border_style="yellow", padding=(1,2))
    console.print()
    console.print(enable_panel)

    steps_lines = []
    if not here and not merge_into_existing:
        steps_lines.append(f"1. Go to the project folder: [cyan]cd {project_name}[/cyan]")
        step_num = 2
    else:
        if here:
            steps_lines.append("1. You're already in the project directory!")
        else:
            steps_lines.append(f"1. Go to the project folder: [cyan]cd {project_name}[/cyan]")
        step_num = 2

    # Add Codex-specific setup step if needed
    if "codex" in selected_ais:
        codex_path = project_path / ".codex"
        quoted_path = shlex.quote(str(codex_path))
        if os.name == "nt":  # Windows
            cmd = f"setx CODEX_HOME {quoted_path}"
        else:  # Unix-like systems
            cmd = f"export CODEX_HOME={quoted_path}"

        steps_lines.append(f"{step_num}. Set [cyan]CODEX_HOME[/cyan] environment variable before running Codex: [cyan]{cmd}[/cyan]")
        step_num += 1

    steps_lines.append(f"{step_num}. Start using the installed skills with your AI agent:")
    steps_lines.append("")
    steps_lines.append("   [bold cyan]Available Skills (39 total):[/bold cyan]")
    steps_lines.append("")
    steps_lines.append("   [cyan]Cloud Platforms (6):[/cyan] AWS, Azure, GCP, Alibaba, IBM, Oracle Cloud")
    steps_lines.append("   [cyan]Development (7):[/cyan] Backend, Frontend, Database, DevOps, Refactoring")
    steps_lines.append("   [cyan]Review & Quality (8):[/cyan] Code quality, Security, Architecture reviews")
    steps_lines.append("   [cyan]Requirements (3):[/cyan] Gathering, Review, Architecture design")
    steps_lines.append("   [cyan]Migration & Legacy (10):[/cyan] COBOL, JCL, PL/I, RPG analyzers, Mainframes")
    steps_lines.append("   [cyan]Testing (1):[/cyan] Integration testing strategies")
    steps_lines.append("   [cyan]Specialized (4):[/cyan] Bug analysis, Git commits, Technical writing, KeyCloak")
    steps_lines.append("")
    steps_lines.append("   [dim]Skills are automatically activated based on your requests and file context.[/dim]")

    steps_panel = Panel("\n".join(steps_lines), title="Next Steps", border_style="cyan", padding=(1,2))
    console.print()
    console.print(steps_panel)

    usage_lines = [
        "The skills will automatically activate when you:",
        "",
        "  • Mention specific technologies (AWS, React, PostgreSQL, etc.)",
        "  • Request development activities (design API, review code, etc.)",
        "  • Work with certain file types (.cobol, .jcl, .rpg, etc.)",
        "  • Ask about cloud platforms, migrations, or best practices",
        "",
        "[dim]Example: \"Design a REST API for user management\" will activate backend-design skill[/dim]",
        "[dim]Example: \"Review this React component\" will activate frontend-code-review skill[/dim]"
    ]
    usage_panel = Panel("\n".join(usage_lines), title="How to Use Skills", border_style="cyan", padding=(1,2))
    console.print()
    console.print(usage_panel)


@app.command()
def check():
    """Check that all required tools are installed."""
    show_banner()
    console.print("[bold]Checking for installed tools...[/bold]\n")

    tracker = StepTracker("Check Available Tools")

    tracker.add("git", "Git version control")
    git_ok = check_tool("git", tracker=tracker)

    agent_results = {}
    for agent_key, agent_config in AGENT_CONFIG.items():
        agent_name = agent_config["name"]
        requires_cli = agent_config["requires_cli"]

        tracker.add(agent_key, agent_name)

        if requires_cli:
            agent_results[agent_key] = check_tool(agent_key, tracker=tracker)
        else:
            # IDE-based agent - skip CLI check and mark as optional
            tracker.skip(agent_key, "IDE-based, no CLI check")
            agent_results[agent_key] = False  # Don't count IDE agents as "found"

    # Check VS Code variants (not in agent config)
    tracker.add("code", "Visual Studio Code")
    code_ok = check_tool("code", tracker=tracker)

    tracker.add("code-insiders", "Visual Studio Code Insiders")
    code_insiders_ok = check_tool("code-insiders", tracker=tracker)

    console.print(tracker.render())

    console.print("\n[bold green]Nghệ CLI is ready to use![/bold green]")

    if not git_ok:
        console.print("[dim]Tip: Install git for repository management[/dim]")

    if not any(agent_results.values()):
        console.print("[dim]Tip: Install an AI assistant for the best experience[/dim]")


@app.command()
def version():
    """Display version and system information."""

    show_banner()

    # Get CLI version from package metadata
    cli_version = "unknown"
    try:
        cli_version = importlib.metadata.version("nghe-cli")
    except Exception:
        # Fallback: try reading from pyproject.toml if running from source
        try:
            import tomllib
            pyproject_path = Path(__file__).parent.parent.parent / "pyproject.toml"
            if pyproject_path.exists():
                with open(pyproject_path, "rb") as f:
                    data = tomllib.load(f)
                    cli_version = data.get("project", {}).get("version", "unknown")
        except Exception:
            pass

    # Fetch latest template release version
    api_url = f"https://api.github.com/repos/{GITHUB_REPO_OWNER}/{GITHUB_REPO_NAME}/releases/latest"

    template_version = "unknown"
    release_date = "unknown"

    try:
        response = client.get(
            api_url,
            timeout=10,
            follow_redirects=True,
            headers=_github_auth_headers(),
        )
        if response.status_code == 200:
            release_data = response.json()
            template_version = release_data.get("tag_name", "unknown")
            # Remove 'v' prefix if present
            if template_version.startswith("v"):
                template_version = template_version[1:]
            release_date = release_data.get("published_at", "unknown")
            if release_date != "unknown":
                # Format the date nicely
                try:
                    dt = datetime.fromisoformat(release_date.replace('Z', '+00:00'))
                    release_date = dt.strftime("%Y-%m-%d")
                except Exception:
                    pass
    except Exception:
        pass

    info_table = Table(show_header=False, box=None, padding=(0, 2))
    info_table.add_column("Key", style="cyan", justify="right")
    info_table.add_column("Value", style="white")

    info_table.add_row("CLI Version", cli_version)
    info_table.add_row("Template Version", template_version)
    info_table.add_row("Released", release_date)
    info_table.add_row("", "")
    info_table.add_row("Python", platform.python_version())
    info_table.add_row("Platform", platform.system())
    info_table.add_row("Architecture", platform.machine())
    info_table.add_row("OS Version", platform.version())

    panel = Panel(
        info_table,
        title="[bold cyan]Nghệ CLI Information[/bold cyan]",
        border_style="cyan",
        padding=(1, 2)
    )

    console.print(panel)
    console.print()
