# Project Context

Additional context for Copilot and the Compound GPID plugin. Edit freely —
this file is committed to git and shared with the team.

## Data Sources
<!-- Where does data come from? File paths, databases, APIs, vintage conventions -->

## Domain Rules
<!-- Project-specific rules that Copilot should always follow -->
- **Pairing-filtered container startup**: Each `{pipapi}` container serves one `(schema, pipapi_version)` pairing from `pairing.yml`. Startup in `main.R` must pass the pairing's explicit folder list (`read_pairing_vintages()` + `build_vintage_pattern()` or `vintage_folders`) and explicit `schema` (`"new-lineup"` or `"pre-lineup"`) to `create_versioned_lkups()`, never scanning all of `/Data` or relying on the legacy `2025-05-01` date cutoff (`use_new_lineup_version()`).
- **Lookup version synchronization**: In `create_versioned_lkups()`, `lkups$versions` and `names(lkups$versions_paths)` must remain strictly identical after per-vintage `tryCatch()` filtering to satisfy `.cache_v2_prepare_lkups()`.
- **Frozen pairing SHA tagging**: Historical `{pipapi}` commits bound in `pairing.yml` with `.900x` dev versions (e.g., `089c3fc877e003259694fb9b6aef8626a317bb53`) must be git-tagged directly on the frozen commit SHA (`v1.3.22`) rather than creating a new commit that edits `DESCRIPTION`.

## Work in Progress
<!-- Modules, features, or migrations currently underway -->

## Workspace Notes
<!-- Related folders, dependencies on other projects in the VS Code workspace -->

## Wiki Configuration
<!-- folder: wiki -->
<!-- audience: developers | researchers | end-users -->
<!-- tone: technical | conversational | formal -->
