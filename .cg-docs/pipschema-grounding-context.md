# {pipschema} - Grounding Context (source of truth)

**Audience:** the AI assistant (and humans) building the `{pipschema}` R package.
**Written:** 2026-10-05, from the design work on the PIP API (`{pipapi}`) and the meeting with ITS.
**How to use:** copy this file into the `{pipschema}` workspace and treat it as the starting point for *why* the package exists and *where it fits*. It deliberately does **not** define the schemas or their rules. That is the first piece of work to do inside `{pipschema}`, separately. Items marked **[OPEN]** are not decided: flag them, don't invent answers.

---

## 1. Purpose in one paragraph

The PIP API (`{pipapi}`) serves poverty and inequality numbers computed from dated data snapshots called **vintage folders**, which live in one data directory (`/Data`). The data has changed structure over time, and the API needs to know which structure each vintage has. Today that knowledge is implicit and hard-coded inside `{pipapi}`. `{pipschema}` is a new R package whose job is to make it **explicit and central**: it defines what a **schema** is, holds the logical rules that decide which schema each vintage folder has, and publishes the result as one central file. It is built for the PIP API team (DECDG / GPID, World Bank) and its deployment workflow with ITS.

**The schema definition and its rules are the heart of the package.** Scanning folders, validating and writing the file exist to apply and publish those rules.

---

## 2. The larger project it belongs to

**The feature:** *code-data versioning for the PIP API.* Users should be able to pick a **PIP release** (a date, for example `20260922`) and get results from a specific, reproducible pairing of **pipapi code + data**. Reproducibility is the top priority: published numbers must be reproducible later.

**The problem it addresses:**
- Code and data are tightly coupled, but nothing formally records or enforces that coupling.
- The API's structure knowledge is a hard-coded guess (based on dates in folder names) spread across the code, which makes the code harder to maintain.
- The deployed API installs a floating, unpinned branch of the code, so "version X" doesn't mean a stable code + data pairing.
- One incompatible vintage folder can break the API's startup.

**The agreed direction (decided with ITS and the team):**
- Data stays in **one flat `/Data` folder** of vintage folders, copied from Azure Blob storage to the VMs, and mounted into the containers.
- There will be **one container per pairing** (a schema plus a specific `{pipapi}` version), each with its own Dockerfile (pins the `{pipapi}` version) and `main.R` (declares the schema).
- Each container loads **only the vintage folders that belong to its schema**.
- A **router** sends a request for a given PIP release to the right container. The routing mechanism is still to be confirmed with ITS **[OPEN]**.
- An earlier idea, putting a manifest file inside every vintage folder, was **dropped** because editing hundreds of folders one by one is impractical. A single, centrally maintained file replaces it. **That file is what `{pipschema}` produces.**

```
ITS data pipeline --writes--> /Data (flat folder of vintage folders)
                                    |
                         {pipschema} reads them (read-only)
                                    |
                    central schema file (stored with /Data)
                                    |
      used by {pipapi} deployments: each container loads only
      the vintage folders of its own schema
```

---

## 3. What `{pipschema}` is responsible for

1. **Defining schemas**: a named schema plus the explicit rules that identify it.
2. **Classifying** every vintage folder in `/Data` against those rules.
3. **Validating** the classification (see the relationship rules below).
4. **Publishing** one central schema file: all schemas, their rules, and the vintage folders in each.

**Later, not in the first version [OPEN]:** a `pairing` file that links PIP releases to schemas and to `{pipapi}` versions. It's expected to be append-only (once a release is bound, the binding never changes, for reproducibility). Who writes it, when, and where it lives are undecided. The schema file should not make that harder later.

**Outside `{pipschema}`'s scope:** routing, Docker, ITS pipelines, and any change to `{pipapi}`. `{pipschema}` only *reads* vintage folders and only *writes* its own output file(s). It never modifies a vintage folder.

---

## 4. Glossary (authoritative)

| Term | Meaning | Example |
|---|---|---|
| **Vintage folder** | A dated snapshot of PIP data, one folder directly under `/Data`. | `20250601_2025_01_02_PROD` |
| **Release date** | The first 8 digits of a vintage folder name (`YYYYMMDD`). | `20250601` |
| **PIP release** | The date a user chooses in the API. Working hypothesis **[OPEN]**: equal to the release-date prefix of vintage folder names. | `20260922` |
| **Schema** | A named description of the structure and format of a vintage folder, identified by explicit rules. See section 4a. The set of schemas is **not fixed in advance**: it comes out of applying the rules to the real data. | *(names to be decided)* |
| **`{pipapi}` version** | A git tag of the pipapi R package. | `v1.6.0` |
| **Pairing** | A (schema, `{pipapi}` version) combination. One pairing = one Docker image = one container. | `schema-a__v1.6.0` *(illustrative names only)* |
| **Schema file** | The one central file `{pipschema}` writes. Name and location **[OPEN]**. | |

## 4a. What a schema is

> A **schema** is a named description of the structure and format a vintage folder has, defined by explicit rules about its contents, so that vintage folders with the same schema have the same structure and can be handled in the same way. `{pipapi}` is the main consumer, so what it relies on weighs heavily, but the rules are not limited to what it reads today.

**The principle: schemas describe the data's structure, with `{pipapi}` as the main (but not the only) reference.** `{pipapi}` is the main consumer of the schemas, so what it reads is very important and relevant, and it is the first evidence for which structural differences matter. But the schema rules must **not be limited to what `{pipapi}` consumes today**. This is the reason `{pipschema}` is a separate package: to identify and record the structure of the data properly, so the definitions stay valid when `{pipapi}` changes. If a future `{pipapi}` version starts using files or sub-folders it ignores today, the rules should not be unprepared for it.

This does **not** mean schemas should be very fine-grained. The aim is a sensible, reasonably coarse set of schemas, whose rules are allowed to look at the structure of the folder beyond what `{pipapi}` happens to read now. Whether a given difference defines a new schema is a judgement to make case by case during the design work.

**One container serves exactly one schema** (see section 2). That stays unchanged.

**Shape, not content.** A schema describes the *shape* of a vintage folder: which files and sub-folders exist, what format they are in, and which tables and columns they hold. It does not describe the *content*, meaning which countries, years or estimates are inside. A new vintage with updated numbers and the same shape has the same schema as the previous one.

**Identified by rules.** A schema is identified by explicit, checkable statements about a folder's contents. Any vintage folder can be tested against them, and a person can read the rules to see why a folder was assigned to a schema. A schema is **not** defined by the folder's name or date. A date may hint at when a structure changed, but the structure is what counts.

**Not fixed in advance, and not tied to any one change.** Schemas are the distinct groups of vintage folders that the rules actually produce on the real data. The lineup change in `{pipapi}` (see section 6) is **one known example** of a structural difference, not the definition of a schema. There may be other boundaries (for example file format switches or changed columns), and one known change may even split into several. This has to be found in the data, not assumed.

**Kinds of difference worth considering** (a checklist for the design work, not a list of answers):
- a file or folder that exists in one structure but not in another
- a file whose name changed
- a file whose format changed
- a table whose columns changed (added, removed, renamed, or a different type)

**Granularity is a design question.** Because the rules are not limited to what `{pipapi}` uses, they could in principle separate folders by very small differences. The aim is the opposite: a reasonably coarse set of schemas. Which differences are worth a new schema, and which are only recorded as detail within one, is open work **[OPEN]**. The first version starts coarse.

**Out of scope for the first version:** differences in *meaning only*, where a column keeps its name and type but its definition changes. They can't be detected from file structure alone and would need information from the data team. Record them as a known limit **[OPEN, later]**.

### Relationship rules

```
PIP release --many:1--> pairing (schema + pipapi version) --1:1--> container
schema --1:many--> vintage folders
```

- **Every vintage folder belongs to exactly one schema.** Zero or several matches are errors.
- A schema contains many vintage folders.
- A `{pipapi}` version can support several schemas, and a schema can be supported by many `{pipapi}` versions. Choosing which pairing is deployed is *not* `{pipschema}`'s job in the first version.
- Many PIP releases can share one schema.
- **Folders that share a release-date prefix are expected to belong to the same schema [OPEN].** Report violations as a validation problem. Don't assume it.

---

## 5. Why a central schema file (design rationale)

- **One place to maintain.** Hundreds of vintage folders exist and more arrive. Changing each one is impractical. One file is manageable and reviewable.
- **Removes guesswork from the API.** `{pipapi}` can stop inferring structure from folder-name dates. In the longer term each `{pipapi}` release can support a single schema, which allows old conditional code to be deleted.
- **Efficiency.** Containers read a ready-made list of their vintage folders instead of scanning everything and rejecting most of it. (The project charter requires code to be as efficient as possible.)
- **Reproducibility.** An explicit, versioned record of "which vintage had which structure" is part of the audit trail behind the numbers.

---

## 6. Where to find the schema content (to be worked separately)

Defining the actual schemas and rules is **separate work, done inside `{pipschema}`**, not in this document. The only schema-related knowledge that exists today is in the `{pipapi}` repository. Treat it as a **starting reference and one example, not as the list of schemas**:

- `R/create_lkups.R`, function `create_lkups()`: shows which files and folders `{pipapi}` expects inside a vintage folder. Part of it runs only for folders after a certain date (the "new lineup" approach). That is one known case where `{pipapi}` depends on the structure of a vintage folder, and it is not the only one that may exist.
- `R/create_lkups.R`, function `use_new_lineup_version()`: the current way of telling that case apart, a date cutoff on the folder name (after 2025-05-01). This is the kind of hard-coded guess `{pipschema}` is meant to replace with explicit, content-based rules.
- `R/create_lkups.R`, function `get_vintage_pattern_regex()`: the current definition of a vintage folder name.

Earlier design notes mentioned incremental changes in 2025 (file format switches, auxiliary table updates). They are a hint that structural differences may exist beyond the lineup change, and they are to be checked against the data.

---

## 7. Hard constraints

1. **Exactly one schema per vintage folder.** Zero or several matches are errors, never resolved silently.
2. **No silent fallbacks.** If a rule can't be evaluated, report it. Never guess a schema.
3. **Strict validation** every time the schema file is generated. Generation stops on failure.
4. **Read-only on the data.** The only output is the schema file.
5. **Deterministic and reproducible.** Same data and rules give the same output.
6. **Rules are explicit and reviewable.** Stored as readable definitions, not hidden logic. Every rule change or new schema is a deliberate, reviewed change.
7. **Existing schema definitions don't change meaning** without explicit review. Changing one would reclassify past data and break reproducibility.
8. **Decisions based on folder contents, not only the folder name.**
9. **Efficient.** Cheap checks first. Don't load full data tables.
10. **All tests must always pass.** Tests use synthetic folder trees and never depend on the real `/Data`. Runs on real data are manual and read-only.
11. **Stable, versioned output format**, so consumers (for example `{pipapi}` deployments) can check they understand it.

---

## 8. Deliverables

1. R package `{pipschema}` whose core is the **schema definitions**.
2. A **classification function** returning, per vintage folder, its schema or a clear error.
3. A **validation step** with a readable problem report.
4. A **central schema file** (generated, never hand-edited, with a `schema_file_version` field).
5. **Tests** on synthetic folder trees.
6. **Documentation** of each schema and its rules, and of how to add a new schema.

**First version excludes:** the `pairing` file.

---

## 9. Open questions that affect `{pipschema}`

1. **Release definition.** Is a PIP release exactly the date prefix of vintage folder names?
2. **Release consistency.** Do all folders sharing a date prefix (different PPP years, PROD/INT/TEST) always share a schema?
3. **Schema file name, location and format.** One file at the top of `/Data`, or one file in a new `schemas/` folder? YAML or JSON?
4. **How rules are expressed**, and what kinds of rule are needed.
5. **Granularity: how small a difference counts.** The rules are not limited to what `{pipapi}` reads today, but the aim is a reasonably coarse set of schemas, not a very fine one. Which differences define a new schema, and which are recorded only as detail within a schema, is undecided. The decision for the first version is to **start coarse**, and to refine only when the data shows it is needed.
5b. **Meaning-only changes** (same names and types, different definition) are out of scope for the first version and a known limit.
6. **Folders that match no schema** (older or damaged ones): fail, or list as excluded? Never classify them silently.
7. **Sharing the vintage-folder-name definition** with `{pipapi}`, so both agree on what a vintage folder is.
8. **The `pairing` file**: who writes it, when, where it lives. (Not for the first version.)
9. **In-place data corrections.** If a vintage's contents are corrected later, is its classification stable?
10. **ITS items that may later affect the file's location or timing**: how data is copied from Blob to the VMs, and how a new vintage reaches running containers.

---

## 10. Suggested first step

Start with a short design session inside `{pipschema}` (for example `/cg-brainstorm`), working from the definition in section 4a:

1. Decide **which structural differences between vintage folders are worth separating into schemas** (the granularity question in section 4a). Use section 6 as starting evidence: `{pipapi}`'s code shows some differences that exist and are important, but it does not limit what the rules may cover.
2. Decide **how to express the rules** (open questions 4 to 6).
3. Run the rules over the real folders (read-only) and see **which groups appear**. Those groups are the schemas, and they are named afterwards.
4. Settle the schema file format (open question 3) only after that.
