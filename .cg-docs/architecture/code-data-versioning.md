# Code–data versioning

**Status:** decided design, not implemented.  
**Written:** 2026-10-31.  
**Use:** reference for later plans. Do not restate this elsewhere.  
**Wins over:** earlier brainstorms where they conflict, except items marked **[OPEN]**.

Numbers such as `v1.4.2` and `v1.6.0` in the Azure diagram are examples. Real pins come from the pairing freeze.

## Goal

A request must hit one frozen pairing: one `{pipapi}` tag, and only the vintage folders that tag can read. The same request later must hit the same pairing.

Today this is implicit: `use_new_lineup_version()` (date after 2025-05-01), Dockerfile installs `pipapi@DEV`, one container loads every folder under `/Data`.

## Chain

```text
PIP release date          first 8 digits of the vintage folder name
    → vintage folders     often two, different PPP years, same date prefix
    → one schema          {pipschema}; both folders of a release must match
    → one pipapi tag      the frozen pin for that schema
    → one container       image of that tag, loads only that schema's folders
```

A release is the date prefix, not one folder. `20260922` is one release with `20260922_2017_01_02_PROD` and `20260922_2021_01_02_PROD`.

A schema is folder **shape** (files, formats, columns), not content and not a date. Current groups: `pre-lineup` (13 folders, 20220609–20250401) and `new-lineup` (6 folders, 20250930–20260922). See `pipschema/inst/schemas/schema.yml`.

## Three files — do not merge them

| File | Says | Changes when | Lives |
|---|---|---|---|
| `schema.yml` | Which folders share a shape | New vintage, or a reviewed rule change | With the data (`/Data`) |
| `find_pipapi_version` result | Which pipapi versions were verified to run | A test is re-run | In `find_pipapi_version`. Evidence, not a pin |
| pairing file | The frozen pin: schema → one pipapi tag; release date inherits it | A new release is bound. Append-only | Deploy workflow. Not inside vintage folders |

"Can run" is not "will serve". Several tested versions may work on one schema. The pairing picks one and does not rewrite it.

## Names

| Name | What it is |
|---|---|
| `release/1` | Long-lived 1.x branch of the **Azure deploy repo**. Not a pipapi branch. Not an environment |
| `v1.6.0` | Frozen commit. GitHub `{pipapi}` tag and Azure deploy-repo tag must be the same number |
| `api:1.6.0` | Image in ACR, built from that deploy-repo tag |
| `api-1-6` | Running container for minor line 1.6. A patch replaces this container; it does not add a slot |
| `X-API-Version: 1.6` | Custom header. The `X-` is not a version. Value `1.6` routes to `api-1-6`. Missing header → `api-latest` |
| `DEV` / `QA` / `PROD` | Pipeline stages and storage accounts. Not git branches in the target layout |

## Who owns what

**This team** creates the Azure DevOps repos, branches, tags, and Dockerfiles. **ITS** operates the runtime in the architecture diagram: ACR, VMs, Podman, nginx, blobfuse2, image promotion, data copy.

`{pipschema}` reads vintage folders and writes `schema.yml`. It does not own Docker, nginx, or `{pipapi}` startup.

## Once, for what already exists

1. Join `schema.yml` to the `find_pipapi_version` results. Decide the two odd cases: `20220609` tested as `1.0.0`, and `20250401_2021` failing under `1.3.16`. Do not add a schema until that decision says the shape actually differs.
2. Write the pairing file once: one pipapi tag per schema. Existing release dates inherit it. Not one pairing per folder.
3. `{pipapi}`: load an explicit folder list (`vintage_pattern`). Stop using the May 2025 date cutoff for load decisions. One bad folder must not kill startup.
4. Tag those pins on GitHub `{pipapi}`. Not `@DEV`. Prefer a release tag over a `.900x` dev version.
5. Azure API repo (`ITSES-POVERTYSCOREAPI`), only after 2–4 exist:
   - Inspect the live Dockerfile and `main.R`.
   - Create `release/1` from the commit that matches what is live. Stop using `DEV` / `QA` / `PROD` as version branches.
   - Dockerfile installs the frozen `{pipapi}` tag, not `@DEV`. One image per pairing, not per vintage folder.
   - `main.R` loads only that schema's folders.
   - Tag the deploy repo with the same number (`v1.5.15` → image `api:1.5.15` → container `api-1-5`). Repeat for the other pairing.
6. Publish `schema.yml` with the data repo so VMs see it on the shared mount.

## Each next release

- Same shape: classify the new folders. The existing container loads them. No new branch, Dockerfile, or image.
- New shape, or a new pipapi tag that must be pinned: new tag on `release/1`, new image, new container slot.
- New major (`2.0.0`): create `release/2`.

## Runtime (ITS, after the contract exists)

Tag pipeline builds one image and promotes that same image DEV → QA → PROD. One VM per environment. Podman network. nginx is the only published port. Containers bind-mount the same blobfuse2 `/mnt/data`. VM disk holds OS and images only.

Do not run several containers on the shared mount until each `main.R` loads only its schema's folders.

## Open

- Odd folders: one pin for all of `pre-lineup`, or a separate pin for `20220609`.
- User-facing route: header `X-API-Version` (diagram) vs `/releases/<date>/...` (earlier decision). Pending ITS.
- How a new vintage reaches an already running container: restart or reload.
- Who appends the pairing file, and at data publish or at API deploy.
- Azure diagram versions (`1.4.2`, `1.6.0`) are not the pins. Pins come from step 2.
