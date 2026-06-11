# Deploying to Posit Connect Cloud

Deployment is git-backed: Posit Connect Cloud rebuilds the app from the
GitHub repo using `manifest.json`, with `.rscignore` excluding internal files
from the bundle. There is no push-button publish step from R.

## Procedure

1. **Land the change on `main`** with CI green (CI runs the full testthat
   suite and the release checklist).
2. **Regenerate the manifest if the bundle changed** — required after
   adding/removing files that `app.R` sources, adding `www/` assets, file
   content changes (the manifest pins per-file checksums), or
   `renv::snapshot()`. Do NOT call `rsconnect::writeManifest(appDir = '.')`
   bare — it bundles every file in the directory, including `.rscignore`d
   internals, and the release checklist will fail. Build `appFiles` as
   git-tracked files filtered by `.rscignore`:

   ```r
   tracked <- system2("git", c("-c", "core.quotepath=false", "ls-files"), stdout = TRUE)
   ignore <- trimws(readLines(".rscignore", warn = FALSE))
   ignore <- ignore[nzchar(ignore) & !startsWith(ignore, "#")]
   is_excluded <- function(f) {
     any(vapply(ignore, function(p) {
       if (endsWith(p, "/")) startsWith(f, p)
       else f == p || startsWith(f, paste0(p, "/"))
     }, logical(1)))
   }
   rsconnect::writeManifest(
     appDir = ".",
     appFiles = tracked[!vapply(tracked, is_excluded, logical(1))]
   )
   ```

   Commit the manifest. The release checklist fails if a sourced file is
   missing from it or an `.rscignore` path leaks in.
3. **Run the release checklist locally** (also enforced in CI):

   ```bash
   Rscript -e "source('R/release_checklist.R'); validate_release_checklist()"
   ```
4. **Bump the version**: set `Version:` in `DESCRIPTION` and move the
   matching section in `NEWS.md` from "in development" to a dated release.
5. **Tag the deployed commit**:

   ```bash
   git tag vX.Y.Z
   git push origin main --tags
   ```
6. **Verify the live app** at
   <https://anthonypuggs-housing-affordability-dashboard.share.connect.posit.cloud/>
   using `docs/ui_smoke_checklist.md` (both themes).

## Notes

- Scheduled data refreshes (`.github/workflows/data-refresh.yml`) commit
  `data/*.csv` only and do not constitute a release; they do not bump the
  version or tag.
- `renv.lock` pins packages; Connect restores from it. Never ad-hoc
  `install.packages()` for project dependencies.
