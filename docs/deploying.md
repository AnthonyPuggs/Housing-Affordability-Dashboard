# Deploying to Posit Connect Cloud

Deployment is git-backed: Posit Connect Cloud rebuilds the app from the
GitHub repo using `manifest.json`, with `.rscignore` excluding internal files
from the bundle. There is no push-button publish step from R.

## Procedure

1. **Land the change on `main`** with CI green (CI runs the full testthat
   suite and the release checklist).
2. **Regenerate the manifest if the bundle changed** — required after
   adding/removing files that `app.R` sources, adding `www/` assets, or
   `renv::snapshot()`:

   ```bash
   Rscript -e "rsconnect::writeManifest(appDir = '.')"
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
