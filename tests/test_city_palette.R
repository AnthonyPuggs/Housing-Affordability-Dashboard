# Runs standalone via `Rscript tests/test_city_palette.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
#
# Colour-blind-safety contract for the shared city palette (UX-02): every colour
# the price/rent city charts draw must come from a vetted colour-blind-safe set
# (Okabe-Ito + neutral grey + the national reference tones), and the eight
# capitals must be mutually distinct so no two cities collide under any vision.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("city palette is colour-blind safe", {
  repo_root <- repo_root_path()
  use_fixture_data()
  source(file.path(repo_root, "plot_setup.R"), local = TRUE)

  check(exists("city_palette", mode = "function"),
        "city_palette() must be defined")
  skip_if_not(exists("city_palette", mode = "function"))

  # Okabe-Ito qualitative palette (the canonical colour-blind-safe set) plus
  # neutral grey for the eighth capital and the black/light national tones.
  okabe_ito <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7",
                 "#D55E00", "#56B4E9", "#F0E442", "#000000")
  cb_safe_allow <- toupper(c(okabe_ito, "#999999", "#E8EEF7"))

  capitals <- c("Sydney", "Melbourne", "Brisbane", "Adelaide",
                "Perth", "Hobart", "Darwin", "Canberra")

  for (dark in c(FALSE, TRUE)) {
    pal <- city_palette(dark)
    off_palette <- setdiff(toupper(unname(pal)), cb_safe_allow)
    check(length(off_palette) == 0,
          paste0("city_palette(dark=", dark,
                 ") uses non-colour-blind-safe colours: ",
                 paste(off_palette, collapse = ", ")))

    missing_caps <- setdiff(capitals, names(pal))
    check(length(missing_caps) == 0,
          paste("city_palette() missing capitals:",
                paste(missing_caps, collapse = ", ")))

    cap_cols <- toupper(unname(pal[capitals]))
    check(length(unique(cap_cols)) == length(capitals),
          paste0("The eight capital colours must be mutually distinct (dark=",
                 dark, "); got ", length(unique(cap_cols)), " unique"))

    # The national reference must not reuse a capital's hue.
    check(!(toupper(pal[["National Avg"]]) %in% cap_cols),
          "National Avg must be visually distinct from every capital")
  }

  # The old ColorBrewer Set1 red/green confusion pair must be gone.
  pal <- city_palette(FALSE)
  check(!identical(toupper(pal[["Sydney"]]), "#E41A1C"),
        "Sydney must not use the old red-green-unsafe Set1 red")
  check(!identical(toupper(pal[["Brisbane"]]), "#4DAF4A"),
        "Brisbane must not use the old red-green-unsafe Set1 green")
})
