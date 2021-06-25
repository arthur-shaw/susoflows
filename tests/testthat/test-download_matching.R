# =============================================================================
# inputs
# =============================================================================

# -----------------------------------------------------------------------------
# matches
# -----------------------------------------------------------------------------

# valid regex
# TODO: think about how/whether to do this

# =============================================================================
# outputs
# =============================================================================

# -----------------------------------------------------------------------------
# file downloaded
# -----------------------------------------------------------------------------

# get name of file
qnrs <- find_matching_qnrs(matches = "^EHCVM")
qnr_title <- qnrs$title
qnr_variable <- qnrs$variable
qnr_version <- qnrs$version
qnr_export <- "STATA"

# compose file name
file_name <- paste0(
    qnr_variable, "_",
    qnr_version, "_",
    qnr_export, "_",
    "All_no-meta.zip"
    )

# request file
download_matching(
    matches = qnr_title,
    export_type = qnr_export,
    include_meta = FALSE,
    path = vcr::vcr_test_path("fixtures")
        # "C:/Users/wb393438/Downloads/"
)

# check that file exists
test_that("File downloaded matching expected name", {
    expect_true(file.exists(paste0(vcr::vcr_test_path("fixtures"), "/", file_name)))
})

# teardown: remove file
file.remove(paste0(vcr::vcr_test_path("fixtures"), "/", file_name))
rm(qnrs, qnr_title, qnr_variable, qnr_version, qnr_export, file_name)
