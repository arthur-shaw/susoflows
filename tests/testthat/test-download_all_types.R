# =============================================================================
# inputs
# =============================================================================

# N/A

# =============================================================================
# outputs
# =============================================================================

# get name of file
qnrs <- find_matching_qnrs(matches = "^EHCVM")
qnr_id_version <- qnrs$QuestionnaireIdentity
qnr_title <- qnrs$Title
qnr_variable <- qnrs$Variable
qnr_version <- qnrs$Version

# compose file names
file_name_Tabular <- paste0(
    qnr_variable, "_",
    qnr_version, "_",
    "Tabular", "_",
    "All_no-meta.zip"
    )

file_name_STATA <- paste0(
    qnr_variable, "_",
    qnr_version, "_",
    "STATA", "_",
    "All_no-meta.zip"
    )

file_name_SPSS <- paste0(
    qnr_variable, "_",
    qnr_version, "_",
    "SPSS", "_",
    "All_no-meta.zip"
    )

file_name_Binary <- paste0(
    qnr_variable, "_",
    qnr_version, "_",
    "Binary", "_",
    "All_no-meta.zip"
    )

file_name_DDI <- paste0(
    qnr_variable, "_",
    qnr_version, "_",
    "ddi", ".zip"
    )

file_name_Paradata <- paste0(
    qnr_variable, "_",
    qnr_version, "_",
    "Paradata", "_",
    "All_no-meta.zip"
    )



# -----------------------------------------------------------------------------
# file downloaded
# -----------------------------------------------------------------------------

test_that("Files downloaded with expected names", {

    # download file
    download_all_types(
        qnr_id = qnr_id_version,
        include_meta = FALSE,
        path = vcr::vcr_test_path("fixtures")
    )

    # confirm that files downloaded
    expect_true(file.exists(paste0(vcr::vcr_test_path("fixtures"), "/", file_name_Tabular)))
    expect_true(file.exists(paste0(vcr::vcr_test_path("fixtures"), "/", file_name_STATA)))
    expect_true(file.exists(paste0(vcr::vcr_test_path("fixtures"), "/", file_name_SPSS)))
    expect_true(file.exists(paste0(vcr::vcr_test_path("fixtures"), "/", file_name_Binary)))
    expect_true(file.exists(paste0(vcr::vcr_test_path("fixtures"), "/", file_name_DDI)))
    expect_true(file.exists(paste0(vcr::vcr_test_path("fixtures"), "/", file_name_Paradata)))

})

# teardown: remove files
purrr::walk(
    .x = c(file_name_STATA, file_name_Tabular, file_name_SPSS, file_name_Binary, file_name_DDI, file_name_Paradata),
    .f = ~ file.remove(paste0(vcr::vcr_test_path("fixtures"), "/", .x))
)
rm(
    qnrs, qnr_id_version, qnr_title, qnr_variable, qnr_version, 
    file_name_Tabular, file_name_STATA, file_name_SPSS, 
    file_name_Binary, file_name_DDI, file_name_Paradata
)
