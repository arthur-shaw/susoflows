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
qnr_export <- "STATA"

# compose file name
file_name <- paste0(
    qnr_variable, "_",
    qnr_version, "_",
    qnr_export, "_",
    "All_no-meta.zip"
    )

# -----------------------------------------------------------------------------
# message on progress, on download
# -----------------------------------------------------------------------------

test_that("Message on progress, on download", {

    # TODO: figure out why cannot be recorded in cassette/why vcr error
    # vcr::use_cassette("download_data_progress_msg", {
        expect_message(
            download_data(
                qnr_id = qnr_id_version,
                export_type = qnr_export,
                include_meta = FALSE,
                path = vcr::vcr_test_path("fixtures")
            )
        )
    # })

})

# teardown: delete downloaded file
file.remove(paste0(vcr::vcr_test_path("fixtures"), "/", file_name))

# -----------------------------------------------------------------------------
# file downloaded
# -----------------------------------------------------------------------------

test_that("File downloaded with expected name", {

    # download file
    download_data(
        qnr_id = qnr_id_version,
        export_type = qnr_export,
        include_meta = FALSE,
        path = vcr::vcr_test_path("fixtures")
    )

    # confirm that file downloaded
    expect_true(file.exists(paste0(vcr::vcr_test_path("fixtures"), "/", file_name)))

})

# teardown: remove file
file.remove(paste0(vcr::vcr_test_path("fixtures"), "/", file_name))
rm(qnrs, qnr_id_version, qnr_title, qnr_variable, qnr_version, qnr_export, file_name)

