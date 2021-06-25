# =============================================================================
# inputs
# =============================================================================

# N/A

# =============================================================================
# outputs
# =============================================================================

# -----------------------------------------------------------------------------
# file downloaded
# -----------------------------------------------------------------------------

construct_file_names <- function(qnr_variable, qnr_version) {

    library(purrr)

    # export types
    export_types <- c("Tabular", "STATA", "SPSS", "Binary", "DDI", "Paradata")

    # all data
    file_names <- purrr::map_chr(
        .x = export_types,
        .f = ~ paste0(
            qnr_variable, "_",
            qnr_version, "_",
            ifelse(.x == "DDI", "ddi.zip", paste0(.x, "_All_no-meta.zip"))
        ))

    # print(file_names)
    return(file_names)

}

qnrs <- susoapi::get_questionnaires()

file_names <- qnrs %>%
    dplyr::rename(
        qnr_variable = variable,
        qnr_version = version
    ) %>%
    dplyr::select(qnr_variable, qnr_version) %>%
    purrr::pmap_dfc(
        .l = .,
        .f = construct_file_names
    ) %>% 
    purrr::flatten_chr()

test_that("File downloaded for each qnr in each format", {

    download_all(
        include_meta = FALSE,
        path = vcr::vcr_test_path("fixtures")
    )

    # files downloaded, with expected names
    purrr::map_lgl(
        .x = file_names,
        .f = ~ expect_true(file.exists(paste0(vcr::vcr_test_path("fixtures"), "/", .x)))
    )



})

# teardown
# remove all files and all file names
purrr::walk(
    .x = file_names,
    .f = ~ file.remove(paste0(vcr::vcr_test_path("fixtures"), "/", .x))
)
# remove all objects created
rm(qnrs, file_names)
