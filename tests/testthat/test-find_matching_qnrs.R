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
# if matching questionnaires
# -----------------------------------------------------------------------------

# message
test_that("Issues message that qnrs found", {

    expect_message(find_matching_qnrs(
        matches = "^EHCVM"
    ))    

})

# data frame with expected columns
test_that("Returns df of qnrs with exepected columns", {

    vcr::use_cassette("find_matching_qnrs_df", {
        x <- suppressMessages(find_matching_qnrs(
            matches = "^EHCVM"
        ))
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c(
            "id", "questionnaireId", "version", 
            "variable", "title", 
            "defaultLanguageName", "translations"
        ), 
        ignore.order = TRUE)

})

# -----------------------------------------------------------------------------
# if NO matching questionnaires
# -----------------------------------------------------------------------------

# if no matching qnrs, expect_warning() 
test_that("Issues message that qnrs found", {

    vcr::use_cassette("find_matching_qrs_warning", {
        expect_warning(find_matching_qnrs(
            matches = "foobar"
        ))
    })

})
