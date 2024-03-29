# =============================================================================
# inputs
# =============================================================================

# -----------------------------------------------------------------------------
# start date is valid
# -----------------------------------------------------------------------------

test_that("Error if start date invalid", {

    expect_error(
        get_all_user_logs(start = "2021-02-99", end = "2021-03-31")
    )

})

# -----------------------------------------------------------------------------
# end date is valid
# -----------------------------------------------------------------------------

test_that("Error if end date invalid", {

    expect_error(
        get_all_user_logs(start = "2021-02-01", end = "2021-03-32")
    )

})

# =============================================================================
# outputs
# =============================================================================

# -----------------------------------------------------------------------------
# df with expected columns
# -----------------------------------------------------------------------------

test_that("Returns df with expected columns", {

    x <- suppressMessages(
        get_all_user_logs(start = "2021-02-01", end = "2021-03-31")
    )

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c("Time", "Message", "UserId"), 
        ignore.order = TRUE)

})

