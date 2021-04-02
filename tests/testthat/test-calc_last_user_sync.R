# =============================================================================
# inputs
# =============================================================================

# -----------------------------------------------------------------------------
# df is a data frame with expected columns of expected type
# -----------------------------------------------------------------------------

# df is a data frame
test_that("Error if `df` is not a data frame", {

    expect_error(calc_last_user_sync(df = "boo"))

})

# df contains expected columns
test_that("Error if `df` is not a data frame", {

    fake_frame <- data.frame(
        Time = "2021-03021",
        Message = "yo!",
        c = "I am an error"
    )

    expect_error(calc_last_user_sync(df = fake_frame))

})

# df columns are all character
test_that("Error if any column is non-character", {

    fake_frame <- data.frame(
        Time = "2021-03021",
        Message = "yo!",
        UserId = 123
    )

    expect_error(calc_last_user_sync(df = fake_frame))

})

# =============================================================================
# outputs
# =============================================================================

# -----------------------------------------------------------------------------
# df with expected columns
# -----------------------------------------------------------------------------

# get user action logs
action_logs <- get_all_user_logs(start = "2021-02-01", end = "2021-03-31")

test_that("Data frame with expected columns of expected type", {

    vcr::use_cassette("calc_last_user_sync_input_df", {
        x <- calc_last_user_sync(df = action_logs)
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, 
        c("UserId", "last_sync_date"), 
        ignore.order = TRUE)

})
