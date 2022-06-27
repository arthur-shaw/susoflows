
#' Calculates the last sync date in the action log data for each interviewer
#' 
#' Returns a data frame. Columns contain Contains the user ID (`UserId`) and last sync date (`last_sync_date`). 
#' Rows correspond to interviewers.
#' `NA` in `last_sync_date` mean that there was no sync action identified in the action log--that is between the `start` and `end` dates used to obtain the action logs.
#' 
#' @param df Data frame consisting of one or more user action log.
#' 
#' @return Data frame .
#' 
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr `%>%` filter mutate group_by summarize ungroup
#' @importFrom rlang .data
#' @importFrom stringr str_detect str_sub
#' @importFrom lubridate ymd
#' 
#' @export 
calc_last_user_sync <- function(
	df 	# action log data file 
) {

	# check inputs 
	# df is a data frame
    assertthat::assert_that(
        is.data.frame(df),
        msg = paste0(
            "The object provided in `df` is not a data frame.",
            "\nPlease provide a data frame."
        )
    )

    # df contains columns
    assertthat::assert_that(
        assertthat::has_name(df, c("Time", "Message", "UserId")),
        msg = paste0(
            "The data frame provided is missing at least one of the following columns: ",
            "`Time`, `Message`, `UserId`"
        )
    )

    # columns are chr
    assertthat::assert_that(
        is.character(df$Time) & is.character(df$Message) & is.character(df$UserId),
        msg = paste0(
            "One or more of the following columns is not a character vector: ",
            "`Time`, `Message`, `UserId`"
        )
    )    

	# extract last sync date for each interviewer
	sync_dates <- df %>%
		# keep only completed syncs
		dplyr::filter(
            stringr::str_detect(.data$Message, pattern = "^Sync completed") | 
            is.na(.data$Message)
        ) %>%
		# transform character date/time into date
		dplyr::mutate(date = lubridate::ymd(stringr::str_sub(.data$Time, start = 1L, end = 10L))) %>%
		# keep the first (most recent) sync event for each user
		dplyr::group_by(.data$UserId) %>%
		dplyr::summarize(last_sync_date = dplyr::first(.data$date)) %>%
		dplyr::ungroup()

	return(sync_dates)

}

#' Obtains action logs for all interviewer users
#' 
#' Performs workflow to obtain all action logs:
#' 
#' 1. Obtain the list of all interviewers
#' 2. Fetch the action logs for each interviewers
#' 
#' Contains action log columns: time of event (`Time`) and description of event (`UserId`).
#' Also contains an identifier for the user: `UserID`, a GUID that can be used to add the user name.
#' Rows correspond to events in the action log.
#' `NA` values in `Time` and `Message` mean that there were no actions in action log between the `start` and `end` dates used to obtain the action logs.
#' 
#' @return Data frame.
#' 
#' @param start Character vector. Date in YYY-MM-DD format
#' @param end Character vector. Date in YYY-MM-DD format
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Workspace whose action logs to get.
#' @param user API user name
#' @param password API password
#' 
#' @importFrom susoapi get_interviewers get_user_action_log
#' @importFrom dplyr `%>%` filter
#' @importFrom rlang .data
#' @importFrom purrr map_dfr
#' 
#' @export 
get_all_user_logs <- function(
    start,
    end, 
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password 
) {

    # check inputs
    # start date
    assertthat::assert_that(
        is_valid_date(start),
        msg = "Invalid start date provided. Please specify `start` as a character in YYYY-MM-DD format"
    )

    # end date
    assertthat::assert_that(
        is_valid_date(end),
        msg = "Invalid end date provided. Please specify `end` as a character in YYYY-MM-DD format"
    )

    # fetch list of users
    interviewers <- susoapi::get_interviewers(
        server = server, 
        workspace = workspace,
        user = user, 
        password = password) %>%
        # remove any rows where interviewer missing
        # for example, supervisors with no interviewers
        dplyr::filter(!is.na(.data$UserId) & .data$UserId != "")

    # fetch interview log for each user
    action_logs <- purrr::map_dfr(
        .x = dplyr::pull(interviewers, .data$UserId),
        .f = ~ susoapi::get_user_action_log(
            user_id = .x,
            start = start,
            end = end,
            server = server,
            workspace = workspace,
            user = user,
            password = password
        )
    )

    return(action_logs)

}

#' Compute the last sync date for each interviewer
#' 
#' Conducts full workflow to get the last sync date for each interviewer:
#' 
#' 1. Obtain the list of all interviewers
#' 2. Fetch the action logs for each interviewers
#' 3. Determine the last sync date for each interviewer
#' 
#' Returns a data frame. Columns contain Contains the user ID (`UserId`) and last sync date (`last_sync_date`). 
#' Rows correspond to interviewers.
#' `NA` in `last_sync_date` mean that there was no sync action identified in the action log--that is between the `start` and `end` dates used to obtain the action logs.
#' 
#' @param start Character vector. Date in YYY-MM-DD format
#' @param end Character vector. Date in YYY-MM-DD format
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Workspace whose action logs to get.
#' @param user API user name
#' @param password API password
#' 
#' @return Data frame consisting of one or more user action log.
#' 
#' @importFrom susoapi get_interviewers get_user_action_log
#' @importFrom dplyr `%>%` filter
#' @importFrom rlang .data
#' 
#' @export 
get_last_sync_dates <- function(
    start,
    end, 
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password 
) {

    # check inputs
    # start date
    assertthat::assert_that(
        is_valid_date(start),
        msg = "Invalid start date provided. Please specify `start` as a character in YYYY-MM-DD format"
    )

    # end date
    assertthat::assert_that(
        is_valid_date(end),
        msg = "Invalid end date provided. Please specify `end` as a character in YYYY-MM-DD format"
    )

    # get all user logs
    action_logs <- get_all_user_logs(
        start = start,
        end = end,
        server = server,
        workspace = workspace,
        password = password
    )

    # calculate the last sync date for each user
    calc_last_user_sync(df = action_logs)

}

# TODO: think whether syncs per day is useful
# get_syncs_per_day <- function(
#     start,
#     end, 
#     server = Sys.getenv("SUSO_SERVER"),     # full server address
#     user = Sys.getenv("SUSO_USER"),         # API user name
#     password = Sys.getenv("SUSO_PASSWORD")  # API password 
# ) {

#     # fetch list of users
#     interviewers <<- susoapi::get_interviewers(
#         server = server, 
#         user = user, 
#         password = password) %>%
#         # remove any rows where interviewer missing
#         # for example, supervisors with no interviewers
#         filter(!is.na(.data$UserId) & .data$UserId != "")

#     # fetch interview log for each user
#     action_logs <<- purrr::map_dfr(
#         .x = dplyr::pull(interviewers, .data$UserId),
#         .f = ~ susoapi::get_user_action_log(
#             user_id = .x,
#             start = start,
#             end = end,
#             server = server,
#             user = user,
#             password = password
#         )
#     )


# }
