
#' Get list of questionnaires with a matching title.
#'
#' Get a data frame of questionnaires that match the character vector provided.
#'
#' If matching questionnaires are found, return a data frame with attributes. These attributes can be used, among other things, to find the necessary parameters for downloading any associated data.
#'
#' If no matching questionnaires are found, return a warning message.
#'
#' In both cases, print the outcome to the console for information.
#'
#' @param matches Atomic character vector. Character string to be detected in questionnaire title. May be a string, sub-string, or regex.
#' @param workspace Character. Name of the workspace whose questionnaires to query.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Data frame of matching questionnaires, if successful; warning message, if not successful.
#'
#' @importFrom susoapi get_questionnaires
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom stringr str_detect
#' @importFrom tibble as_tibble
#'
#' @export
find_matching_qnrs <- function(
    matches,
    workspace = "primary",
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

    # get list of all questionnaires
    qnrs <- susoapi::get_questionnaires(
        workspace = workspace, 
        server = server, 
        user = user, 
        password = password
    )

    # find those questionnaires that match
    qnrs_found <- dplyr::filter(qnrs, stringr::str_detect(.data$title, matches))

    num_qnrs_found <- nrow(qnrs_found)
    if (num_qnrs_found > 0) {

        message(paste0(
            "Found ", num_qnrs_found, " matching questionnaires."
        ))

        qnrs_match_to_print <- dplyr::select(qnrs_found, .data$title, .data$version, .data$variable, .data$questionnaireId)
        print(tibble::as_tibble(qnrs_match_to_print))

        return(qnrs_found)

    } else if (num_qnrs_found == 0) {

        warning(paste0(
            "No questionnaires found.",
            "\nPlease check the correctness of the search term provided in `matches`: ", matches,
            "\nAlternatively, use `susoapi::get_questionnaires` to get a full list of questionnaires and refine your search term on the `Title` column of that data frame."
        ))

    }

}

#' Downloads data for questionnaires that match
#'
#' If the export file is ready, this function downloads it to the target directory specified in `path`. If the file is not ready, print a message describing the situation.
#'
#' @param matches Atomic character vector. Character string to be detected in questionnaire title. May be a string, sub-string, or regex.
#' @param export_type Type of data to export. Values: \code{c("Tabular", "STATA", "SPSS", "Binary", "DDI", "Paradata")}
#' @param interview_status Status of interviews to export. Values: \code{c("All", "SupervisorAssigned", "InterviewerAssigned", "Completed", "RejectedBySupervisor", "ApprovedBySupervisor", "RejectedByHeadquarters", "ApprovedByHeadquarters")}
#' @param from Start date for interviews to export. date-time, UTC
#' @param to End date for interviews to export. date-time, UTC
#' @param access_token Access token to external storage. Relevant only if external cloud storage is destination for the export file.
#' @param refresh_token Refresh token to external storage.Relevant only if external cloud storage is destination for the export file.
#' @param storage_type External storage type, if relevant. Values: \code{c("Dropbox", "OneDrive", "GoogleDrive")}
#' @param translation_id Translation ID for variable and value labels to include in export files.
#' @param include_meta Logical. If `TRUE`, include questionnaire metadata in export file.
#' @param path File path where export file should be downloaded
#' @param workspace Character. Name of the workspace whose data to download.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @importFrom purrr walk
#' @importFrom rlang .data
#'
#' @export
download_matching <- function(
    # questionnaire name
    matches,
    # describe job
    export_type,
    interview_status = "All",
    from = "",
    to = "",
    access_token = "",
    refresh_token = "",
    storage_type = "",
    translation_id = "",
    include_meta = TRUE,
    # download path
    path,
    # credentials
    workspace = "primary",
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

    # find matching questionnaires
    # if questionnaires found, continue
    # if not, throw and error with the warning from `find_matching_qnrs`
    qnrs_found <- tryCatch(
        find_matching_qnrs(
            matches = matches,
            workspace = workspace,
            server = server,
            user = user,
            password = password
        ),
        warning = function(cnd) {
            stop(conditionMessage(cnd))
        }
    )

    # download them all
    # walk through vector of questionnaire IDs
    # download file to path
    purrr::walk(
        .x = pull(qnrs_found, .data$id),
        .f = ~ download_data(
            # describe job
            qnr_id = .x,
            export_type = export_type,
            interview_status = interview_status,
            from = from,
            to = to,
            access_token = access_token,
            refresh_token = refresh_token,
            storage_type = storage_type,
            translation_id = translation_id,
            include_meta = include_meta,
            # download path
            path = path,
            # credentials
            workspace = workspace,
            server = server,
            user = user,
            password = password
        )
    )

}

#' Download data for a single export job.
#'
#' Executes full download process for a single export job.
#'
#' 1. **Starts export job.** Specifies the parameters of the download job through the `qnr_id`, `export_type`, `interview_status`,  `from`, `to`, `access_token`, `refresh_token`, `storage_type`, `translation_id`, and `include_meta` parameters
#' 2. **Gets regular updates on the progress of the export job.** Messages about progress.
#' 3. **Downloads file to desired destination.** Use the `path` parameter to specify where the export file should be saved.
#'
#' @param qnr_id Questionnaire ID. Format: \code{qnr_id$version}
#' @param export_type Type of data to export. Values: \code{c("Tabular", "STATA", "SPSS", "Binary", "DDI", "Paradata")}
#' @param interview_status Status of interviews to export. Values: \code{c("All", "SupervisorAssigned", "InterviewerAssigned", "Completed", "RejectedBySupervisor", "ApprovedBySupervisor", "RejectedByHeadquarters", "ApprovedByHeadquarters")}
#' @param from Start date for interviews to export. date-time, UTC
#' @param to End date for interviews to export. date-time, UTC
#' @param access_token Access token to external storage. Relevant only if external cloud storage is destination for the export file.
#' @param refresh_token Refresh token to external storage.Relevant only if external cloud storage is destination for the export file.
#' @param storage_type External storage type, if relevant. Values: \code{c("Dropbox", "OneDrive", "GoogleDrive")}
#' @param translation_id Translation ID for variable and value labels to include in export files.
#' @param include_meta Logical. If `TRUE`, include questionnaire metadata in export file.
#' @param path File path where export file should be downloaded
#' @param workspace Character. Name of the workspace whose data to download.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @importFrom susoapi start_export get_export_job_details get_export_file
#'
#' @export
download_data <- function(
    # describe job
    qnr_id,
    export_type,
    interview_status = "All",
    from = "",
    to = "",
    access_token = "",
    refresh_token = "",
    storage_type = "",
    translation_id = "",
    include_meta = TRUE,
    # download path
    path,
    # credentials
    workspace = "primary",
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

    # start export process; get job ID
    job_id <- susoapi::start_export(
        qnr_id = qnr_id,
        export_type = export_type,
        interview_status = interview_status,
        from = from,
        to = to,
        access_token = access_token,
        refresh_token = refresh_token,
        storage_type = storage_type,
        translation_id = translation_id,
        include_meta = include_meta,
        server = server,
        user = user,
        password = password
    )

    # TODO: fail if warning issued by start_export--that is, if export job not started, for whatever reason

    # initialize job status details
    job_complete <- FALSE
    job_has_file <- FALSE

    # while incomplete
    while(job_complete == FALSE & job_has_file == FALSE) {

        # inquire about job status
        job_status <- susoapi::get_export_job_details(
            job_id = job_id,
            workspace = workspace,
            server = server,
            user = user,
            password = password
        )

        # extract salient attributes
        job_percent <- job_status$Progress
        job_status_string <- job_status$ExportStatus
        job_complete <- ifelse(job_status_string == "Completed", TRUE, FALSE)
        job_failed <- ifelse(job_status_string == "Fail", TRUE, FALSE)
        job_has_file <- job_status$HasExportFile

        # fail if update indicates export job failed; surface message
        if (job_failed == TRUE) {
            stop(paste0(
                "Unable to download data for questionnaire ", qnr_id,
                "\nError: ", job_status$Error
            ))
        }

        # provide in-console update on status
        message(paste0(
            "Export status: ", job_status_string, ". ",
            "Percent progress: ", job_percent
        ))

        # wait a few seconds before getting next update
        Sys.sleep(2)

    }

    # when finished fetch file
    message("Downloading file")
    susoapi::get_export_file(
        job_id = job_id,
        path = path,
        workspace = workspace,
        server = server,
        user = user,
        password = password
    )

}

#' Download all export types for a given questionnaire.
#'
#' Downloads tab-delimited, Stata, SPSS, binary, DDI, and Paradata for a given questionnaire version.
#'
#' @param qnr_id Questionnaire ID. Format: \code{qnr_id$version}
#' @param interview_status Status of interviews to export. Values: \code{c("All", "SupervisorAssigned", "InterviewerAssigned", "Completed", "RejectedBySupervisor", "ApprovedBySupervisor", "RejectedByHeadquarters", "ApprovedByHeadquarters")}
#' @param from Start date for interviews to export. date-time, UTC
#' @param to End date for interviews to export. date-time, UTC
#' @param access_token Access token to external storage. Relevant only if external cloud storage is destination for the export file.
#' @param refresh_token Refresh token to external storage.Relevant only if external cloud storage is destination for the export file.
#' @param storage_type External storage type, if relevant. Values: \code{c("Dropbox", "OneDrive", "GoogleDrive")}
#' @param translation_id Translation ID for variable and value labels to include in export files.
#' @param include_meta Logical. If `TRUE`, include questionnaire metadata in export file.
#' @param path File path where export file should be downloaded
#' @param workspace Character. Name of the workspace whose data to download.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @importFrom purrr walk
#'
#' @export
download_all_types <- function(
    # describe job
    qnr_id,
    interview_status = "All",
    from = "",
    to = "",
    access_token = "",
    refresh_token = "",
    storage_type = "",
    translation_id = "",
    include_meta = TRUE,
    # download path
    path,
    # credentials
    workspace = "primary",
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

    export_types <- c("Tabular", "STATA", "SPSS", "Binary", "DDI", "Paradata")

    purrr::walk(
        .x = export_types,
        .f = ~ download_data(
            # describe job
            qnr_id = qnr_id,
            export_type = .x,
            interview_status = interview_status,
            from = from,
            to = to,
            access_token = access_token,
            refresh_token = refresh_token,
            storage_type = storage_type,
            translation_id = translation_id,
            include_meta = include_meta,
            # download path
            path = path,
            # credentials
            workspace = workspace,
            server = server,
            user = user,
            password = password
        )
    )

}

#' Download all export types for all questionnaires
#'
#' Useful at project close, this function downloads tab-delimited, Stata, SPSS, binary, DDI, and Paradata for every questionnaire version.
#'
#' Beware: this function may long-running if binary data, such as audio, are captured for many or all interviews.
#'
#' @param interview_status Status of interviews to export. Values: \code{c("All", "SupervisorAssigned", "InterviewerAssigned", "Completed", "RejectedBySupervisor", "ApprovedBySupervisor", "RejectedByHeadquarters", "ApprovedByHeadquarters")}
#' @param from Start date for interviews to export. date-time, UTC
#' @param to End date for interviews to export. date-time, UTC
#' @param access_token Access token to external storage. Relevant only if external cloud storage is destination for the export file.
#' @param refresh_token Refresh token to external storage.Relevant only if external cloud storage is destination for the export file.
#' @param storage_type External storage type, if relevant. Values: \code{c("Dropbox", "OneDrive", "GoogleDrive")}
#' @param translation_id Translation ID for variable and value labels to include in export files.
#' @param include_meta Logical. If `TRUE`, include questionnaire metadata in export file.
#' @param path File path where export file should be downloaded
#' @param workspace Character. Name of the workspace whose data to download.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @importFrom susoapi get_questionnaires
#' @importFrom dplyr pull
#' @importFrom purrr walk
#' @importFrom rlang .data
#'
#' @export
download_all <- function(
    # describe job
    interview_status = "All",
    from = "",
    to = "",
    access_token = "",
    refresh_token = "",
    storage_type = "",
    translation_id = "",
    include_meta = TRUE,
    # download path
    path,
    # credentials
    workspace = "primary",
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

    # get list of all questionnaires
    qnrs <- susoapi::get_questionnaires(
        workspace = workspace,
        server = server, 
        user = user, 
        password = password
    )

    # download all data types for each questionnaire
    purrr::walk(
        .x = dplyr::pull(qnrs, .data$id),
        .f = ~ download_all_types(
            # describe job
            qnr_id = .x,
            interview_status = interview_status,
            from = from,
            to = to,
            access_token = access_token,
            refresh_token = refresh_token,
            storage_type = storage_type,
            translation_id = translation_id,
            include_meta = include_meta,
            # download path
            path = path,
            # credentials
            workspace = workspace,
            server = server,
            user = user,
            password = password
        )
    )

}
