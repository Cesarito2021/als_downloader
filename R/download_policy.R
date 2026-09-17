#' Plan download concurrency
#' @param mode Deployment mode: `"hosted"` or `"local"`.
#' @param available_cores Available logical CPU cores. Missing values use one.
#' @param requested User-selected worker count, or `NULL` for the recommendation.
#' @param provider_limit Maximum simultaneous transfers allowed by the provider.
#' @param jobs Number of pending tile transfers.
#' @return A list containing recommended, maximum, requested and effective counts.
#' @details Hosted mode uses one worker. Local mode leaves four available cores
#'   unused when possible and initially recommends at most ten workers. During
#'   automated package checks callers must explicitly request no more than two.
#' @export
#' @examples
#' download_worker_policy("local", available_cores = 8)
#' download_worker_policy("hosted", available_cores = 16, requested = 4)
download_worker_policy <- function(mode, available_cores = NA_integer_,
                                   requested = NULL, provider_limit = Inf,
                                   jobs = Inf) {
  mode <- match.arg(mode, c("hosted", "local"))
  positive_integer <- function(x, label, allow_zero = FALSE, allow_inf = FALSE) {
    if (length(x) != 1L || !is.numeric(x) || is.na(x) ||
        (!is.finite(x) && !(allow_inf && identical(x, Inf))) ||
        x < (if (allow_zero) 0 else 1) || (is.finite(x) && x != floor(x))) {
      stop(label, " must be a valid integer", call. = FALSE)
    }
    x
  }
  if (length(available_cores) != 1L || !is.numeric(available_cores) ||
      is.na(available_cores) || !is.finite(available_cores) || available_cores < 1) {
    available_cores <- 1L
  }
  cores <- floor(available_cores)
  max_local <- max(1, cores - 4)
  maximum <- if (mode == "hosted") 1 else max_local
  recommended <- min(10, maximum)
  if (is.null(requested)) requested <- recommended
  requested <- positive_integer(requested, "requested")
  provider_limit <- positive_integer(provider_limit, "provider_limit", allow_inf = TRUE)
  jobs <- positive_integer(jobs, "jobs", allow_zero = TRUE, allow_inf = TRUE)
  effective <- min(requested, maximum, provider_limit, jobs)
  list(mode = mode, detected_cores = cores, recommended = recommended,
       maximum = maximum, requested = requested, effective = effective,
       exceeds_recommendation = requested > recommended)
}
