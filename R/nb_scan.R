# ============================================================
# NEGATIVE-BINOMIAL SPACE-TIME SCAN STATISTIC
# (overdispersed outbreak / cluster detection)
# ============================================================

#' Negative-Binomial Space-Time Scan Statistic (KK)
#'
#' @description Detects localized disease clusters in space-time surveillance
#'   data while accounting for overdispersion relative to the Poisson
#'   assumption, following the score-based approach of Tango & Takahashi. A
#'   cylinder scan is performed over candidate spatial circles (nested by
#'   distance around each region) and contiguous temporal windows; within each
#'   cylinder an efficient score statistic contrasts observed with expected
#'   counts, standardized by the negative-binomial variance. Two alternatives
#'   are supported: a constant `"elevated"` rate (step increase) and a
#'   `"trend"` (gradually increasing counts, using time-centred weights).
#'   Statistical significance is obtained by Monte-Carlo simulation from the
#'   fitted null, which corrects for the multiplicity of overlapping windows.
#'
#' @param data Long data frame with one row per region-by-time.
#' @param region,time,count,expected Column names (bare or string) for the
#'   region id, time index, observed count, and expected/baseline count.
#' @param coords Length-2 character vector naming the region x/y coordinate
#'   columns (constant within a region).
#' @param type `"elevated"` (constant excess, default) or `"trend"` (gradual
#'   increase over the window).
#' @param max_radius Maximum spatial radius for candidate circles (default
#'   `Inf`).
#' @param max_temporal Maximum length of candidate temporal windows (default
#'   `Inf`).
#' @param size Negative-binomial size (dispersion) parameter; if `NULL` it is
#'   estimated from the data with `MASS::glm.nb` (variance = mu + mu^2 / size).
#' @param n_sim Number of Monte-Carlo replications (default 999).
#' @param seed Optional integer seed.
#'
#' @return One-row tibble describing the most likely cluster: the score
#'   statistic, Monte-Carlo `p.value`, the centre region, the regions in the
#'   cluster, the temporal window, and the observed/expected counts inside it.
#'   The Monte-Carlo null maxima are attached as attribute `null_distribution`.
#'
#' @examples
#' \dontrun{
#' # 4x4 grid of regions over 12 weeks, with a planted outbreak
#' grid <- expand.grid(x = 1:4, y = 1:4)
#' grid$region <- seq_len(nrow(grid))
#' set.seed(1)
#' d <- do.call(rbind, lapply(1:12, function(t) {
#'   data.frame(region = grid$region, x = grid$x, y = grid$y, time = t,
#'              expected = 10)
#' }))
#' d$count <- rnbinom(nrow(d), size = 5, mu = d$expected)
#' hot <- d$region %in% c(1, 2, 5, 6) & d$time >= 9      # outbreak cluster
#' d$count[hot] <- rnbinom(sum(hot), size = 5, mu = 30)
#' kk_nb_scan(d, region, time, count, expected, n_sim = 499)
#' }
#'
#' @export
kk_nb_scan <- function(data, region, time, count, expected,
                       coords = c("x", "y"), type = c("elevated", "trend"),
                       max_radius = Inf, max_temporal = Inf, size = NULL,
                       n_sim = 999, seed = NULL) {
  validate_data_frame(data)
  type <- match.arg(type)
  if (!is.null(seed)) set.seed(seed)

  reg <- .kk_colname(rlang::enquo(region))
  tm <- .kk_colname(rlang::enquo(time))
  ct <- .kk_colname(rlang::enquo(count))
  ex <- .kk_colname(rlang::enquo(expected))
  need <- c(reg, tm, ct, ex, coords)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }

  # Region / time indexing
  regs <- sort(unique(data[[reg]]))
  times <- sort(unique(data[[tm]]))
  R <- length(regs)
  T <- length(times)
  ri <- match(data[[reg]], regs)
  ti <- match(data[[tm]], times)

  Cmat <- matrix(NA_real_, R, T)
  Mmat <- matrix(NA_real_, R, T)
  Cmat[cbind(ri, ti)] <- data[[ct]]
  Mmat[cbind(ri, ti)] <- data[[ex]]
  if (anyNA(Cmat) || anyNA(Mmat)) {
    stop("Every region must be observed at every time (no gaps allowed).")
  }

  # Region coordinates & distance matrix
  first_row <- !duplicated(data[[reg]])
  coord <- data[first_row, c(reg, coords), drop = FALSE]
  coord <- coord[order(match(coord[[reg]], regs)), ]
  D <- as.matrix(stats::dist(coord[, coords, drop = FALSE]))

  # Negative-binomial dispersion (size); variance = mu + mu^2 / size
  if (is.null(size)) {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("Package 'MASS' is required to estimate `size`; supply `size` directly.")
    }
    fit <- tryCatch(
      MASS::glm.nb(y ~ 1 + offset(log(mu)),
        data = data.frame(y = data[[ct]], mu = data[[ex]])),
      error = function(e) NULL
    )
    size <- if (is.null(fit)) {
      # Method-of-moments fallback
      mu_bar <- mean(data[[ex]])
      s2 <- stats::var(data[[ct]])
      max(mu_bar^2 / max(s2 - mu_bar, 1e-6), 1e-3)
    } else {
      fit$theta
    }
  }
  Vmat <- Mmat + Mmat^2 / size

  # Distance-ordered region list per centre (nearest first, within max_radius).
  # Candidate spatial circles are the nested prefixes of each list, which lets
  # the scan accumulate scores with a cumulative sum rather than re-summing every
  # circle from scratch.
  ordered_regions <- lapply(seq_len(R), function(c0) {
    ord <- order(D[c0, ])
    ord[D[c0, ord] <= max_radius]
  })
  # Candidate temporal windows (contiguous, length <= max_temporal)
  windows <- list()
  for (i in seq_len(T)) {
    for (j in i:T) {
      if ((j - i + 1) > max_temporal) break
      windows[[length(windows) + 1]] <- i:j
    }
  }

  # Score of the single most likely cluster for a given count matrix. For each
  # temporal window the per-region weighted residual and variance are formed once
  # (a matrix-vector product); each centre's nested circles are then scored by a
  # cumulative sum over its distance-ordered regions, so a region's contribution
  # is added once rather than re-summed for every enclosing circle.
  best_score <- function(CC) {
    resid <- CC - Mmat
    best <- -Inf
    best_meta <- NULL
    for (w in seq_along(windows)) {
      tw <- windows[[w]]
      # time-centred weights for trend, unit weights for elevated
      cw <- if (type == "trend") (times[tw] - mean(times[tw])) else rep(1, length(tw))
      rnum <- as.vector(resid[, tw, drop = FALSE] %*% cw)
      rden <- as.vector(Vmat[, tw, drop = FALSE] %*% (cw^2))
      for (c0 in seq_len(R)) {
        ord <- ordered_regions[[c0]]
        num <- cumsum(rnum[ord])
        den <- cumsum(rden[ord])
        sc <- num / sqrt(den)
        sc[den <= 0] <- -Inf
        m <- which.max(sc)
        if (sc[m] > best) {
          best <- sc[m]
          best_meta <- list(center = c0, regions = ord[seq_len(m)], window = tw)
        }
      }
    }
    list(score = best, meta = best_meta)
  }

  obs <- best_score(Cmat)

  # Monte-Carlo null: simulate counts from NB(mu, size) and recompute the max
  null_max <- vapply(seq_len(n_sim), function(r) {
    Csim <- matrix(stats::rnbinom(R * T, size = size, mu = as.vector(Mmat)), R, T)
    best_score(Csim)$score
  }, numeric(1))

  p <- (1 + sum(null_max >= obs$score)) / (n_sim + 1)

  meta <- obs$meta
  cluster_regs <- regs[meta$regions]
  win_times <- times[meta$window]
  in_idx <- cbind(
    rep(meta$regions, each = length(meta$window)),
    rep(meta$window, times = length(meta$regions))
  )

  out <- tibble::tibble(
    score = obs$score,
    p.value = p,
    type = type,
    center = regs[meta$center],
    n_regions = length(cluster_regs),
    cluster = paste(cluster_regs, collapse = ", "),
    time_start = min(win_times),
    time_end = max(win_times),
    observed = sum(Cmat[in_idx]),
    expected = sum(Mmat[in_idx]),
    size = size,
    n_sim = n_sim
  )
  attr(out, "null_distribution") <- null_max
  out
}
