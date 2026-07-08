# ============================================================
# HEALTH ECONOMICS & HTA (cost-effectiveness modeling)
# ============================================================

#' Cost-Effectiveness Analysis & Efficiency Frontier (KK)
#'
#' @description Performs an incremental cost-effectiveness analysis across two or
#'   more mutually exclusive strategies. Strategies are ranked by effectiveness,
#'   strictly (strongly) dominated options are flagged, extendedly (weakly)
#'   dominated options are removed by the standard iterative algorithm, and the
#'   incremental cost-effectiveness ratios (ICERs) along the resulting efficiency
#'   frontier are reported.
#'
#' @param data Data frame with one row per strategy, or a numeric vector of costs
#'   (in which case `effect` and `strategy` are supplied separately).
#' @param cost Column of total costs per strategy (bare name or string), or, when
#'   `data` is a numeric vector, a numeric vector of effects.
#' @param effect Column of total effects per strategy (bare name or string), e.g.
#'   QALYs or life-years. Ignored when `data` is a numeric vector.
#' @param strategy Optional column of strategy labels (bare name or string). When
#'   omitted, row order is used.
#'
#' @return Tibble ordered by cost with the incremental cost, incremental effect,
#'   ICER, and a `status` column labelling each strategy as `"frontier"`,
#'   `"dominated"` (strongly dominated), or `"ext.dominated"` (extendedly
#'   dominated). ICERs are computed only between adjacent non-dominated
#'   strategies on the frontier.
#'
#' @examples
#' ce <- data.frame(
#'   strategy = c("Standard care", "Drug A", "Drug B", "Drug C"),
#'   cost = c(2000, 8000, 12000, 30000),
#'   effect = c(3.5, 5.0, 5.5, 5.6)
#' )
#' kk_icer(ce, cost, effect, strategy)
#'
#' @export
kk_icer <- function(data, cost, effect = NULL, strategy = NULL) {
  # Vector interface: kk_icer(costs, effects, labels)
  if (is.numeric(data)) {
    costs <- data
    effects <- cost
    labs <- if (is.null(effect)) paste0("Strategy_", seq_along(costs)) else effect
    if (!is.numeric(effects) || length(effects) != length(costs)) {
      stop("When `data` is numeric, `cost` must be a numeric vector of effects of the same length.")
    }
  } else {
    validate_data_frame(data)
    cost_name <- .kk_colname(rlang::enquo(cost))
    effect_name <- .kk_colname(rlang::enquo(effect))
    strat_quo <- rlang::enquo(strategy)
    missing_cols <- setdiff(c(cost_name, effect_name), names(data))
    if (length(missing_cols) > 0) {
      stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
    }
    costs <- data[[cost_name]]
    effects <- data[[effect_name]]
    labs <- if (rlang::quo_is_null(strat_quo)) {
      if (".strategy" %in% names(data)) data[[".strategy"]] else paste0("Strategy_", seq_along(costs))
    } else {
      as.character(data[[.kk_colname(strat_quo)]])
    }
  }

  if (length(costs) < 2) stop("At least two strategies are required.")
  if (anyNA(costs) || anyNA(effects)) stop("Costs and effects must not contain NA.")

  n <- length(costs)
  status <- rep("frontier", n)

  # 1. Strong dominance: a strategy is dominated if another is at least as
  #    effective and at least as cheap, and strictly better on one axis.
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      if (effects[j] >= effects[i] && costs[j] <= costs[i] &&
        (effects[j] > effects[i] || costs[j] < costs[i])) {
        status[i] <- "dominated"
        break
      }
    }
  }

  # 2. Extended dominance: among non-dominated strategies ordered by cost, ICERs
  #    must increase monotonically; otherwise the option with the higher
  #    preceding ICER is extendedly dominated. Iterate until stable.
  repeat {
    keep <- which(status == "frontier")
    ord <- keep[order(costs[keep], effects[keep])]
    if (length(ord) < 3) break
    icers <- rep(NA_real_, length(ord))
    for (k in 2:length(ord)) {
      d_c <- costs[ord[k]] - costs[ord[k - 1]]
      d_e <- effects[ord[k]] - effects[ord[k - 1]]
      icers[k] <- if (d_e > 0) d_c / d_e else Inf
    }
    # find first violation where a later ICER is smaller than an earlier one
    flagged <- NA_integer_
    for (k in 3:length(ord)) {
      if (icers[k] < icers[k - 1]) {
        flagged <- ord[k - 1]
        break
      }
    }
    if (is.na(flagged)) break
    status[flagged] <- "ext.dominated"
  }

  # 3. Final ICERs along the frontier
  inc_cost <- rep(NA_real_, n)
  inc_effect <- rep(NA_real_, n)
  icer <- rep(NA_real_, n)
  front <- which(status == "frontier")
  front <- front[order(costs[front], effects[front])]
  if (length(front) >= 2) {
    for (k in 2:length(front)) {
      cur <- front[k]
      prev <- front[k - 1]
      inc_cost[cur] <- costs[cur] - costs[prev]
      inc_effect[cur] <- effects[cur] - effects[prev]
      icer[cur] <- if (inc_effect[cur] > 0) inc_cost[cur] / inc_effect[cur] else Inf
    }
  }

  out_order <- order(costs, effects)
  tibble::tibble(
    strategy = labs[out_order],
    cost = costs[out_order],
    effect = effects[out_order],
    inc_cost = inc_cost[out_order],
    inc_effect = inc_effect[out_order],
    icer = icer[out_order],
    status = status[out_order]
  )
}

#' Net Monetary & Net Health Benefit (KK)
#'
#' @description Computes the net monetary benefit (NMB) and net health benefit
#'   (NHB) of one or more strategies at one or more willingness-to-pay (WTP)
#'   thresholds, and flags the optimal (benefit-maximising) strategy at each
#'   threshold. NMB = effect * WTP - cost; NHB = effect - cost / WTP.
#'
#' @param data Data frame with one row per strategy, or a numeric vector of costs.
#' @param cost Column of total costs (bare name or string), or a numeric vector of
#'   effects when `data` is numeric.
#' @param effect Column of total effects (bare name or string). Ignored when
#'   `data` is a numeric vector.
#' @param wtp Willingness-to-pay threshold(s); a scalar or numeric vector. The
#'   result is expanded over all thresholds.
#' @param strategy Optional column of strategy labels (bare name or string).
#'
#' @return Tibble (strategies x thresholds) with `nmb`, `nhb`, and a logical
#'   `optimal` marking the highest-NMB strategy at each threshold.
#'
#' @examples
#' ce <- data.frame(
#'   strategy = c("Standard care", "Drug A", "Drug B"),
#'   cost = c(2000, 8000, 12000),
#'   effect = c(3.5, 5.0, 5.5)
#' )
#' kk_nmb(ce, cost, effect, wtp = c(20000, 50000), strategy = strategy)
#'
#' @export
kk_nmb <- function(data, cost, effect = NULL, wtp = 50000, strategy = NULL) {
  if (is.numeric(data)) {
    costs <- data
    effects <- cost
    labs <- if (is.null(effect)) paste0("Strategy_", seq_along(costs)) else effect
  } else {
    validate_data_frame(data)
    cost_name <- .kk_colname(rlang::enquo(cost))
    effect_name <- .kk_colname(rlang::enquo(effect))
    strat_quo <- rlang::enquo(strategy)
    missing_cols <- setdiff(c(cost_name, effect_name), names(data))
    if (length(missing_cols) > 0) {
      stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
    }
    costs <- data[[cost_name]]
    effects <- data[[effect_name]]
    labs <- if (rlang::quo_is_null(strat_quo)) {
      paste0("Strategy_", seq_along(costs))
    } else {
      as.character(data[[.kk_colname(strat_quo)]])
    }
  }

  if (!is.numeric(wtp) || any(wtp <= 0)) stop("`wtp` must be positive.")

  grid <- expand.grid(.i = seq_along(costs), wtp = wtp, KEEP.OUT.ATTRS = FALSE)
  res <- tibble::tibble(
    strategy = labs[grid$.i],
    wtp = grid$wtp,
    cost = costs[grid$.i],
    effect = effects[grid$.i],
    nmb = effects[grid$.i] * grid$wtp - costs[grid$.i],
    nhb = effects[grid$.i] - costs[grid$.i] / grid$wtp
  )
  res %>%
    dplyr::group_by(.data$wtp) %>%
    dplyr::mutate(optimal = .data$nmb == max(.data$nmb)) %>%
    dplyr::ungroup()
}

#' Cost-Effectiveness Acceptability Curve (KK)
#'
#' @description Builds a cost-effectiveness acceptability curve (CEAC) from the
#'   output of a probabilistic sensitivity analysis (PSA). For each
#'   willingness-to-pay threshold the function computes, across simulations, the
#'   probability that each strategy yields the highest net monetary benefit. The
#'   cost-effectiveness acceptability frontier (the strategy optimal on the
#'   expected-NMB frontier) is also identified.
#'
#' @param data Long-format data frame of PSA draws with one row per
#'   simulation x strategy.
#' @param sim Column identifying the simulation / iteration (bare name or string).
#' @param strategy Column of strategy labels (bare name or string).
#' @param cost Column of simulated costs (bare name or string).
#' @param effect Column of simulated effects (bare name or string).
#' @param wtp Numeric vector of willingness-to-pay thresholds to evaluate.
#'
#' @return Tibble (strategies x thresholds) with `prob_ce` (probability the
#'   strategy is cost-effective, i.e. highest NMB) and `on_frontier` (whether the
#'   strategy has the highest mean NMB at that threshold).
#'
#' @examples
#' set.seed(1)
#' psa <- do.call(rbind, lapply(1:200, function(i) {
#'   data.frame(
#'     sim = i,
#'     strategy = c("A", "B"),
#'     cost = c(rnorm(1, 8000, 1500), rnorm(1, 12000, 2000)),
#'     effect = c(rnorm(1, 5.0, 0.3), rnorm(1, 5.5, 0.3))
#'   )
#' }))
#' kk_ceac(psa, sim, strategy, cost, effect, wtp = seq(0, 60000, 10000))
#'
#' @export
kk_ceac <- function(data, sim, strategy, cost, effect,
                    wtp = seq(0, 1e5, by = 5000)) {
  validate_data_frame(data)
  sim_name <- .kk_colname(rlang::enquo(sim))
  strat_name <- .kk_colname(rlang::enquo(strategy))
  cost_name <- .kk_colname(rlang::enquo(cost))
  effect_name <- .kk_colname(rlang::enquo(effect))

  missing_cols <- setdiff(c(sim_name, strat_name, cost_name, effect_name), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }

  strategies <- sort(unique(as.character(data[[strat_name]])))
  sims <- unique(data[[sim_name]])
  n_sim <- length(sims)

  # cost / effect matrices: rows = simulations, cols = strategies
  cost_mat <- matrix(NA_real_, nrow = n_sim, ncol = length(strategies),
    dimnames = list(NULL, strategies))
  eff_mat <- cost_mat
  sim_idx <- match(data[[sim_name]], sims)
  strat_idx <- match(as.character(data[[strat_name]]), strategies)
  cost_mat[cbind(sim_idx, strat_idx)] <- data[[cost_name]]
  eff_mat[cbind(sim_idx, strat_idx)] <- data[[effect_name]]

  if (anyNA(cost_mat) || anyNA(eff_mat)) {
    stop("Every simulation must contain exactly one row per strategy.")
  }

  out <- vector("list", length(wtp))
  for (w in seq_along(wtp)) {
    nmb <- eff_mat * wtp[w] - cost_mat
    winners <- max.col(nmb, ties.method = "first")
    prob <- tabulate(winners, nbins = length(strategies)) / n_sim
    mean_nmb <- colMeans(nmb)
    on_frontier <- seq_along(strategies) == which.max(mean_nmb)
    out[[w]] <- tibble::tibble(
      strategy = strategies,
      wtp = wtp[w],
      prob_ce = prob,
      on_frontier = on_frontier
    )
  }
  dplyr::bind_rows(out)
}

#' Discount a Stream of Costs or Effects (KK)
#'
#' @description Discounts a stream of future costs or health effects to present
#'   value using the standard exponential discounting formula
#'   `PV = sum(x_t / (1 + rate)^t)`. Times default to `0, 1, ..., n-1` so the
#'   first element is treated as occurring now (undiscounted).
#'
#' @param x Numeric vector of per-period values (costs or effects).
#' @param rate Annual discount rate (e.g. 0.03 for 3 percent).
#' @param times Optional numeric vector of times (in years) for each element of
#'   `x`. Defaults to `seq_along(x) - 1`.
#'
#' @return One-row tibble with the undiscounted total, the discounted present
#'   value, and the discount rate used.
#'
#' @examples
#' # A 2000-per-year cost stream over 5 years at 3.5 percent
#' kk_discount(rep(2000, 5), rate = 0.035)
#'
#' @export
kk_discount <- function(x, rate = 0.03, times = NULL) {
  if (!is.numeric(x)) stop("`x` must be numeric.")
  if (rate < 0) stop("`rate` must be non-negative.")
  if (is.null(times)) times <- seq_along(x) - 1
  if (length(times) != length(x)) stop("`times` must match the length of `x`.")
  factors <- 1 / (1 + rate)^times
  tibble::tibble(
    undiscounted = sum(x, na.rm = TRUE),
    present_value = sum(x * factors, na.rm = TRUE),
    rate = rate
  )
}

#' Markov Cohort Model (KK)
#'
#' @description Runs a discrete-time Markov cohort simulation, the standard
#'   engine for cost-utility models in health technology assessment. A closed
#'   cohort is pushed through a set of health states according to a
#'   transition-probability matrix; per-cycle state costs and utilities are
#'   accumulated, discounted, and (optionally) half-cycle corrected to give the
#'   total expected cost and QALYs.
#'
#' @param transition Square transition-probability matrix (rows must sum to 1),
#'   with states in the same order as `costs` and `utilities`. May be named. For
#'   time-varying transitions supply a 3-D array `state x state x cycle`.
#' @param costs Numeric vector of costs accrued per cycle in each state.
#' @param utilities Numeric vector of utilities (QALY weight per cycle) in each
#'   state.
#' @param cycles Number of cycles to run.
#' @param init Initial state distribution (numbers or proportions). Defaults to
#'   the whole cohort in the first state.
#' @param disc_cost Annual discount rate for costs (default 0.03).
#' @param disc_effect Annual discount rate for effects (default 0.03).
#' @param cycle_length Length of one cycle in years, used for discounting and to
#'   scale QALYs (default 1).
#' @param half_cycle Apply half-cycle correction (default `TRUE`).
#'
#' @return List with `trace` (a tibble of the cohort distribution across states
#'   by cycle) and `summary` (a one-row tibble of total discounted and
#'   undiscounted cost and QALYs).
#'
#' @examples
#' # Three-state model: Healthy -> Sick -> Dead
#' P <- matrix(c(
#'   0.85, 0.10, 0.05,
#'   0.00, 0.80, 0.20,
#'   0.00, 0.00, 1.00
#' ), nrow = 3, byrow = TRUE,
#' dimnames = list(c("Healthy", "Sick", "Dead"), c("Healthy", "Sick", "Dead")))
#' kk_markov(
#'   transition = P,
#'   costs = c(Healthy = 500, Sick = 8000, Dead = 0),
#'   utilities = c(Healthy = 1, Sick = 0.6, Dead = 0),
#'   cycles = 30
#' )
#'
#' @export
kk_markov <- function(transition, costs, utilities, cycles,
                      init = NULL, disc_cost = 0.03, disc_effect = 0.03,
                      cycle_length = 1, half_cycle = TRUE) {
  # Resolve time-homogeneous vs time-varying transitions
  if (is.matrix(transition)) {
    n_states <- nrow(transition)
    get_P <- function(t) transition
    state_names <- rownames(transition)
  } else if (is.array(transition) && length(dim(transition)) == 3) {
    n_states <- dim(transition)[1]
    if (dim(transition)[3] < cycles) {
      stop("Time-varying `transition` must have at least `cycles` slices.")
    }
    get_P <- function(t) transition[, , t]
    state_names <- dimnames(transition)[[1]]
  } else {
    stop("`transition` must be a square matrix or a 3-D array (state x state x cycle).")
  }

  if (n_states != ncol(get_P(1))) stop("Transition matrix must be square.")
  if (length(costs) != n_states || length(utilities) != n_states) {
    stop("`costs` and `utilities` must have one value per state.")
  }
  # Row-sum check (allow tiny numerical tolerance)
  for (t in seq_len(cycles)) {
    rs <- rowSums(get_P(t))
    if (any(abs(rs - 1) > 1e-6)) {
      stop("Rows of the transition matrix must sum to 1 (cycle ", t, ").")
    }
  }

  if (is.null(state_names)) state_names <- paste0("State_", seq_len(n_states))
  if (is.null(init)) {
    init <- c(1, rep(0, n_states - 1))
  }
  if (length(init) != n_states) stop("`init` must have one value per state.")

  # Cohort trace: rows = cycles 0..cycles, cols = states
  trace <- matrix(0, nrow = cycles + 1, ncol = n_states,
    dimnames = list(NULL, state_names))
  trace[1, ] <- init
  for (t in seq_len(cycles)) {
    trace[t + 1, ] <- trace[t, ] %*% get_P(t)
  }

  # Per-cycle cost and effect (effects scaled by cycle length -> QALYs)
  cycle_cost <- as.numeric(trace %*% costs)
  cycle_qaly <- as.numeric(trace %*% utilities) * cycle_length

  # Half-cycle correction: weight endpoints by 0.5 (trapezoidal)
  w <- rep(1, cycles + 1)
  if (half_cycle) {
    w[1] <- 0.5
    w[length(w)] <- 0.5
  }

  # Discount factors evaluated at the start of each cycle (t = 0, 1, ...)
  t_years <- (seq_len(cycles + 1) - 1) * cycle_length
  df_cost <- 1 / (1 + disc_cost)^t_years
  df_eff <- 1 / (1 + disc_effect)^t_years

  total_cost <- sum(cycle_cost * w)
  total_qaly <- sum(cycle_qaly * w)
  total_cost_disc <- sum(cycle_cost * w * df_cost)
  total_qaly_disc <- sum(cycle_qaly * w * df_eff)

  trace_tbl <- tibble::as_tibble(trace)
  trace_tbl <- tibble::add_column(trace_tbl, cycle = seq_len(cycles + 1) - 1, .before = 1)
  trace_tbl$cost <- cycle_cost
  trace_tbl$qaly <- cycle_qaly

  summary_tbl <- tibble::tibble(
    total_cost = total_cost,
    total_qaly = total_qaly,
    total_cost_disc = total_cost_disc,
    total_qaly_disc = total_qaly_disc,
    disc_cost = disc_cost,
    disc_effect = disc_effect,
    cycles = cycles,
    half_cycle = half_cycle
  )

  list(trace = trace_tbl, summary = summary_tbl)
}

#' Expected Value of Perfect Information (KK)
#'
#' @description Computes the expected value of perfect information (EVPI) from the
#'   output of a probabilistic sensitivity analysis (PSA), quantifying the
#'   expected cost of decision uncertainty at each willingness-to-pay threshold.
#'   EVPI is the difference between the expected net benefit under perfect
#'   information (choosing the best strategy in every simulation) and the expected
#'   net benefit of the single strategy that is best on average:
#'   `EVPI = E_theta[max_d NMB(d, theta)] - max_d E_theta[NMB(d, theta)]`.
#'   Shares the PSA input format of [kk_ceac()].
#'
#' @param data Long-format data frame of PSA draws with one row per
#'   simulation x strategy.
#' @param sim Column identifying the simulation / iteration (bare name or string).
#' @param strategy Column of strategy labels (bare name or string).
#' @param cost Column of simulated costs (bare name or string).
#' @param effect Column of simulated effects (bare name or string).
#' @param wtp Numeric vector of willingness-to-pay thresholds to evaluate.
#' @param pop_size Optional size of the affected population; when supplied the
#'   population EVPI is returned alongside the per-patient value.
#' @param horizon Optional decision time horizon in years over which the
#'   population bears the decision (multiplies the population, discounted at
#'   `disc_rate`). Ignored if `pop_size` is NULL.
#' @param disc_rate Annual discount rate applied over `horizon` (default 0.03).
#'
#' @return Tibble with one row per threshold: the per-patient `evpi`, the
#'   strategy optimal on the expected-NMB frontier (`optimal_strategy`), and, when
#'   `pop_size` is supplied, `population_evpi`.
#'
#' @examples
#' set.seed(1)
#' psa <- do.call(rbind, lapply(1:400, function(i) {
#'   data.frame(
#'     sim = i,
#'     strategy = c("A", "B"),
#'     cost = c(rnorm(1, 8000, 1500), rnorm(1, 12000, 2000)),
#'     effect = c(rnorm(1, 5.0, 0.3), rnorm(1, 5.5, 0.3))
#'   )
#' }))
#' kk_evpi(psa, sim, strategy, cost, effect,
#'         wtp = seq(0, 60000, 10000), pop_size = 5000, horizon = 10)
#'
#' @export
kk_evpi <- function(data, sim, strategy, cost, effect,
                    wtp = seq(0, 1e5, by = 5000),
                    pop_size = NULL, horizon = NULL, disc_rate = 0.03) {
  validate_data_frame(data)
  sim_name <- .kk_colname(rlang::enquo(sim))
  strat_name <- .kk_colname(rlang::enquo(strategy))
  cost_name <- .kk_colname(rlang::enquo(cost))
  effect_name <- .kk_colname(rlang::enquo(effect))

  missing_cols <- setdiff(c(sim_name, strat_name, cost_name, effect_name), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }

  strategies <- sort(unique(as.character(data[[strat_name]])))
  sims <- unique(data[[sim_name]])
  n_sim <- length(sims)

  # cost / effect matrices: rows = simulations, cols = strategies
  cost_mat <- matrix(NA_real_, nrow = n_sim, ncol = length(strategies),
    dimnames = list(NULL, strategies))
  eff_mat <- cost_mat
  sim_idx <- match(data[[sim_name]], sims)
  strat_idx <- match(as.character(data[[strat_name]]), strategies)
  cost_mat[cbind(sim_idx, strat_idx)] <- data[[cost_name]]
  eff_mat[cbind(sim_idx, strat_idx)] <- data[[effect_name]]

  if (anyNA(cost_mat) || anyNA(eff_mat)) {
    stop("Every simulation must contain exactly one row per strategy.")
  }

  # Effective population when a horizon is supplied (discounted person-years)
  pop_mult <- NULL
  if (!is.null(pop_size)) {
    pop_mult <- pop_size
    if (!is.null(horizon)) {
      years <- seq_len(horizon) - 1
      pop_mult <- pop_size * sum(1 / (1 + disc_rate)^years)
    }
  }

  out <- vector("list", length(wtp))
  for (w in seq_along(wtp)) {
    nmb <- eff_mat * wtp[w] - cost_mat
    mean_nmb <- colMeans(nmb)
    # E[max] under perfect info minus max of E (current decision)
    evpi <- mean(apply(nmb, 1, max)) - max(mean_nmb)
    evpi <- max(evpi, 0)  # numerical guard: EVPI is non-negative
    row <- tibble::tibble(
      wtp = wtp[w],
      evpi = evpi,
      optimal_strategy = strategies[which.max(mean_nmb)]
    )
    if (!is.null(pop_mult)) row$population_evpi <- evpi * pop_mult
    out[[w]] <- row
  }
  dplyr::bind_rows(out)
}
