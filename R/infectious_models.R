# ============================================================
# INFECTIOUS DISEASE TRANSMISSION MODELS (compartmental)
# ============================================================

# Internal: fixed-step 4th-order Runge-Kutta integrator. Kept dependency-free so
# the compartmental models do not require deSolve. `deriv` takes (t, y) and
# returns the derivative vector; returns a matrix of states over `times`.
.kk_rk4 <- function(deriv, y0, times) {
  n <- length(times)
  out <- matrix(NA_real_, nrow = n, ncol = length(y0))
  out[1, ] <- y0
  y <- y0
  for (i in seq_len(n - 1)) {
    h <- times[i + 1] - times[i]
    t <- times[i]
    k1 <- deriv(t, y)
    k2 <- deriv(t + h / 2, y + h / 2 * k1)
    k3 <- deriv(t + h / 2, y + h / 2 * k2)
    k4 <- deriv(t + h, y + h * k3)
    y <- y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    y[y < 0] <- 0
    out[i + 1, ] <- y
  }
  out
}

#' Deterministic SIR / SEIR Transmission Model (KK)
#'
#' @description Simulates a deterministic compartmental epidemic model. With
#'   `sigma = NULL` an SIR model is solved; supplying a finite `sigma` adds an
#'   exposed (latent) compartment for an SEIR model. Optional vital dynamics
#'   (`mu`) and a constant vaccination rate (`nu`) are supported. The system is
#'   integrated with a built-in Runge-Kutta solver, so no external ODE package is
#'   required. The basic reproduction number `R0` implied by the parameters is
#'   returned as an attribute.
#'
#' @param beta Transmission rate (effective contact rate).
#' @param gamma Recovery rate; the mean infectious period is `1 / gamma`.
#' @param sigma Rate of progression from exposed to infectious (`1 / sigma` is the
#'   mean latent period). `NULL` (default) gives an SIR model with no latent
#'   stage.
#' @param S0,I0,R0_init Initial number susceptible, infectious, and recovered.
#'   `E0` initialises the exposed compartment for SEIR models.
#' @param E0 Initial number exposed (SEIR only, default 0).
#' @param times Numeric vector of time points to report (default `0:180`).
#' @param mu Per-capita birth = death rate for vital dynamics (default 0, a
#'   closed population).
#' @param nu Per-capita vaccination rate moving susceptibles directly to
#'   recovered/immune (default 0).
#'
#' @return Tibble of the compartment sizes at each time point, plus `incidence`
#'   (new infections per step) and the cumulative incidence `C`. The attribute
#'   `R0` holds the basic reproduction number implied by the parameters.
#'
#' @examples
#' # Classic closed SIR outbreak, R0 = 2.5
#' sir <- kk_seir(beta = 0.5, gamma = 0.2, S0 = 999, I0 = 1, R0_init = 0)
#' attr(sir, "R0")
#'
#' # SEIR with a 5-day latent period
#' seir <- kk_seir(beta = 0.6, gamma = 0.2, sigma = 0.2,
#'                 S0 = 9999, I0 = 1, R0_init = 0, times = 0:120)
#'
#' @export
kk_seir <- function(beta, gamma, sigma = NULL,
                    S0, I0, R0_init = 0, E0 = 0,
                    times = 0:180, mu = 0, nu = 0) {
  if (beta < 0 || gamma <= 0) stop("`beta` must be >= 0 and `gamma` > 0.")
  if (!is.null(sigma) && sigma <= 0) stop("`sigma` must be > 0 when supplied.")
  seir <- !is.null(sigma)

  N <- S0 + E0 + I0 + R0_init

  if (seir) {
    y0 <- c(S = S0, E = E0, I = I0, R = R0_init, C = 0)
    deriv <- function(t, y) {
      S <- y[1]; E <- y[2]; I <- y[3]; R <- y[4]
      Npop <- S + E + I + R
      infection <- beta * S * I / Npop
      c(
        mu * Npop - infection - (mu + nu) * S, # dS
        infection - (sigma + mu) * E, # dE
        sigma * E - (gamma + mu) * I, # dI
        gamma * I + nu * S - mu * R, # dR
        infection # dC (cumulative infections)
      )
    }
    R0 <- (beta / (gamma + mu)) * (sigma / (sigma + mu))
  } else {
    y0 <- c(S = S0, I = I0, R = R0_init, C = 0)
    deriv <- function(t, y) {
      S <- y[1]; I <- y[2]; R <- y[3]
      Npop <- S + I + R
      infection <- beta * S * I / Npop
      c(
        mu * Npop - infection - (mu + nu) * S, # dS
        infection - (gamma + mu) * I, # dI
        gamma * I + nu * S - mu * R, # dR
        infection # dC
      )
    }
    R0 <- beta / (gamma + mu)
  }

  sol <- .kk_rk4(deriv, y0, times)
  colnames(sol) <- names(y0)

  cum <- sol[, "C"]
  incidence <- c(cum[1], diff(cum))

  out <- tibble::tibble(time = times)
  for (nm in setdiff(colnames(sol), "C")) out[[nm]] <- sol[, nm]
  out$C <- cum
  out$incidence <- incidence

  attr(out, "R0") <- R0
  attr(out, "model") <- if (seir) "SEIR" else "SIR"
  out
}

#' Basic Reproduction Number R0 (KK)
#'
#' @description Estimates the basic reproduction number `R0` by one of three
#'   approaches: directly from transmission parameters, from the exponential
#'   growth rate of an early epidemic, or from the final attack rate of a closed
#'   epidemic. For the growth-rate method the SIR relationship `R0 = 1 + r / gamma`
#'   is used, extended to `R0 = (1 + r / gamma)(1 + r / sigma)` when a latent rate
#'   `sigma` is supplied (SEIR).
#'
#' @param method One of `"params"`, `"growth"`, or `"final_size"`.
#' @param beta,gamma Transmission and recovery rates (method `"params"`).
#' @param r Exponential growth rate of the epidemic, per unit time (method
#'   `"growth"`).
#' @param sigma Latent progression rate; supplying it uses the SEIR growth
#'   relationship (method `"growth"`).
#' @param attack_rate Final proportion of the population infected, in (0, 1)
#'   (method `"final_size"`).
#'
#' @return One-row tibble with the estimated `R0` and the method used.
#'
#' @examples
#' kk_r0("params", beta = 0.5, gamma = 0.2)
#' kk_r0("growth", r = 0.15, gamma = 0.2)
#' kk_r0("final_size", attack_rate = 0.8)
#'
#' @export
kk_r0 <- function(method = c("params", "growth", "final_size"),
                  beta = NULL, gamma = NULL, r = NULL, sigma = NULL,
                  attack_rate = NULL) {
  method <- match.arg(method)

  if (method == "params") {
    if (is.null(beta) || is.null(gamma)) stop("`beta` and `gamma` are required.")
    r0 <- beta / gamma
  } else if (method == "growth") {
    if (is.null(r) || is.null(gamma)) stop("`r` and `gamma` are required.")
    r0 <- if (is.null(sigma)) 1 + r / gamma else (1 + r / gamma) * (1 + r / sigma)
  } else {
    if (is.null(attack_rate) || attack_rate <= 0 || attack_rate >= 1) {
      stop("`attack_rate` must be in (0, 1).")
    }
    r0 <- -log(1 - attack_rate) / attack_rate
  }

  tibble::tibble(R0 = r0, method = method)
}

#' Final Epidemic Size (KK)
#'
#' @description Solves the final-size relation `z = 1 - exp(-R0 * z)` for the
#'   total proportion of a closed, fully susceptible population that is infected
#'   over the course of an epidemic. When `R0 <= 1` the only solution is 0 (no
#'   epidemic). The herd-immunity threshold `1 - 1 / R0` is reported alongside.
#'
#' @param R0 Basic reproduction number.
#'
#' @return One-row tibble with `R0`, the final `attack_rate`, and the
#'   `herd_immunity` threshold.
#'
#' @examples
#' kk_final_size(2.5)
#'
#' @export
kk_final_size <- function(R0) {
  if (R0 <= 0) stop("`R0` must be positive.")
  if (R0 <= 1) {
    return(tibble::tibble(R0 = R0, attack_rate = 0, herd_immunity = 0))
  }
  # Solve z = 1 - exp(-R0 z) on (0, 1)
  f <- function(z) 1 - exp(-R0 * z) - z
  z <- stats::uniroot(f, interval = c(1e-8, 1 - 1e-10))$root
  tibble::tibble(
    R0 = R0,
    attack_rate = z,
    herd_immunity = 1 - 1 / R0
  )
}
