# ============================================================
# CONSTANTS & CONFIGURATION
# ============================================================

CONSTANTS <- list(
              OUTLIER_IQR_MULTIPLIER = 1.5,
              DEFAULT_CONF_LEVEL = 0.95,
              TRIM_PERCENTAGE = 0.1,
              HUBER_C_CONSTANT = 1.345,
              HUBER_MAX_ITER = 100,
              HUBER_TOLERANCE = 1e-6,
              SMALL_CONSTANT = 1e-10,
              MIN_SAMPLE_FOR_TESTS = list(
                            shapiro = 3,
                            ks = 5001,
                            anderson = 7,
                            garch = 30,
                            hurst = 20,
                            acf_pacf = 3,
                            adf_kpss = 10
              )
)
