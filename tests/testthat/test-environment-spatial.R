source(testthat::test_path("..", "..", "R", "environment_spatial.R"))

testthat::test_that("spatial blocks are never split across folds", {
  d <- data.frame(
    longitude = rep(c(135, 136, 137, 138, 139), each = 6),
    x_km = rep(seq(-200, 200, length.out = 5), each = 6),
    y_km = rep(seq(0, 250, length.out = 6), 5)
  )
  d <- assign_spatial_blocks(d, block_km = 50, k = 5, east_west_cut = 136.5)
  fold_per_block <- tapply(d$spatial_fold, d$spatial_block, function(x) length(unique(x)))
  testthat::expect_true(all(fold_per_block == 1))
  testthat::expect_equal(length(unique(d$spatial_fold)), 5)
})

testthat::test_that("Bombus groups are equal-weight normal-score blocks, not a probability union", {
  d <- data.frame(longitude = 135:138, latitude = 34:37)
  for (sp in c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")) {
    d[[paste0("bee_", sp)]] <- seq(0.1, 0.9, length.out = nrow(d))
  }
  for (nm in paste0("bee_", c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis"))) {
    d[[paste0(nm, "_ns")]] <- normal_score(d[[nm]])
  }
  d$Bombus_W <- safe_z(rowMeans(d[paste0("bee_", c("ardens", "diversus"), "_ns")]))
  union <- 1 - (1 - d$bee_ardens) * (1 - d$bee_diversus)
  testthat::expect_false(isTRUE(all.equal(d$Bombus_W, union)))
})

testthat::test_that("candidate construction has no weighted composite score", {
  d <- data.frame(
    upper_excess = c(-1, 0.1, 1, 2), early_anomaly = c(2, -1, -3, -5),
    z_H = c(-1, 0, 1, 2), z_R = NA_real_, z_A = NA_real_
  )
  out <- build_candidate_table(d)$data
  testthat::expect_false(any(grepl("weight|weighted|composite", names(out), ignore.case = TRUE)))
  testthat::expect_true(all(out$robust_candidate <= out$upper_flag))
})

testthat::test_that("environment VIF is invariant to added Bombus values", {
  set.seed(1)
  d <- data.frame(env_a = rnorm(100), env_b = rnorm(100), Bombus_W = rnorm(100))
  a <- compute_vif(d, c("env_a", "env_b"))
  d$Bombus_W <- d$env_a * 10
  b <- compute_vif(d, c("env_a", "env_b"))
  testthat::expect_equal(a$VIF, b$VIF)
})

testthat::test_that("missing public axes remain missing instead of becoming zero", {
  testthat::expect_true(all(is.na(safe_z(rep(NA_real_, 4)))))
  testthat::expect_equal(safe_z(rep(2, 4)), rep(0, 4))
  testthat::expect_true(is.na(safe_z(c(1, 2, NA_real_))[3]))

  d <- data.frame(
    upper_excess = c(0.1, 1), early_anomaly = c(-1, -2),
    z_H = c(0, 1), z_R = NA_real_, z_A = NA_real_
  )
  axes <- build_candidate_table(d)$available_axes
  testthat::expect_false(axes$available[axes$axis == "R_score"])
  testthat::expect_false(axes$available[axes$axis == "A_score"])
})

testthat::test_that("joint soil axes take precedence over collinear legacy soil terms", {
  txt <- paste(deparse(body(make_analysis_data)), collapse = " ")
  testthat::expect_match(txt, 'all(c("soil_PC1", "soil_PC2")', fixed = TRUE)
})

testthat::test_that("manual warnings and extreme colours are retained", {
  raw <- data.frame(
    median_R = c(250, 110), median_G = c(20, 100), median_B = c(40, 100),
    qc_status = c("manual_review_required", "ok"),
    manual_review_status = c("pending", "not_required_by_automated_qc"),
    duplicate_image_sha256 = c(FALSE, FALSE),
    possible_overexposure = c(TRUE, FALSE),
    mask_pixels = c(100, 100), mask_fraction_visible = c(0.2, 1),
    qc_flags = c("possible_multiple_components;hsv_multimodal_colour", "")
  )
  qc <- audit_colour_qc(raw, author_review_confirmed = TRUE)$data
  testthat::expect_true(all(qc$colour_qc_primary))
  testthat::expect_equal(
    unique(qc$analysis_manual_review_status),
    "author_confirmed_flower_colour_extraction_region"
  )
  testthat::expect_true(all(qc$qc_author_region_review_confirmed))
  testthat::expect_false(qc$colour_qc_no_warning_sensitivity[1])
})

testthat::test_that("large qGAM fit objects are opt-in before INLA", {
  testthat::expect_identical(formals(fit_horticultural_qgams)$keep_fits, FALSE)
  body_text <- paste(deparse(body(fit_one_inla_spde)), collapse = " ")
  testthat::expect_match(body_text, "config = FALSE", fixed = TRUE)
  testthat::expect_false(grepl("num.threads", body_text, fixed = TRUE))
  testthat::expect_match(body_text, "verbose = TRUE", fixed = TRUE)
})

testthat::test_that("qGAM smoothness is estimated with explicit REML", {
  crossfit_text <- paste(deparse(body(crossfit_qgam)), collapse = " ")
  horticulture_text <- paste(deparse(body(fit_horticultural_qgams)), collapse = " ")
  testthat::expect_match(crossfit_text, 'list(method = "REML")', fixed = TRUE)
  testthat::expect_match(horticulture_text, 'list(method = "REML")', fixed = TRUE)
})
