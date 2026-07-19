source("R/analysis_core.R")

# Row-safe extraction: invalid middle row must not shift later values.
d <- data.frame(sample = letters[1:4], longitude = c(1, NA, 3, 4), latitude = c(10, 20, 30, 40))
extractor <- function(points) data.frame(environment = points$longitude * 100 + points$latitude)
out <- row_safe_extract(d, extractor)
stopifnot(identical(out$sample, d$sample))
stopifnot(out$environment[1] == 110, is.na(out$environment[2]), out$environment[3] == 330, out$environment[4] == 440)

# Continuous suitability and thresholded potential richness are distinct.
b <- data.frame(sp1 = c(0.8, NA, 0.2), sp2 = c(0.3, NA, 0.9))
m <- compute_bombus_metrics(b, c("sp1", "sp2"), thresholds = c(sp1 = 0.5, sp2 = 0.5))
stopifnot(all.equal(m$bombus_suitability_sum, c(1.1, NA, 1.1), check.attributes = FALSE))
stopifnot(all.equal(m$potential_species_richness, c(1, NA, 1), check.attributes = FALSE))

# Variance allocation must retain covariance and sum exactly to one.
f <- c(-1, 0, 1, 2)
s <- c(-0.5, 0, 0.5, 1)
v <- covariance_aware_variance_allocation(f, s, residual_variance = 0.25)
stopifnot(abs(sum(v$proportion) - 1) < 1e-12)
stopifnot(v$covariance_fixed_spatial[1] > 0)

# Colour response does not reuse algebraically dependent chroma as an independent indicator.
c <- make_colour_responses(data.frame(a = c(2, 3), L = c(80, 60)))
stopifnot(identical(c$pigment_redness, c(2, 3)), identical(c$pigment_darkness, c(-80, -60)))

cat("analysis_core tests passed\n")
