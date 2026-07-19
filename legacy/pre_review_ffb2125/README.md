# Pre-review scripts (legacy; do not execute)

These files preserve the repository state immediately before the reproducibility
review, at commit `ffb21256745a089aaae39e87f350076047b7aebd`. They are evidence
of the previous methods only. They are excluded from tests and must not be used
to regenerate current results.

Material defects include local absolute paths, row-order raster joins, a
one-cluster k-means colour mean, unsupported Hue bounds, mixed photograph/site
grain, the CFA `Pigment` construct, a continuous SDM sum labelled richness,
unreviewed SPDE/qGAM stages, and absent SDM-generation inputs. `Code_S1.py` is
actually CP932 RTF rather than Python, and `Code_S3.R` contains R Markdown.

The archive copies are text-identical to their Git blobs except that
`Code_S1.py`, `Code_S3.Rmd`, and `Code_S4.R` have a final newline added by the
repository patching system. `SOURCE_BLOBS.csv` records both source and archive
hashes. Exact source bytes remain recoverable from the immutable blob IDs, for
example:

```bash
git cat-file blob 47457832de765aa05c22155b061d37b490e3d7b5 \
  > /tmp/legacy-Code_S1.py
```

Current entry points are in `scripts/` and reusable reviewed functions are in
`R/`. The current top-level `Code_S*` files are compatibility entry points, not
these legacy implementations.
