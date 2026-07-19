#!/usr/bin/env python3
"""Extract a robust representative petal colour from flower images.

The HSV mask is a configurable heuristic, not a trained segmentation model.
For each accepted mask the script reports median RGB (robust location), mean
RGB, mask size, and a simple quality flag. Median RGB is the recommended value
for downstream analyses; k-means with one cluster is exactly the arithmetic
mean and therefore adds no information.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path

import cv2
import numpy as np
import pandas as pd


@dataclass(frozen=True)
class MaskConfig:
    pink_low: tuple[int, int, int] = (130, 60, 60)
    pink_high: tuple[int, int, int] = (179, 255, 255)
    white_low: tuple[int, int, int] = (0, 0, 180)
    white_high: tuple[int, int, int] = (179, 60, 255)
    kernel_size: int = 7
    iterations: int = 3
    min_mask_pixels: int = 100
    min_mask_fraction: float = 0.001
    max_mask_fraction: float = 0.80


def petal_mask(image_bgr: np.ndarray, config: MaskConfig) -> np.ndarray:
    hsv = cv2.cvtColor(image_bgr, cv2.COLOR_BGR2HSV)
    pink = cv2.inRange(hsv, np.array(config.pink_low), np.array(config.pink_high))
    white = cv2.inRange(hsv, np.array(config.white_low), np.array(config.white_high))
    mask = cv2.bitwise_or(pink, white)
    kernel = np.ones((config.kernel_size, config.kernel_size), dtype=np.uint8)
    mask = cv2.morphologyEx(mask, cv2.MORPH_OPEN, kernel, iterations=config.iterations)
    return cv2.morphologyEx(mask, cv2.MORPH_CLOSE, kernel, iterations=config.iterations)


def summarize_image(path: Path, config: MaskConfig) -> dict[str, object]:
    image = cv2.imread(str(path), cv2.IMREAD_COLOR)
    if image is None or image.size == 0:
        return {"filename": path.name, "quality_flag": "read_error"}

    mask = petal_mask(image, config)
    pixels_bgr = image[mask > 0]
    mask_pixels = int(pixels_bgr.shape[0])
    mask_fraction = mask_pixels / float(image.shape[0] * image.shape[1])

    if mask_pixels < config.min_mask_pixels or mask_fraction < config.min_mask_fraction:
        return {
            "filename": path.name,
            "mask_pixels": mask_pixels,
            "mask_fraction": mask_fraction,
            "quality_flag": "mask_too_small",
        }
    if mask_fraction > config.max_mask_fraction:
        quality = "mask_too_large"
    else:
        quality = "ok"

    pixels_rgb = pixels_bgr[:, ::-1].astype(np.float64)
    median = np.median(pixels_rgb, axis=0)
    mean = np.mean(pixels_rgb, axis=0)
    return {
        "filename": path.name,
        "median_R": median[0],
        "median_G": median[1],
        "median_B": median[2],
        "mean_R": mean[0],
        "mean_G": mean[1],
        "mean_B": mean[2],
        "mask_pixels": mask_pixels,
        "mask_fraction": mask_fraction,
        "quality_flag": quality,
    }


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input_dir", type=Path)
    parser.add_argument("output_csv", type=Path)
    parser.add_argument("--pattern", default="*.png")
    parser.add_argument("--min-mask-pixels", type=int, default=100)
    return parser


def main() -> int:
    args = build_parser().parse_args()
    config = MaskConfig(min_mask_pixels=args.min_mask_pixels)
    files = sorted(path for path in args.input_dir.glob(args.pattern) if path.is_file())
    if not files:
        raise SystemExit(f"No images matched {args.pattern!r} in {args.input_dir}")

    table = pd.DataFrame(summarize_image(path, config) for path in files)
    args.output_csv.parent.mkdir(parents=True, exist_ok=True)
    table.to_csv(args.output_csv, index=False)
    print(table["quality_flag"].value_counts(dropna=False).to_string())
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
