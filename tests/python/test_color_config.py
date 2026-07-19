import inspect
import json
from pathlib import Path

from scripts import extract_color_v2 as colour


def test_documented_colour_configuration_matches_code_defaults():
    root = Path(__file__).resolve().parents[2]
    config = json.loads((root / "config" / "color_extraction_v2.json").read_text())
    mask = config["mask"]
    signature = inspect.signature(colour.make_petal_mask)

    assert config["extraction_version"] == colour.EXTRACTION_VERSION
    for name in (
        "alpha_threshold",
        "pink_hue_low",
        "pink_hue_high",
        "red_hue_high",
        "colour_saturation_min",
        "colour_value_min",
        "white_saturation_max",
        "white_value_min",
        "morph_kernel",
    ):
        config_name = "morph_kernel_px" if name == "morph_kernel" else name
        assert signature.parameters[name].default == mask[config_name]

    peak = config["joint_lab_peak"]
    assert peak["lab_bin_width"] == colour.PEAK_LAB_BIN_WIDTH
    assert peak["delta_e_radius"] == colour.PEAK_DELTA_E_RADIUS
    assert peak["second_peak_min_delta_e"] == colour.PEAK_SECOND_MIN_DELTA_E
    assert peak["multimodal_ratio_threshold"] == colour.PEAK_MULTIMODAL_THRESHOLD
    assert peak["low_peak_fraction_threshold"] == colour.PEAK_LOW_FRACTION_THRESHOLD

    filtered = config["exposure_filtered_peak"]
    assert filtered["minimum_pixels"] == colour.EXPOSURE_FILTERED_MIN_PIXELS
    assert filtered["minimum_retained_fraction"] == colour.EXPOSURE_FILTERED_MIN_FRACTION

    qc = config["qc"]
    assert qc["low_mask_coverage_threshold"] == colour.LOW_MASK_COVERAGE_THRESHOLD
    assert qc["near_white_fraction_threshold"] == colour.NEAR_WHITE_FRACTION_THRESHOLD
    assert qc["all_channels_clipped_threshold"] == colour.ALL_CHANNELS_CLIPPED_THRESHOLD
    assert qc["alpha_hsv_peak_disagreement_delta_e"] == colour.ALPHA_HSV_PEAK_DISAGREEMENT_THRESHOLD
