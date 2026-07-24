import numpy as np
import pytest

from scripts.extract_color import build_parser, srgb_to_cielab


def test_srgb_to_cielab_reference_neutrals() -> None:
    lab = srgb_to_cielab(
        np.asarray([[255, 255, 255], [0, 0, 0]], dtype=np.uint8)
    )

    np.testing.assert_allclose(lab[0], [100.0, 0.0, 0.0], atol=0.02)
    np.testing.assert_allclose(lab[1], [0.0, 0.0, 0.0], atol=1e-12)


def test_srgb_to_cielab_rejects_invalid_channel_shape() -> None:
    with pytest.raises(ValueError, match="final dimension"):
        srgb_to_cielab(np.asarray([0, 1], dtype=float))


def test_colour_cli_uses_stable_public_options() -> None:
    options = build_parser().format_help()

    assert "--input-dir" in options
    assert "--input-workbook" in options
    assert "--qc-dir" in options
