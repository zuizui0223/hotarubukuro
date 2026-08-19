#!/usr/bin/env python3
"""Keep the threshold-event family in Appendix S6 and out of the Main story.

The continuous all-cell isolation analysis is the sole Main human-context axis.
This script is deliberately idempotent: it removes the 16-site event narrative
from the Abstract, Introduction, Methods, Results, Discussion and Figure 4,
while preserving the complete event analysis and numerical locks in Appendix
S6. It also synchronises validators and internal paper maps with that division
of labour.
"""

from __future__ import annotations

import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def words(text: str) -> list[str]:
    return re.findall(r"\b[\wÀ-ž*'’-]+\b", text)


def write_if_changed(path: Path, text: str) -> None:
    current = path.read_text(encoding="utf-8")
    if current != text:
        path.write_text(text, encoding="utf-8")


def replace_once_if_present(text: str, old: str, new: str, label: str) -> str:
    if old in text:
        count = text.count(old)
        if count != 1:
            raise RuntimeError(f"Expected one {label}; found {count}")
        return text.replace(old, new, 1)
    if new in text or (new == "" and old not in text):
        return text
    raise RuntimeError(f"Neither legacy nor supplementary-only form found for {label}")


def remove_section(text: str, start_heading: str, end_heading: str, label: str) -> str:
    if start_heading not in text:
        return text
    pattern = rf"\n{re.escape(start_heading)}\n.*?(?=\n{re.escape(end_heading)})"
    updated, count = re.subn(pattern, "", text, count=1, flags=re.DOTALL)
    if count != 1:
        raise RuntimeError(f"Expected one {label}; found {count}")
    return updated


def update_main_manuscript() -> tuple[int, int]:
    path = ROOT / "submission/jbi/JBI_main_manuscript_anonymized.md"
    text = path.read_text(encoding="utf-8")

    text = replace_once_if_present(
        text,
        "**Methods:** We converted 1,922 author-screened YAMAP photographs into pigmentation state and pigmented-only intensity. INLA-SPDE models separated environmental associations from residual geography, and 67 independently fixed white-pigmented boundaries supplied the local bumblebee comparison. For human context, we measured distance to the nearest occurrence of the same colour in all 1,305 cells, scaled it by local flower-cell spacing and replayed the geometry on 10,000 natural predictive maps. This continuous analysis was post hoc; a threshold event detector was retained only for calibration and field targeting.",
        "**Methods:** We converted 1,922 author-screened YAMAP photographs into pigmentation state and pigmented-only intensity. INLA-SPDE models separated environmental associations from residual geography, and 67 independently fixed white-pigmented boundaries supplied the local bumblebee comparison. For human context, we measured distance to the nearest occurrence of the same colour in all 1,305 cells, scaled it by local flower-cell spacing and replayed the geometry on 10,000 natural predictive maps. This analysis was post hoc, with its density correction and natural-map guardrails frozen before the validated run.",
        "Abstract Methods",
    )
    text = replace_once_if_present(
        text,
        " The raw white relationship did not retain its opposite sign after density correction. Sixteen event-defined populations were not excessive under natural maps.",
        " The raw white relationship did not retain its opposite sign after density correction.",
        "Abstract Results event sentence",
    )

    text = replace_once_if_present(
        text,
        "The broad natural template also makes the spatial geometry of human context testable without first declaring a small set of anomalies. For every 1-km flower cell, we measured distance to the nearest other cell with the same colour state. This continuous isolation measure is defined for all pigmented and white cells and requires no event radius, environmental caliper, minimum-neighbour rule or residual cutoff. We then asked whether isolation covaried with population exposure within each colour, whether that relationship changed after scaling by the local spacing of all flower cells, and whether the fitted natural model generated the same pattern across 10,000 predictive maps.",
        "The broad natural template also makes the spatial geometry of human context testable without first declaring a small set of anomalies. For every 1-km flower cell, we measured distance to the nearest other cell with the same colour state. This continuous isolation measure is defined for all pigmented and white cells and requires no event radius, environmental caliper, minimum-neighbour rule or residual cutoff. We then asked whether isolation covaried with population exposure within each colour, whether that relationship changed after scaling by the local spacing of all flower cells, and whether the fitted natural model generated the same pattern across 10,000 predictive maps. Because *C. punctata* is cultivated, planting or escape is one possible explanation, but the geometry is treated as contextual association rather than ancestry evidence.",
        "Introduction continuous human-context paragraph",
    )
    text = replace_once_if_present(
        text,
        "\nA separate relational event—a pigmented cell surrounded by nearby environmentally similar white cells—was retained for a different job. Replaying that event on natural maps calibrates visual surprise and provides reproducibly selected populations for field history, vouchers and genetic assignment. Because *C. punctata* is cultivated, planting, escape or introgression remain plausible provenance hypotheses, but neither continuous geometry nor event membership is treated as ancestry evidence.\n",
        "\n",
        "Introduction event paragraph",
    )
    text = replace_once_if_present(
        text,
        "The all-cell isolation analysis was developed after inspection of the earlier event-based human-context results and is therefore explicitly post hoc rather than a preregistered third prediction.",
        "The all-cell isolation analysis was developed after inspection of preliminary human-context patterns and is therefore explicitly post hoc rather than a preregistered third prediction.",
        "post hoc status sentence",
    )
    text = replace_once_if_present(
        text,
        " The event analysis remains a supplementary calibration and field-target procedure; an excess event count is not required for the continuous human-context question to be answerable.",
        "",
        "Introduction event-role sentence",
    )

    text = replace_once_if_present(
        text,
        "\nThe later departure analysis used the same eight abiotic axes and five approximately 100-km geographical folds (Roberts et al., 2017; Valavi et al., 2019).\n",
        "\n",
        "Methods departure bridge",
    )
    text = replace_once_if_present(
        text,
        "### 2.5 Continuous colour isolation and event-based field targets",
        "### 2.5 Continuous colour isolation and natural-geography guardrails",
        "Methods 2.5 heading",
    )
    text = replace_once_if_present(
        text,
        "\nFor continuity with the earlier workflow, we also retained the threshold-defined ecological event. A candidate required a pigmented focal cell with at least three neighbours within 10 km, root-mean-square environmental distance <=1 across the eight standardized abiotic axes, and only observed white flowers among eligible neighbours. Human variables were absent from the event rule. The identical detector was applied to 10,000 cross-fitted predictive maps to calibrate event count and fraction, after which predefined human contrasts were calculated. This event family now serves as supplementary calibration and as a reproducible selector of extreme field/provenance targets rather than the primary human-context estimand. Appendix S6 gives complete definitions and results.\n",
        "\nAn earlier threshold-event analysis is retained only in Appendix S6 as a supplementary sensitivity and does not contribute to the Main human-context inference.\n",
        "Methods event paragraph",
    )
    text = replace_once_if_present(
        text,
        " Event identities are fixed before their human variables are read.",
        "",
        "Reproducibility event-order sentence",
    )

    text = remove_section(
        text,
        "### 3.5 Predictive replay retained extreme populations as field targets",
        "## 4. Discussion",
        "Main Results event section",
    )
    text = replace_once_if_present(
        text,
        "The continuous analysis changes the human-context conclusion from a near-threshold result in 16 selected sites to an all-cell property of the pigmented distribution.",
        "The continuous analysis treats human context as an all-cell property of the pigmented distribution.",
        "Discussion 4.4 opening",
    )
    text = remove_section(
        text,
        "### 4.5 Event calibration defines targets without turning them into causes",
        "### 4.6 A spatially varying balance can maintain flower-colour polymorphism",
        "Main Discussion event section",
    )
    text = replace_once_if_present(
        text,
        "### 4.6 A spatially varying balance can maintain flower-colour polymorphism",
        "### 4.5 A spatially varying balance can maintain flower-colour polymorphism",
        "final Discussion heading",
    )
    text = replace_once_if_present(
        text,
        "The current data do not directly estimate fitness or provenance, but they identify the phenotype component, geographical scale, candidate process and population set for each causal test.",
        "The current data do not directly estimate fitness or provenance, but they identify the phenotype component, geographical scale and candidate process for each causal test.",
        "synthesis causal-test sentence",
    )
    text = replace_once_if_present(
        text,
        "; and event replay retained calibrated field targets without turning visual exceptions into causes",
        "",
        "final synthesis event clause",
    )

    forbidden = (
        "Sixteen pigmented cells",
        "16 selected sites",
        "16 sites anthropogenic",
        "### 3.5 Predictive replay retained extreme populations as field targets",
        "### 4.5 Event calibration defines targets without turning them into causes",
        "calibrated provenance targets",
    )
    present = [token for token in forbidden if token in text]
    if present:
        raise RuntimeError("Main manuscript still contains event-family Main language: " + ", ".join(present))

    write_if_changed(path, text)
    abstract = text.split("## Abstract", 1)[1].split("**Keywords:**", 1)[0]
    main_body = text.split("## 1. Introduction", 1)[1].split("## Acknowledgements", 1)[0]
    return len(words(abstract)), len(words(main_body))


def update_figure_documents() -> None:
    captions_path = ROOT / "submission/jbi/JBI_main_figure_captions.md"
    captions = captions_path.read_text(encoding="utf-8")
    captions = replace_once_if_present(
        captions,
        " The 16 event-defined field/provenance targets and their calibration are retained in Appendix S6.",
        "",
        "Figure 4 event sentence",
    )
    if "16 event-defined" in captions:
        raise RuntimeError("Figure captions still expose the 16-event family")
    write_if_changed(captions_path, captions)

    plan_path = ROOT / "submission/jbi/JBI_main_figure_plan.md"
    plan = plan_path.read_text(encoding="utf-8")
    plan = replace_once_if_present(
        plan,
        "**Handoff to Figure 4:** after Broad environment, coherent residual geography and a localized Bombus hypothesis, what should count as a genuinely exceptional colour configuration?",
        "**Handoff to Figure 4:** after Broad environment, coherent residual geography and a localized Bombus hypothesis, does human context alter the geometry of the pigmented state beyond natural geography and local sampling density?",
        "Figure 3 handoff",
    )
    plan = replace_once_if_present(
        plan,
        "\n**Supporting calibration:** the former 16-event analysis moves to Appendix S6 as an alternative estimand and a reproducible selector of extreme field/provenance targets.\n",
        "\nThe threshold-event family is documented only in Appendix S6 and is outside the Main figure architecture.\n",
        "Figure 4 supporting-calibration paragraph",
    )
    plan = replace_once_if_present(
        plan,
        "**Ending:** the figures identify where physiology, population history, realized pollination and provenance should be tested directly, while preserving the claim ceiling that a human-context overlay is not evidence of horticultural origin.",
        "**Ending:** the figures identify where physiology, population history, realized pollination and human-context mechanisms should be tested directly, while preserving the claim ceiling that a human-context overlay is not evidence of horticultural origin.",
        "Figure-plan ending",
    )
    if "16-event" in plan or "field/provenance targets" in plan:
        raise RuntimeError("Main figure plan still assigns the event family a narrative role")
    write_if_changed(plan_path, plan)


def update_translated_abstract() -> None:
    path = ROOT / "submission/jbi/JBI_translated_abstract_ja.md"
    text = path.read_text(encoding="utf-8")
    text = replace_once_if_present(
        text,
        "従来の16地点イベント検出器は、主検定ではなく現地・provenance調査対象を選ぶ補助的解析として残した。",
        "",
        "Japanese Methods event sentence",
    )
    text = replace_once_if_present(
        text,
        "16地点の局所イベント数は自然予測地図下で過剰ではなかった。",
        "",
        "Japanese Results event sentence",
    )
    text = replace_once_if_present(
        text,
        "これは園芸由来を証明せず、植栽史・標本・局所環境・遺伝解析を向ける仮説と対象個体群を具体化する。",
        "これは園芸由来を証明せず、植栽史・標本・局所環境・遺伝解析によって検証すべき仮説を具体化する。",
        "Japanese conclusion",
    )
    text = re.sub(r"[。]\s*\n", "。\n", text)
    if "16地点" in text:
        raise RuntimeError("Japanese translated abstract still contains the 16-site family")
    write_if_changed(path, text)


def update_paper_maps() -> None:
    readme_path = ROOT / "paper/README.md"
    readme = readme_path.read_text(encoding="utf-8")
    readme = replace_once_if_present(
        readme,
        "5. **Event calibration and field targets:** the earlier 16 pigmented-among-white events are not excessive under natural maps (count P=0.27897; fraction P=0.12609). Their 5-km local population contrast is +0.06744 (directional P=0.00800; global maxT P=0.05479). They remain reproducible field/provenance targets, not the foundation of the main human-origin claim.\n",
        "",
        "paper README event item",
    )
    readme = replace_once_if_present(
        readme,
        "It identifies where human context overlays the spatial geometry of pigmentation and sharpens the populations and measurements needed for field history, voucher and genomic tests.",
        "It identifies where human context overlays the spatial geometry of pigmentation and sharpens the hypotheses and measurements needed for field history, voucher and genomic tests.",
        "paper README interpretation",
    )
    note = (
        "The threshold-defined event family is retained only in Appendix S6 as a "
        "supplementary sensitivity; it is not part of the Main narrative or Figure 4.\n\n"
    )
    marker = "## Interpretation ceiling\n"
    if note not in readme:
        if marker not in readme:
            raise RuntimeError("paper README interpretation marker is missing")
        readme = readme.replace(marker, note + marker, 1)
    write_if_changed(readme_path, readme)

    map_path = ROOT / "paper/analysis-map.md"
    analysis_map = map_path.read_text(encoding="utf-8")
    analysis_map = replace_once_if_present(
        analysis_map,
        "| Event calibration | Are restrictive pigmented-among-white configurations excessive, and which populations should be revisited? | 16 event-defined cells | Supporting / field targeting | count P=0.27897; fraction P=0.12609; local population contrast +0.06744, global maxT P=0.05479 |",
        "| Event calibration | Are restrictive pigmented-among-white configurations excessive? | 16 event-defined cells | Appendix S6 only / outside Main inference | count P=0.27897; fraction P=0.12609; local population contrast +0.06744, global maxT P=0.05479 |",
        "analysis-map event row",
    )
    analysis_map = replace_once_if_present(
        analysis_map,
        "The retained event scripts and v20-v22 results remain active because they answer different questions: frequency calibration, local candidate-versus-white contrast and reproducible selection of provenance targets. They no longer define the primary human-context estimand.",
        "The retained event scripts and v20-v22 results remain reproducible, but their presentation is confined to Appendix S6. They do not appear in the Main Results, Discussion or Figure 4 and do not define the human-context conclusion.",
        "analysis-map event role",
    )
    write_if_changed(map_path, analysis_map)


def update_checklist(abstract_n: int, body_n: int) -> None:
    path = ROOT / "submission/jbi/JBI_submission_checklist.md"
    text = path.read_text(encoding="utf-8")
    text = replace_once_if_present(
        text,
        "- The conceptual advance is **scale-matched attribution**: new trait data establish the national pattern; INLA-SPDE separates measured environment from coherent residual geography; the environment-derived Bombus hypothesis is tested first at local colour boundaries; and continuous same-colour isolation is tested against local sampling density and 10,000 natural maps before event-defined populations are retained as field targets.",
        "- The conceptual advance is **scale-matched attribution**: new trait data establish the national pattern; INLA-SPDE separates measured environment from coherent residual geography; the environment-derived Bombus hypothesis is tested first at local colour boundaries; and continuous same-colour isolation is tested against local sampling density and 10,000 natural maps.",
        "checklist conceptual advance",
    )
    text = re.sub(
        r"- The paper is achievement-forward without inflating causality\..*?\n- Methods are described",
        "- The paper is achievement-forward without inflating causality. Positive, heterogeneous and null results provide distinct outputs: response-specific geography, localized biotic inference and a guarded pigmented human-context overlay. The threshold-event family is restricted to Appendix S6.\n- Methods are described",
        text,
        count=1,
        flags=re.DOTALL,
    )
    text = re.sub(
        r"Current Introduction-through-Discussion count = \*\*[\d,]+ words\*\* by repository validator\.",
        f"Current Introduction-through-Discussion count = **{body_n:,} words** by repository validator.",
        text,
    )
    text = re.sub(
        r"Current abstract = \*\*\d+ words\*\* by repository validator\.",
        f"Current abstract = **{abstract_n} words** by repository validator.",
        text,
    )
    text = re.sub(
        r"- \*\*Departure killing part:\*\*.*?\n",
        "",
        text,
        count=1,
    )
    text = replace_once_if_present(
        text,
        "- **Synthesis:** the Discussion converges on a spatially changing physiological/reproductive cost-benefit model, modified by history and occasional human movement.",
        "- **Synthesis:** the Discussion converges on a spatially changing physiological/reproductive cost-benefit model, modified by history and contemporary human context.",
        "checklist synthesis",
    )
    text = replace_once_if_present(
        text,
        "- No claim that the **674 pigmented cells** establish horticultural origin, or that the **16 event-defined field targets** are anthropogenic or more frequent than the finalized natural model expects.",
        "- No claim that the **674 pigmented cells** establish horticultural origin.\n- Appendix S6 alone retains the threshold-event calibration and 16-site list; neither appears in the Main text or Figure 4.",
        "checklist claim ceiling",
    )
    support_marker = "- Sources cited only in Supporting Information must remain in the supplementary reference sections rather than the Main list.\n"
    support_note = "- Appendix S6 contains the threshold-event calibration and 16-site list only; Main inference does not depend on them.\n"
    if support_note not in text:
        if support_marker not in text:
            raise RuntimeError("Supporting Information checklist marker is missing")
        text = text.replace(support_marker, support_marker + support_note, 1)
    write_if_changed(path, text)


def update_submission_validator() -> None:
    path = ROOT / "submission/jbi/validate_jbi_submission.py"
    text = path.read_text(encoding="utf-8")
    text = text.replace(
        '"### 3.5 Predictive replay retained extreme populations as field targets",',
        '"### 3.4 Continuous isolation revealed a pigmented human-context overlay",',
    )
    text = text.replace(
        '"### 4.6 A spatially varying balance can maintain flower-colour polymorphism",',
        '"### 4.5 A spatially varying balance can maintain flower-colour polymorphism",',
    )
    text = text.replace('        "Sixteen pigmented cells",\n', "")
    text = text.replace('        "calibrated field targets",\n', "")
    text = text.replace(', "16 event-defined"))', '))')
    text = text.replace(', "16地点"))', '))')
    text = text.replace('        "16 event-defined field targets",\n', "")

    marker = "for citation, reference in (\n"
    guard = '''for forbidden_main_token in (
    "Sixteen pigmented cells",
    "16 event-defined",
    "calibrated field targets",
    "### 3.5 Predictive replay retained extreme populations as field targets",
    "### 4.5 Event calibration defines targets without turning them into causes",
):
    if forbidden_main_token in text:
        fail(f"Threshold-event family leaked into Main manuscript: {forbidden_main_token}")

'''
    if guard not in text:
        if marker not in text:
            raise RuntimeError("submission validator citation marker is missing")
        text = text.replace(marker, guard + marker, 1)

    figure_guard_marker = 'if re.search(r"\\([A-D]\\)", figure_text):\n'
    figure_guard = '''if "16 event-defined" in figure_text or "field/provenance targets" in figure_text:
    fail("Threshold-event family leaked into Main-figure captions")
'''
    if figure_guard not in text:
        if figure_guard_marker not in text:
            raise RuntimeError("submission validator figure marker is missing")
        text = text.replace(figure_guard_marker, figure_guard + figure_guard_marker, 1)

    translated_guard_marker = 'if "17地点" in translated:\n'
    translated_guard = '''if "16地点" in translated:
    fail("Threshold-event family leaked into Japanese translated abstract")
'''
    if translated_guard not in text:
        if translated_guard_marker not in text:
            raise RuntimeError("submission validator translated-abstract marker is missing")
        text = text.replace(translated_guard_marker, translated_guard + translated_guard_marker, 1)

    write_if_changed(path, text)


def update_lock_contract() -> None:
    path = ROOT / "config/paper_pipeline.lock.json"
    lock = json.loads(path.read_text(encoding="utf-8"))
    for check in lock["alignment"]["checks"]:
        if check.get("label") == "paper overview mirrors continuous and event roles":
            check["label"] = "paper overview mirrors Main continuous role"
            check["patterns"] = [
                pattern for pattern in check["patterns"]
                if pattern not in {"16", "0\\.05479"}
            ]
    path.write_text(
        json.dumps(lock, ensure_ascii=False, separators=(",", ":")) + "\n",
        encoding="utf-8",
    )


def update_integration_note(abstract_n: int, body_n: int) -> None:
    path = ROOT / "reproducibility/continuous_colour_isolation_manuscript_integration_2026-08-18.md"
    text = f"""# Continuous colour-isolation manuscript integration

**Status:** the continuous all-cell isolation analysis is the sole Main human-context axis. The threshold-event family and its 16-site list are retained only in Appendix S6.

The JBI narrative now assigns the analyses distinct roles:

- continuous all-cell isolation is the Main descriptive human-context geometry;
- density correction and 10,000 natural maps set its interpretation ceiling;
- the robust focal statement concerns the excess positive pigmented isolation-population relationship;
- the raw white sign reversal is not retained as a density-independent conclusion;
- the threshold-event family is absent from the Abstract, Main Introduction, Main Results, Main Discussion and Figure 4.

Current Main structure:

- structured Abstract: {abstract_n} words;
- Introduction-through-Discussion: {body_n:,} words;
- Results end with continuous isolation (Section 3.4);
- Discussion synthesis follows the human-overlay section directly (Section 4.5);
- Appendix S6 preserves the complete event definitions, numerical calibration and 16-site list for transparency and reproducibility.

Numerical source and claim lock:

- `reproducibility/continuous_colour_isolation_human_context_result_2026-08-18.md`;
- analysis workflow run `32116805570`;
- artifact `9317087893`;
- ZIP SHA-256 `6fd26d9a938b68d3f0c56512cd1620597c740d44ba91ab5a7ccbb9daa99d5386`.

This division keeps the Main conclusion focused on the all-cell human-context overlay without using a selected 16-site tail as a narrative endpoint.
"""
    write_if_changed(path, text)


def main() -> None:
    abstract_n, body_n = update_main_manuscript()
    update_figure_documents()
    update_translated_abstract()
    update_paper_maps()
    update_checklist(abstract_n, body_n)
    update_submission_validator()
    update_lock_contract()
    update_integration_note(abstract_n, body_n)
    print(f"event family demoted to Appendix S6; abstract={abstract_n} words; body={body_n} words")


if __name__ == "__main__":
    main()
