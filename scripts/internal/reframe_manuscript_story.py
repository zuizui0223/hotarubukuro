#!/usr/bin/env python3
from pathlib import Path
import re

path = Path("manuscript/ecology-and-evolution-manuscript.md")
text = path.read_text(encoding="utf-8")

# Title: name the three explanatory layers without implying anthropogenic causation.
text = re.sub(
    r"^# .*?$",
    "# Layered flower-colour geography in *Campanula punctata*: abiotic structure, local bumblebee limitation and anthropogenic context",
    text,
    count=1,
    flags=re.M,
)

abstract = """Flower-colour geography can reflect several layers of process, from broad abiotic gradients to local biotic interactions and, finally, departures associated with human-modified landscapes. These layers are difficult to interpret when colour is treated as a single continuous response or when every predictor is placed in one national model. We assembled an author-reviewed, route-linked photographic dataset of *Campanula punctata* from YAMAP and analysed 1,909 observations in a staged framework. A response-blind Gaussian mixture separated 955 white-like and 954 pigmented flowers at CIELAB a*=5.0202; 125 low-confidence classifications were flagged but retained. We therefore analysed two responses: pigmentation presence and visible intensity conditional on pigmentation. First, national INLA-SPDE models established the broad natural geography. Temperature was negatively associated with both components, conditional intensity also varied with topography, and residual spatial ranges were approximately 131 km for pigmentation presence and 61 km for conditional intensity. Five geographically blocked folds yielded AUC=0.863 for pigmentation presence and RMSE=0.912 for conditional intensity. We then zoomed to a local biotic hypothesis rather than treating bumblebee SDMs as national causal predictors. Among one-to-one pairs within 25 km and matched on environmental context, 22 lower-third pairs had 0.223 higher pigmented share where at least one focal *Bombus* species had moderate predicted availability than where all five species had low predicted availability (upper-tail predictive p=0.017). Because the lower-third gate was adopted after exploratory design development, the complete threshold grid was retained and the across-grid BH q-value was 0.102; this is directionally coherent exploratory support, not a confirmatory test. Conditional intensity did not increase across the same gate (difference approximately -0.615; p=0.904), suggesting that predicted bumblebee availability may be more relevant to whether pigmentation is maintained than to how dark already pigmented flowers become. Finally, we asked what remained after the natural reference. Eighteen local pigmented isolates were identified, but their excess was not robust across held-out and joint posterior-predictive references; population and densely inhabited district context were suggestive but did not survive familywise correction. The resulting picture is layered rather than additive: abiotic environment and spatial structure define the national background, local bumblebee limitation may modify the maintenance of pigmentation, and residual local departures can be screened for anthropogenic context without being labelled anthropogenic in origin. Predicted *Bombus* support remains an availability proxy rather than visitation or selection pressure, and direct field measurements are required to test the proposed attraction mechanism."""
text = re.sub(
    r"(?s)(## Abstract\n\n).*?(\n\n\*\*Keywords:\*\*)",
    lambda m: m.group(1) + abstract + m.group(2),
    text,
    count=1,
)
text = text.replace(
    "**Keywords:** bumblebee limitation; digital phenotyping; flower colour; INLA-SPDE; pollinator availability; *Campanula punctata*",
    "**Keywords:** anthropogenic context; bumblebee limitation; digital phenotyping; flower colour; INLA-SPDE; pollinator availability; *Campanula punctata*",
)

old_intro_roadmap = """The paper is consequently organized around one measurement framework and one common natural reference rather than four independent analyses. First, we quantify the two-part flower-colour phenotype. Second, we estimate national environmental clines and continuous spatial structure and generate geographically cross-fitted natural predictive maps. Against this same reference, we then ask two bounded local questions: whether low predicted *Bombus* availability is associated with relaxation of pigmentation, and whether locally isolated pigmented cells occur more often than expected before their human context is described. This sequence makes the inferential hierarchy explicit: broad environment and space establish the national geography; the bumblebee analysis tests a local mechanistic hypothesis without claiming direct visitation; and human-context analysis remains a post-selection extension rather than evidence of horticultural origin."""
new_intro_roadmap = """Yet neither a broad natural baseline nor one focal biotic hypothesis should be expected to explain every local observation. The remainder is itself informative. A pigmented cell embedded in an otherwise white, environmentally similar neighbourhood marks a local departure from the broad natural expectation. If such departures are defined without using human variables, their landscape context can then be examined without defining the anomaly by the same anthropogenic predictors later used to characterize it. We therefore treat human influence as a third, bounded layer: not as a competing national driver, but as a way to characterize local departures left after the natural geography has been established.

The analysis consequently narrows the question step by step. First, we define the two-part flower-colour phenotype. Second, we establish the national abiotic and continuous spatial background and generate geographically cross-fitted natural predictive maps. Third, we zoom to environmentally similar nearby sites and ask whether low predicted *Bombus* availability corresponds to relaxation of pigmentation. Finally, we identify local pigmented departures from the natural reference and ask whether those pre-defined departures occupy distinctive anthropogenic contexts. The hierarchy is deliberate: abiotic environment and space set the broad stage; pollinator availability tests a local biotic mechanism; and human context is examined only after the remaining local departures have been identified independently of human variables."""
if old_intro_roadmap not in text:
    raise SystemExit("Could not find Introduction roadmap block")
text = text.replace(old_intro_roadmap, new_intro_roadmap, 1)

# Results: make each section answer the next question and explicitly hand off to the next layer.
text = text.replace(
    "### 3.3 | Local *Bombus* limitation was directionally associated with pigmentation state",
    "### 3.3 | After environmental matching, local *Bombus* limitation tracked pigmentation state",
    1,
)
text = text.replace(
    "Thus the active local evidence concerns the probability of being pigmented, not stronger visible pigmentation once pigmentation is present.\n\n### 3.4 | Local pigmented isolates were not robustly excessive across predictive references",
    "Thus the active local evidence concerns the probability of being pigmented, not stronger visible pigmentation once pigmentation is present. This local biotic pattern did not, however, exhaust the geography left by the natural reference. We next asked where pigmented cells remained locally discordant with otherwise similar white neighbourhoods.\n\n### 3.4 | The natural reference left 18 local pigmented departures, but their excess was not robust",
    1,
)
text = text.replace(
    "Because the apparent fraction excess did not persist across predictive references, the candidate set is retained for follow-up rather than interpreted as evidence that such isolates occur more often than expected naturally.\n\n### 3.5 | Human context was suggestive but not familywise significant",
    "Because the apparent fraction excess did not persist across predictive references, the candidate set is retained for follow-up rather than interpreted as evidence that such isolates occur more often than expected naturally. These 18 cells therefore serve as pre-defined local departures for a final question: do they occupy distinctive human-modified contexts?\n\n### 3.5 | The remaining local departures showed suggestive anthropogenic context",
    1,
)

# Discussion: replace the overview with a layered reading, then sharpen section labels.
new_41 = """### 4.1 | A layered explanation of flower-colour geography

The clearest way to read the analysis is from broad structure to increasingly local questions. The two-part phenotype first establishes what varies. National environment-plus-space models then define the broad natural geography. Against that background, the *Bombus* analysis asks whether a biologically motivated local contrast appears among environmentally similar nearby sites. Only after those natural layers are established do we ask where local pigmented departures remain and whether those departures occupy distinctive anthropogenic contexts.

This order matters because the three explanatory layers are not interchangeable covariates in a single regression. Abiotic environment and continuous space describe the national background. Predicted bumblebee availability addresses a local biotic hypothesis at a scale where broad environmental differences are reduced. Anthropogenic variables are used last, to characterize pre-defined local departures rather than to create them. The result is therefore a sequence of bounded questions, each opening the next, rather than four coequal objectives competing for a single causal interpretation.

The route-linked photographic sample makes this layered design possible because it provides broad geographical coverage, but it remains an opportunistic sample rather than a random survey. The inferential aim is consequently bounded hypothesis generation with explicit failure conditions, not population-wide causal estimation."""
text = re.sub(
    r"(?s)### 4\.1 \| One natural reference, not four disconnected analyses\n\n.*?(?=\n### 4\.2 \|)",
    new_41 + "\n",
    text,
    count=1,
)
text = text.replace(
    "### 4.2 | Pigmentation state and conditional intensity have different geographical signatures",
    "### 4.2 | The broad layer: abiotic and spatial structure differs between colour components",
    1,
)
text = text.replace(
    "### 4.3 | The *Bombus* hypothesis is intentionally local",
    "### 4.3 | The biotic layer must be tested locally",
    1,
)

new_45 = """### 4.5 | Residual local departures point toward anthropogenic context, not provenance

After the broad natural geography and the local bumblebee hypothesis were considered, a small set of locally discordant pigmented cells remained worth examining. These cells were defined without human variables. That ordering is essential: population density, densely inhabited districts, roads or land use did not decide which observations counted as departures; they were inspected only after the candidate set had been fixed.

The evidence for an excess of these departures was itself reference-dependent. Candidate fraction lay near the upper tail of the held-out cross-fitted reference but was compatible with the full-data joint posterior-predictive reference. The 18 cells should therefore not be read as proof that an additional process is required. They are better treated as locations where the natural model gives us a concrete reason to look more closely.

That final look produced a suggestive but incomplete anthropogenic signal. Population and DID contrasts pointed in the expected direction, yet neither survived the relevant familywise correction. Human-modified landscapes can also correlate with hiking access and observation opportunity, so the remaining association cannot establish horticultural introduction, escape or introgression. Its value is narrower but still useful: it characterizes where some local departures occur and converts an otherwise vague horticultural story into a finite set of testable field targets. Demonstrating provenance would require planting histories, vouchers and population-genetic comparison with surrounding wild populations and horticultural material."""
text = re.sub(
    r"(?s)### 4\.5 \| Human-context analysis remains a separate follow-up problem\n\n.*?(?=\n### 4\.6 \|)",
    new_45 + "\n",
    text,
    count=1,
)

conclusion = """## 5 | Conclusions

The geography of *C. punctata* flower colour is best understood layer by layer. First, the phenotype itself contains two separable components: whether visible pigmentation is expressed and how intense that pigmentation becomes once expressed. Second, at the national scale, abiotic environment and continuous spatial structure provide the dominant background for both components. Third, when the analysis zooms to environmentally similar nearby sites, predicted bumblebee limitation shows a large and directionally coherent association with pigmentation state, but not with conditional intensity. This pattern is consistent with the possibility that bumblebee availability affects the benefit of maintaining a pigmented floral signal rather than simply selecting for progressively darker flowers.

The story does not end there. A small set of local pigmented departures remained after comparison with the natural predictive reference. Their apparent excess was not robust across predictive references, and their associations with population and DID context were suggestive rather than familywise significant. These observations therefore do not demonstrate anthropogenic origin. Instead, they define the final layer of the framework: natural structure first, a local biotic hypothesis second, and anthropogenic characterization only for the residual departures that remain worth investigating.

Taken together, the study does not support a single-driver explanation of flower-colour geography. It provides a staged map of where different questions become informative. Broad abiotic and spatial structure sets the background; local pollinator context may modify whether pigmentation is maintained; and residual departures identify where direct field, provenance and genetic work can most efficiently test human influence. Predicted *Bombus* support is still only an availability proxy, so species-resolved visitation, pollen transfer, reproductive fitness and standardized flower reflectance are required for a causal test of the proposed attraction mechanism."""
text = re.sub(
    r"(?s)## 5 \| Conclusions\n\n.*?(?=\n## Data Accessibility Statement)",
    conclusion + "\n",
    text,
    count=1,
)

text = text.replace(
    "**Figure 4. Repeated local-isolate events and post-selection human context.**",
    "**Figure 4. Residual local departures and their anthropogenic context.**",
    1,
)

path.write_text(text, encoding="utf-8")
print(f"Reframed manuscript story: {path}")
