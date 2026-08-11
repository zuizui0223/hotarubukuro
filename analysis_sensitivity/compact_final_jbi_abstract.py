from pathlib import Path
import re

path = Path('submission/jbi/JBI_main_manuscript_anonymized.md')
text = path.read_text(encoding='utf-8')
start = text.index('## Abstract')
end = text.index('**Keywords:**', start)
abstract = '''## Abstract

**Aim:** Geographical trait variation can reflect abiotic environment, population history, biotic interactions and human movement that share spatial structure. We asked how these processes organize a quantitative flower-colour polymorphism when each is tested at an ecologically defensible scale.

**Location:** Japan.

**Taxon:** *Campanula punctata* (Campanulaceae).

**Methods:** We converted author-screened GPS-linked YAMAP hiking photographs from 2023–2025 into pigmentation state and pigmented-only visible intensity. National INLA-SPDE models tested environmental hypotheses and residual geography; interactions and alternative hydroclimate/spatial formulations were retained only with geographically blocked support. We then tested Bombus-blind white–pigmented transitions within 5 km against occurrence-referenced availability of *Bombus ardens* and *B. diversus*. Finally, local pigmented departures were calibrated on repeated natural predictive maps before human context was examined.

**Results:** Among 1,922 photographs, pigmentation probability declined toward warmer climates. Pigmented-only intensity depended on Temperature PC1 × temperature seasonality, with a stronger warm-climate decline in more thermally seasonal regions, and was weaker toward wetter and more rugged environments. Across 67 sharp transitions, mean focal-bumblebee availability was higher on the pigmented side, but the effect was magnitude-driven and not robust across exposure or scale sensitivities. Seventeen local departures were not excessive under natural predictive maps. Short-scale population exposure was the leading human feature, but corrected support depended on the environmental matching definition.

**Main conclusions:** Flower-colour geography combines a cool-climate pigmentation-state gradient, climate-context-dependent intensity and unresolved regional structure rather than a single high-elevation darkening process. Any pollinator contribution appears weak and local. Human-context results identify matching-sensitive provenance hypotheses, not anthropogenic origins. The study shows why integrative trait biogeography should connect processes across scales rather than collapse them into one geographical regression.

'''
count = len(re.findall(r"\b[\wÀ-ž*'’-]+\b", abstract))
if count > 300:
    raise SystemExit(f'Abstract still too long: {count}')
text = text[:start] + abstract + text[end:]
path.write_text(text, encoding='utf-8')
print('abstract words', count)
