from pathlib import Path

p = Path('submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md')
text = p.read_text(encoding='utf-8')

old = '''Model extension was deliberately conservative. A new environmental term or interaction was eligible for promotion only when it had a defensible ecological interpretation, acceptable collinearity, improved geographically blocked predictive loss, a positive spatial-block bootstrap interval and improvement in at least four of five response-blind geographical folds. WAIC or a concentrated posterior alone was insufficient.'''
new = '''Model extension was deliberately conservative. A new environmental term or interaction was eligible for promotion only when it had a defensible ecological interpretation, acceptable collinearity, improved geographically blocked predictive loss, a positive spatial-block bootstrap interval and improvement in at least four of five response-blind geographical folds. WAIC or a concentrated posterior alone was insufficient. VIF was treated as a graded diagnostic rather than a universal deletion rule: values <5 were preferred; values of 5–10 required explicit stability of the focal coefficient, blocked geographic transfer and spatial hyperparameters; values >10 prevented promotion without exceptional mechanistic and predictive evidence. This final adjudication is intentionally stricter than treating the historical VIF=10 screen as a sufficient condition, while preserving the original screen specification as provenance.'''
if text.count(old) != 1:
    raise SystemExit(f'Expected one model-extension paragraph, found {text.count(old)}')
text = text.replace(old, new)

old2 = '''The mechanism screen identified a transferable Temperature PC1 × temperature-seasonality interaction. The interaction posterior was negative (mean -0.204; 95% CrI -0.302 to -0.107; mechanism-screen BH = 0.00043), WAIC improved by approximately 5.9 units relative to the additive intensity model, held-out squared error improved in four of five folds, the spatial-block bootstrap interval remained above zero and maximum VIF was approximately 6.34.'''
new2 = '''The mechanism screen identified a transferable Temperature PC1 × temperature-seasonality interaction. The interaction posterior was negative (mean -0.204; 95% CrI -0.302 to -0.107; mechanism-screen BH = 0.00043), WAIC improved by approximately 5.9 units relative to the additive intensity model, held-out squared error improved in four of five folds, and the spatial-block bootstrap interval remained above zero. Maximum model VIF was approximately 6.34, placing the model in the moderate 5–10 diagnostic band rather than the preferred <5 band. Crucially, the retained interaction itself had VIF 1.66; the larger values belonged to Temperature PC1 (6.34) and Soil PC1 (5.23), reflecting shared national geography among additive axes. Removing the East/West structural adjustment lowered the maximum VIF below 5 but worsened WAIC by about 5.9 units and did not provide robust transfer gain, so terms were not deleted solely to cross an arbitrary VIF=5 threshold.'''
if text.count(old2) != 1:
    raise SystemExit(f'Expected one intensity VIF paragraph, found {text.count(old2)}')
text = text.replace(old2, new2)

p.write_text(text, encoding='utf-8')
print('patched', p)
