# 査読者視点の解析レビュー v11

## 総合判定

現段階の解析は **限定付きで共有可能（share with caveats）** である。旧設計の
「全花の連続 a* を色素量として扱う」部分は大幅修正が必要だったが、v11では
白花と有色花を分けたため、応答変数の生物学的意味とモデルの尤度が一致した。

論文の主軸として十分に防御できるのは次の三点である。

1. SNS画像から手動確認済み花弁領域の色を広域定量し、白／有色と有色内強度を
   分ける二部モデルを構築したこと。
2. 環境勾配と連続空間場を同時に扱い、単なる地域分布の重なりと説明変数の
   増分情報を分けたこと。
3. 送粉者と人為影響を、VIFで削除せず、生物自身も環境・空間構造を受けるという
   前提の下で「景観重なり→環境調整→空間調整→環境＋空間調整」の証拠階段に
   したこと。

Bombusと園芸は興味深い拡張だが、現在のデータだけから局所的選択や園芸由来を
実証したとは言えない。

## 1. 応答変数の妥当性

### 改善点

- 被写体を抽出・トリミング後、著者が花弁色抽出域を目視確認してからスクリプト
  へ渡したことをMethodsへ明記した。
- マスク形状や濃色をQC除外条件にしていない。特に濃色は園芸仮説の対象なので、
  応答依存の除外を避けた。
- a*だけの応答変数非依存Gaussian mixtureから操作的境界 a*=4.94を推定した。
  白相当966件、有色957件、posterior 0.2--0.8の曖昧例124件である。
- 白相当ではa*を色素量として解析せず、有色内だけで連続強度を標準化した。

### 残る限界

この閾値は光学的分類であり、アントシアニンの化学的有無を直接測った境界では
ない。したがって本文では `optical pigmentation presence` または
`white-like versus pigmented appearance` と呼び、anthocyanin concentrationとは
書かない。

`a*>0` は254件の非負だが白相当の観察を有色へ入れる。これは主解析に不適切で、
感度分析に残した判断が妥当である。高信頼例だけ、joint L*a*C* mixtureでも主要
結論は変わらない。

## 2. 環境・空間・Bombusの交絡

### 解析ロジック

Bombusも生物なので、気候・地形・歴史・空間構造を受ける。したがってBombusと
環境の共線性は矛盾ではなく、予想される共有原因である。VIFを理由にBombusを
削除すると、問いそのものを失う。代わりに以下を別々に推定した。

1. Bombusだけの景観重なり。
2. 環境を調整した関連。
3. 広域空間を調整した関連。
4. 環境と空間を同時に調整した増分関連。
5. 空間foldで未使用地域への予測改善。
6. SPDE-INLAで同じ固定効果追加のWAIC/CPO比較。

### 結果

- 有色化とW/Aには強い広域重なりがある。
- 有色化の環境＋空間調整後は、WもAも区間が0を跨ぎ、空間fold予測改善はほぼ0。
- SPDEでも全国W追加はWAIC +1.37、A利用域でA追加は +2.71で悪化した。
- 有色内強度ではWが負方向に残る。GAM係数 -0.401、空間fold ΔR² +0.0096、
  SPDE事後平均 -0.296（95%信用区間 -0.532-- -0.059）、WAIC改善は0.54である。
- Aは有色内強度でも区間が0を跨ぎ、空間fold ΔR²は負である。

従って「送粉者ギルド転換と花色地理が重なる」は説明できるが、「環境・空間と
独立な局所Bombus選択を検出した」は説明できない。Wの有色内負勾配は小さな残存
関連として報告できるが、機構の向きは慎重に解釈する必要がある。

## 3. 3高山種と広域2種

WはB. ardensとB. diversus、AはB. beaticola、B. consobrinus、
B. honshuensisである。3高山種は別々のENMeval面でも広域正勾配を示すが、
occurrence-density面同士が非常に強く相関しており、三つの独立反復として数えては
ならない。B. consobrinusの有色化モデルでは空間fold 5のbase/full fitにmgcvの
step-failure警告が出た。全標本係数、W/A、有色内強度には同警告はない。

## 4. AUCが低く見える理由

B. diversusのENMeval validation AUCは約0.631である。これは広域分布種の出現点が
accessible backgroundの広い部分を占め、presenceとbackgroundを空間的に順位付け
しにくいことと整合する。空間分割では近隣点の漏洩も減るため、random splitより
低くなる。AUCの低さを補うために同じデータで閾値やモデルを選び直してはならない。
本解析はAICcで複雑度を選び、AUC、CBI、omissionを併記し、下流ではWの効果を
held-out predictionとSPDEの両方で評価している。このため「低AUCなのに強いW効果」
という循環は避けられている。

## 5. 多重共線性

実際の応答標本ごとに監査した。

- presence全国W: max VIF 9.34、W VIF 6.05、condition number 7.97。
- intensity全国W: max VIF 16.09、W VIF 9.99、condition number 10.64。
- presence A利用域W+A: max VIF 35.55、A 21.09、W 17.09、condition 16.93。
- intensity A利用域W+A: max VIF 41.04、W 19.66、A 17.07、condition 18.29。
- 西日本園芸presence: max VIF 9.54、W 4.19、H 3.96、R 1.79、H×R 1.33。
- 西日本園芸intensity: max VIF 9.70、H 6.20、W 4.63、R 2.68、H×R 1.85。

A利用域ではTemperature PC1--Wが0.93前後、Temperature PC1--Aが-0.89-- -0.90、
W--Aが約-0.80である。WとAを同時に入れた係数は過小評価というより分離不能に近い。
この場合は係数の有意性を競わせず、単独追加、同時追加、予測性能、区間幅を並べる
現設計が妥当である。

## 6. SPDE-INLAの合理性

SPDEは単なるノイズ除去ではなく、環境固定効果で説明されない連続的な地理構造の
尺度を与える。本解析ではpresenceにbinomial-logit、intensityにGaussianを用い、
同じmeshとPC-Matérn priorで比較した。presence自然モデルの空間range中央値は約
128 km、intensityは約57 kmであり、二つの応答で残存空間スケールが異なること自体
が結果である。

ただしSPDE場を遺伝構造、歴史、未測定環境のいずれか一つに帰属させてはならない。
またBombus面と同じ空間スケールを共有するため、Bombus固定効果がSPDEへ「吸収」
されるのは欠陥ではなく、独立に識別できる情報が少ないことを示す。

## 7. 残差を扱う正当な理由と注意点

### 正当な役割

残差は「自然要因を除いた真の園芸効果」ではない。正当な役割は、H/Rを含まない
自然モデルが予測しにくい観察を、外部検証の優先順位として並べることである。

- presence residual: 自然モデルでは白を予測したが実際は有色だった程度。
- intensity residual: 有色花の中で自然予測より濃かった程度。

両者とも空間foldでcross-fitし、候補自身を学習に使ったin-sample residualを避けた。

### 循環防止

1. 色素閾値は地理・環境・Bombus・H・R・残差を使わず決める。
2. 自然残差モデルにはH、R、道路アクセス、早咲scoreを入れない。
3. 主推論は残差ではなく元の二部応答にH/Rを追加する。
4. 残差上位は探索的な候補順位と閾値収束テストだけに使う。

### 園芸仮説の結果

西日本の直接モデルではpresenceのH×Rが0.298（95%区間0.009--0.588）、
intensityは0.197（-0.017--0.412）である。しかし残差上位で強まらない。

- presence上位80%ではH×Rが正で予測改善するが、90・95%で消える。
- intensity上位80・90%ではH×Rが負、95%で正へ反転し、全区間が0を跨ぐ。
- intensityのH/R追加は全閾値でheld-out log-lossを悪化させる。
- 西日本intensity上位95%の8件にearly-score上位10%は0件。

したがって現データは園芸由来仮説を支持しない。上位候補の中には高H・高R・濃色の
個体があり外部検証価値は高いが、候補の存在と集団レベルの収束証拠を混同しない。

## 8. 論文ストーリーの推奨順位

### 主結果

1. 手動確認済みSNS画像から花色を広域定量できる。
2. 白／有色と有色内強度を分けることで、生物学的に異なる二つの地理勾配を示す。
3. 環境とSPDE空間場を競合させず、両方を生態学的構造として評価する。

### 生物的拡張

4. Bombusギルド転換は花色地理と重なるが、独立な局所効果は弱い。
5. それでもWの有色内強度には小さな残存勾配があり、広域biotic proxyを評価する
   設計例になる。

### 人為影響の探索

6. 園芸仮説をH/R、残差上位、早咲、道路アクセス、西東差で反証可能にした。
7. 予想した上位収束は得られず、園芸由来を支持しなかった。
8. 候補順位を今後の遺伝・栽培履歴・現地確認へ接続する。

この順なら、園芸が非支持でも論文全体は崩れない。新規性は「園芸を証明したこと」
ではなく、限られた画像データから自然・生物・人為仮説を循環せず順に評価できる
ワークフローと、C. punctataでのみ組み立てやすい競合予測にある。

## 9. 最終的なclaim ceiling

### 書ける

- optical pigmentation presence / conditional visible intensity
- broad Bombus--flower colour co-geography
- small residual W-associated intensity gradient
- human-interface candidate ranking
- no convergence of the current horticultural signatures

### 書けない

- anthocyanin concentration
- Bombus visual preference or selection gradient
- absence of Bombus as the cause of island white flowers without direct island comparison
- horticultural escape/provenance for any ranked observation
- SPDE field as a uniquely identified historical or genetic process

