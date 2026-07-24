# 投稿先選定レビュー

調査日: 2026-07-24
対象原稿: 全国規模のSNS画像から抽出したホタルブクロ花色を、白／有色の二値過程と有色花内の色素強度に分け、環境・INLA-SPDE、局所的な予測マルハナバチ群集転換、自然モデルから外れる候補の人為的文脈を階層的に評価する研究

## 結論

第一候補は **Ecology and Evolution の Research Article**、第二候補は **AoB PLANTS の Study** とする。

Ecology and Evolutionを第一候補とする理由は、次の4点である。

1. 生態・進化・保全を広く扱い、特定分類群や特定の証拠様式に限定されない。誌面は「evidence-based views」を受け入れ、ページ数上限を設けない方針を公式に示しているため、全国パターン、局所送粉者解析、人為的候補の特徴づけという複数スケールの構成を無理に一つの因果モデルへ押し込めず提示できる。[誌面概要](https://onlinelibrary.wiley.com/page/journal/20457758/homepage/productinformation.html)
2. Research Articleとして通常のIntroduction–Methods–Results–Discussion構成を使え、初回投稿はfree formatである。[Author Guidelines](https://onlinelibrary.wiley.com/page/journal/20457758/homepage/forauthors.html)
3. データ、メタデータ、コードを投稿時から査読可能にする要件が、本研究の再現可能なパイプラインとよく合う。[Author Guidelines: Policy on Data Archiving](https://onlinelibrary.wiley.com/page/journal/20457758/homepage/forauthors.html)
4. 完全OAの公示APCはUSD 2,700だが、Wileyの現行日本向け契約では京都大学が対象機関に含まれ、責任著者、論文種別、予算枠などの条件を満たせばAPCが機関負担となり得る。枠はアクセプト時の残額と機関承認に依存するため、投稿前に京都大学図書館へ確認する。[Wiley Japan agreement](https://authors.wiley.com/author-resources/Journal-Authors/open-access/affiliation-policies-payments/japan-agreement.html) / [対象機関一覧](https://authors.wiley.com/author-resources/Journal-Authors/open-access/affiliation-policies-payments/institutional-funder-payments.html)

AoB PLANTSは植物研究としての焦点が最も明確で、rigor、clarity、substanceを重視するsound-science型の誌面であるため、第一候補で広すぎる、または生態情報学的な新規性が弱いと判断された場合の強い第二候補になる。一方、StudyはIntroductionからAcknowledgementsまで6,000語、図表合計10点という上限があり、現行の多層解析をかなり圧縮する必要がある。[General Instructions](https://academic.oup.com/aobpla/pages/General_Instructions)

## 比較表

| 順位 | 誌名 | 適合度 | 推奨論文種別 | 主な利点 | 主なリスク |
|---:|---|---:|---|---|---|
| 1 | Ecology and Evolution | 高 | Research Article | 広い生態・進化スコープ、証拠重視、ページ上限なし、free format、厳格なデータ・コード公開方針、京都大学のWiley OA契約候補 | 送粉者を実測効果やアバンダンスとして書く、人為由来を断定する、自然史を長くしすぎると査読で崩れる |
| 2 | AoB PLANTS | 高 | Study | 植物個体・環境・生態・進化に直接適合、記述的・確認的研究も対象、double-anonymised、比較的低いAPC | 6,000語・図表10点への圧縮が必要。予測Bombusと人為的候補を「補助仮説」と明確化する必要 |
| 3 | Ecological Informatics | 中～高 | Research Article | SNS画像、画像処理、データ集約、Bayesian inference、SDM、uncertaintyがスコープに明記 | 生物学的発見の適用研究だけでは弱く、再利用可能な情報学的ワークフローと画像計測妥当性を主役にする必要。完全OAで高額 |
| 4 | Journal of Ecology | 中（ストレッチ） | Research Article | 植物生態、植物―動物相互作用、再現可能な動的文書に適合 | 一般的生態原理・機構を「significantly advance」する要求が高い。予測送粉者、人為由来未検証、遺伝データなしの現状ではdesk rejectリスクが高い |

## 1. Ecology and Evolution

### Scopeと適合

公式サイトは、生態学・進化学・保全科学の全分野と全分類群を対象とする完全OA誌で、evidence-based viewsを歓迎するとしている。[Journal home](https://onlinelibrary.wiley.com/journal/20457758) 公式の編集方針はページ数上限を置かず、知見の「impact」だけでなく有用性と妥当性を見ると明記する。[Overview](https://onlinelibrary.wiley.com/page/journal/20457758/homepage/productinformation.html)

したがって、本原稿は「花色進化を送粉者が引き起こした」研究としてではなく、次の順に置くと適合する。

1. SNS画像を検証済みの表現型データへ変換し、色の有無と有色内強度を分ける。
2. 全国スケールの環境効果と残余空間構造を分離して、二つの色過程を評価する。
3. 近距離ペアに限定し、花色転換と**予測されたBombus群集適合度の転換**が対応するかを評価する。
4. 自然モデルから反復して極端となる観測について、人為的景観・開花時期・局所的孤立という共通特徴を記述する。
5. 園芸由来は結論ではなく、遺伝・来歴調査で検証すべき候補生成として提示する。

### 投稿形式

- 論文種別: Research Article。[Author Guidelines](https://onlinelibrary.wiley.com/page/journal/20457758/homepage/forauthors.html)
- 本文: 公式方針上、ページ数上限なし。Introduction、Materials and Methods、Results、Discussionを基本とする。[Overview](https://onlinelibrary.wiley.com/page/journal/20457758/homepage/productinformation.html) / [Author Guidelines](https://onlinelibrary.wiley.com/page/journal/20457758/homepage/forauthors.html)
- Abstract: 形式は著者判断。現行規定に語数上限の明記はない。ただし簡潔な一段落型にして250–300語以内を自主目標とする。
- Keywords: 4–6語句。[Author Guidelines](https://onlinelibrary.wiley.com/page/journal/20457758/homepage/forauthors.html)
- 初回投稿: free format。title page、abstract、本文、literature cited、tables、figure legends、Data Accessibility Statement、Competing Interests、CRediT型Author Contributions、Acknowledgementsを含める。[Author Guidelines](https://onlinelibrary.wiley.com/page/journal/20457758/homepage/forauthors.html)
- 表・図: 表は参考文献後の別ページ、図は本文から分離したファイルを推奨し、EPS/TIFFが望ましい。本文中で出現順に引用する。[Author Guidelines](https://onlinelibrary.wiley.com/page/journal/20457758/homepage/forauthors.html)
- 参考文献: 初回は一貫した任意形式でよい。刊行時はHarvard型となる。件数上限は現行規定に明記されていない。[Author Guidelines](https://onlinelibrary.wiley.com/page/journal/20457758/homepage/forauthors.html)

### データ・コード

投稿時に、データ、メタデータ、コードを編集者・査読者が確認できるようにする必要がある。改訂時までに公開リポジトリへ正式に保存し、Data Accessibility Statementからリンクする。データなしの投稿はeditorへ回さないと明記されている。Dryadを使い採択された場合、誌面がアーカイブ料金を負担する。[Author Guidelines: Policy on Data Archiving](https://onlinelibrary.wiley.com/page/journal/20457758/homepage/forauthors.html) / [minimum standards article](https://onlinelibrary.wiley.com/doi/10.1002/ece3.9961)

本研究では、少なくとも次を固定版としてZenodo等へ保存する必要がある。

- 解析に用いた派生花色データと変数辞書
- 元画像を再配布できない場合の画像ID、取得元、取得日、ライセンス・利用制約、抽出値
- 色抽出のコード、手動確認手順、除外ではなくフラグ付けを行ったQC記録
- ENMevalによるBombus SDMの入力出典、fold、候補モデル、選択規則、予測不確実性
- INLA-SPDE、局所ペア、人為的特徴づけ、シミュレーションの全コードと乱数seed
- 実行環境ロックファイルまたはパッケージバージョン一覧

### 費用・OA

完全OA。APCはUSD 2,700 / GBP 1,960 / EUR 2,210で、投稿料・ページ料はない。BES会員20%、IAVS会員10%割引、機関契約・免除制度がある。[Article Publication Charges](https://onlinelibrary.wiley.com/page/journal/20457758/homepage/open-access)

京都大学はWileyの日本向け対象機関一覧に掲載されているが、責任著者、Primary Research、アクセプト時の枠、機関承認等が条件である。**無料と決め打ちせず、投稿前に図書館へ確認する。** [Wiley Japan agreement](https://authors.wiley.com/author-resources/Journal-Authors/open-access/affiliation-policies-payments/japan-agreement.html) / [Institutional payments](https://authors.wiley.com/author-resources/Journal-Authors/open-access/affiliation-policies-payments/institutional-funder-payments.html)

### 査読上の危険箇所

- BombusのSDM出力は存在確率または環境適合度であり、個体数、訪花頻度、送粉効果ではない。
- 5種の予測値を合成した指標は「推定群集fingerprint」であり、実測群集ではない。
- 局所ペア化は全国的な空間・環境クラインを弱める設計だが、空間交絡を消去するものではない。距離matched null、環境差調整、感度分析を明記する。
- 人為的特徴は園芸由来を立証しない。自然モデルからの再現性ある極端候補について、複数の観測可能な特徴が共起するかを記述したものとする。
- 白花の連続色値をアントシアニン量として扱わず、色の有無と有色内強度を分けた理由をIntroductionとMethodsの両方で簡潔に示す。

## 2. AoB PLANTS

### Scopeと適合

AoB PLANTSは、植物のorganismal、environmental、ecological、evolutionary biologyを扱う非営利・完全OA誌で、experimental、theoretical、descriptive、confirmatory studies、negative results、自然・管理系の研究を受け入れる。[General Instructions](https://academic.oup.com/aobpla/pages/General_Instructions) intact plantsのfunction、ecology、evolution、とくに自然・管理環境中の植物を重視する。[Why Publish](https://academic.oup.com/aobpla/pages/why_submit)

このため、ホタルブクロという植物種に焦点を当てる必然性、白／有色と有色内強度の階層、生物間相互作用の候補、人為移送候補までを一つの植物表現型研究として提示しやすい。

### 投稿形式

- 論文種別: Study。大規模データ解析を含むためDatasetやToolではない。[General Instructions](https://academic.oup.com/aobpla/pages/General_Instructions)
- 本文: IntroductionからAcknowledgementsまで最大6,000語。Abstract、Introduction、Referencesを必須とし、再現可能性に必要な他の節を置く。図表合計は最大10点。[General Instructions](https://academic.oup.com/aobpla/pages/General_Instructions)
- Abstract・Keywords: 現行公開規定ではStudyのabstract語数上限とkeyword数上限が明記されていない。投稿システムで再確認する。原稿側ではabstract 250–300語、keywords 6語を自主上限とすれば第一候補からの転送も容易。
- 初回投稿: 匿名化した単一PDFと別title page、1.5行以上、行番号・ページ番号。初回の引用形式は任意だが、各文献に有効なDOIを入れる。[General Instructions](https://academic.oup.com/aobpla/pages/General_Instructions)
- 図: 受理後は原則600 dpi JPG、RGBまたはgrayscale、1段8 cmまたは2段17 cm、最大高23 cm。図ごとのalt textが必要。[General Instructions](https://academic.oup.com/aobpla/pages/General_Instructions)
- 表: 出現順に番号を付け、本文で引用し、内容を要約する導入文から始まるlegendを表上部に置く。[General Instructions](https://academic.oup.com/aobpla/pages/General_Instructions)
- 参考文献: 第一著者姓のアルファベット順。最初の10著者を記載後にet al.を使用し、DOIを含める。件数上限は明記されていない。[General Instructions](https://academic.oup.com/aobpla/pages/General_Instructions)

### データ・コード

新規解析を再現するために必要な全データを読者に提供し、Discussion直後のData Availabilityで保存先とアクセス方法を示す。恒久的な公開リポジトリが推奨され、Supporting Informationも認められる。[General Instructions: Data Policy](https://academic.oup.com/aobpla/pages/General_Instructions)

コードを明示的に全Studyへ義務づける文言はデータ要件ほど強くないが、本研究はコードなしでは再現できないため、第一候補と同じ公開一式を維持する。

### 費用・OA

完全OA、CC BY、著作権は著者保持。公示料金は1論文GBP 1,118で、免除・割引および一部のRead & Publish制度がある。[Publication Charges](https://academic.oup.com/aobpla/pages/publication_charges) / [OA License & Copyright](https://academic.oup.com/aobpla/pages/oa_copyright)

OUPの日本JUSTICE 2026–2028契約ページは対象をeligible hybrid journalsと説明しており、完全OAのAoB PLANTSが京都大学について自動的に無償になるとは読めない。したがって、GBP 1,118を基本予算として想定し、個別の機関適格性だけ別途確認する。[OUP JUSTICE agreement](https://academic.oup.com/pages/open-research/read-and-publish-agreements/japan-alliance-of-university-library-consortia-justice)

### 査読上の危険箇所

- 6,000語で全解析を並列に説明すると散漫になる。主解析を「表現型階層＋全国環境・空間」、送粉者と人為的文脈を二つの局所的拡張として配置する。
- 園芸由来はDiscussionの候補生成に止め、タイトル・abstractの結論にはしない。
- シマホタルブクロ・伊豆諸島の自然史は研究動機として重要だが、「マルハナバチ不在が白花を生じさせた」という因果証拠としては扱わない。

## 3. Ecological Informatics

### Scopeと適合

Ecological Informaticsはcomputational ecology、ecological data science、biogeography、ecosystem analysisを対象とし、画像ベースのmonitoring、multimedia data acquisition、data assimilation、ecological modelling、uncertainty analysis、Bayesian inference、SDMを明示的に挙げている。[Aims & Scope](https://www.sciencedirect.com/journal/ecological-informatics)

SNS画像からの花色抽出と、環境・空間・予測群集・人為的文脈を段階的に結ぶ設計はスコープ上よく合う。ただし、採択可能性を高めるには「ホタルブクロの生態」よりも、次を主たる貢献として示す必要がある。

- 再利用可能な画像表現型ワークフロー
- 無色状態と有色内強度を混ぜないhurdle型表現
- 広域空間モデルと非連続な局所ペア解析の役割分担
- 自然モデルから反復的に候補を定義する不確実性込みの手順
- SNS由来の選択バイアス、色抽出誤差、SDM予測誤差を伝播または感度分析する方法

現在の色抽出が手動で抽出域を確認する研究者監督型ワークフローであること自体は欠点ではないが、画像処理法として売るなら、再現性、観察者間一致、色校正限界、照明・カメラ差への感度を定量的に示す必要がある。

### 投稿形式

- 論文種別: Research Article。[Journal home](https://www.sciencedirect.com/journal/ecological-informatics)
- 公式Guide for Authors: [current guide](https://www.sciencedirect.com/journal/ecological-informatics/publish/guide-for-authors)
- Elsevierのページは調査時に動的表示の一部が安定して取得できず、現行の全文語数、abstract上限、keyword数、図表数上限を公式本文から再確認できなかった。旧規定や他のElsevier誌の数値を流用せず、投稿直前に上記current guideとEditorial Managerで確認する。
- 少なくとも初回原稿は、numbered Introduction、Materials and Methods、Results、Discussion、Conclusions、Data Availability、CRediT、Declaration of Competing Interest、Referencesとし、図表を本文順に引用する。これは誌面変更時にも変換しやすい。
- 参考文献件数の上限は公式公開ページで確認できなかった。本文ではauthor–year型を一貫して使い、DOIを付ける。

### データ・コード

情報学的貢献として投稿する場合、花色派生表、画像利用条件、コード、設定、モデル選択表、seed、環境を恒久リポジトリへ置くことが事実上不可欠である。最終投稿時にはcurrent guideのResearch data/Data statement欄を再確認する。[Guide for Authors](https://www.sciencedirect.com/journal/ecological-informatics/publish/guide-for-authors)

### 費用・OA

2024年1月から完全gold OA。現行公示APCはUSD 2,980（税別）。[Journal Insights](https://www.sciencedirect.com/journal/ecological-informatics/about/insights) / [OA移行告知](https://www.sciencedirect.com/journal/ecological-informatics/about/news/ecological-informatics-is-transitioning-to-gold-open-access)

### 査読上の危険箇所

- 単一種への既存手法適用と見なされるとscope fitはあっても方法的新規性が弱い。
- 画像から得た色値の誤差モデルがない場合、「big data」であることだけでは情報学的貢献にならない。
- INLA-SPDE、ENMeval、局所ペアを単に連結しただけに見せず、各段階が異なる生態学的質問とスケールに対応する設計原理を明示する必要がある。

## 4. Journal of Ecology

### Scopeと適合

Journal of Ecologyは植物生態を中心とし、植物―動物相互作用も対象とする一方、一般的生態原理と植物生態機構の理解を有意に前進させ、分野を方向づけ得る論文のみを掲載すると明記する。[Aims and Scope](https://besjournals.onlinelibrary.wiley.com/hub/journal/13652745/aims-and-scope/read-full-aims-and-scope)

研究対象とテーマは合うが、現状の証拠は次の点で機構誌として弱い。

- Bombusは訪花・選択・送粉成功の実測ではなく、独立データから推定した環境適合度である。
- 園芸由来は遺伝、栽培履歴、形質組合せによる検証がない。
- SNS画像は観察努力と撮影選択を受け、全国の個体群無作為標本ではない。

したがって、現状では第一投稿先にしない。送粉者実測または遺伝データを追加できない前提なら、一般原理として「非無作為なデジタル標本から、状態遷移・連続形質・局所群集転換をどう分離して推論するか」を相当に強く実証しない限りdesk rejectリスクが高い。

### 投稿形式

- 論文種別: Research Article。通常8,000語。内容が正当化すれば長い原稿も検討する。[Author Guidelines](https://besjournals.onlinelibrary.wiley.com/hub/journal/13652745/author-guidelines)
- Abstract: 最大350語。主結果・結論を簡潔な番号付き文で示し、最後を`Synthesis`として非専門家にも分かる一般的前進を述べる。[Author Guidelines](https://besjournals.onlinelibrary.wiley.com/hub/journal/13652745/author-guidelines)
- Keywords: アルファベット順、最大8語または短句。[Revision Guidelines](https://besjournals.onlinelibrary.wiley.com/hub/journal/13652745/author-guidelines-revisions)
- 本文: Introduction、Materials and Methods、Results、Discussion、任意のConclusions、References。初回は1段組・double-spaced、連続行番号・ページ番号。double-anonymousのためtitle pageを分離する。[Author Guidelines](https://besjournals.onlinelibrary.wiley.com/hub/journal/13652745/author-guidelines)
- 図表: 初回査読では該当箇所へ埋め込み可能。図数上限はResearch Articleについて公式ページに固定値の記載なし。
- 参考文献: 初回は特定形式不要で、本文中はauthor–yearを推奨。件数上限の明記なし。[Author Guidelines](https://besjournals.onlinelibrary.wiley.com/hub/journal/13652745/author-guidelines)

### データ・コード

初回査読時からデータ・コードをファイルまたはprivate repositoryで査読可能にする。さらに、データ、実行可能なdynamic document、コードと統計出力を統合した静的再現性PDFの3点を推奨する。[Author Guidelines](https://besjournals.onlinelibrary.wiley.com/hub/journal/13652745/author-guidelines)

### 費用・OA

hybrid誌で、OAは任意。OAを選択する場合の現行APCはUSD 3,970 / GBP 2,650 / EUR 3,300（税別）で、機関契約による負担の可能性がある。[Open Access](https://besjournals.onlinelibrary.wiley.com/hub/journal/13652745/fundedaccess) 京都大学はWiley日本契約の対象機関一覧に含まれるが、枠と承認を確認する。[Wiley Japan agreement](https://authors.wiley.com/author-resources/Journal-Authors/open-access/affiliation-policies-payments/japan-agreement.html)

## 推奨する投稿順と分岐

### 標準ルート

1. Ecology and EvolutionへResearch Articleとして投稿する。
2. scopeまたは編集上の優先度で不採択になった場合、査読意見を反映し、AoB PLANTSへStudyとして圧縮して投稿する。
3. 編集者が方法的新規性を高く評価し、生物学的機構より情報処理を前面に出すべきと判断した場合のみ、Ecological Informaticsへ再構成する。

### Journal of Ecologyを先に試す条件

次のうち少なくとも二つを満たし、かつ一般的機構へ結論を引き上げられる場合に限る。

- 花色転換帯での訪花・選択・結実の独立観測
- 白／有色境界または逸脱候補の遺伝的由来
- SNS観察選択を検証する独立標本
- 画像色計測誤差とBombus予測誤差を含む統合的不確実性解析
- 日本以外または近縁分類群にも適用可能な一般則の外部検証

## 第一候補向けの原稿仕様

原稿改訂は、Ecology and Evolutionのfree-format Research Articleを基準に次の構成とする。

1. Title page
2. Abstract（自主上限250–300語）
3. Keywords（4–6）
4. Introduction
   - SNS画像による広域表現型データの不足補完
   - 白／有色と有色内強度が異なる生成過程であること
   - 全国環境・空間、局所送粉者群集転換、自然モデル外候補というスケール別仮説
5. Materials and Methods
   - Image acquisition and supervised colour extraction
   - Hierarchical flower-colour responses
   - Nationwide environmental and spatial models
   - Local paired tests of predicted Bombus community turnover
   - Repeated natural-model candidate definition
   - Human-context characterization and sensitivity analyses
   - Reproducibility and data ethics
6. Results
   - Phenotype hierarchy
   - Nationwide environment and spatial structure
   - Pigmented-flower intensity gradients
   - Local colour and predicted-community transitions
   - Characteristics of repeatedly extreme candidates
7. Discussion
   - 全国パターン
   - 送粉者について説明できる範囲
   - 人為的候補について説明できる範囲
   - SNSデータの一般的方法的意義
   - 限界と直接検証の優先順位
8. Data Accessibility Statement
9. Competing Interests
10. Author Contributions
11. Acknowledgements
12. References
13. Tables
14. Figure legends

タイトルとabstractでは、`pollinator effects`、`horticultural escape`、`anthropogenic introduction`を確定事項として用いない。推奨語は、`predicted Bombus community turnover`、`human-context signatures`、`repeated departures from a natural baseline`、`candidate anthropogenic influence`である。

## 投稿前の実務チェック

- [ ] 京都大学図書館へWiley APC枠と責任著者要件を確認
- [ ] 4–6 keywordsを決定
- [ ] main manuscriptとfigure filesを分離
- [ ] 全データ・メタデータ・コードを査読可能なprivate archiveへ固定
- [ ] 元SNS画像を再配布できない場合の権利・代替再現手順をData Accessibility Statementに明記
- [ ] ENMevalで新規作成したBombus SDMの入力・fold・選択表・予測値を保存
- [ ] 既存Bombus TIFFを入力にしていないことをMethodsとprovenanceに明記
- [ ] 予測確率をアバンダンスまたは送粉効果と呼んでいないか全文検索
- [ ] 園芸由来を立証したと読める文を全文検索
- [ ] データ・コード・図・表・本文の数値を最終ロック結果と照合
- [ ] AIを原稿作成・コード支援に使用した場合、投稿先の現行開示方針に沿って申告

## 公式情報源

以下はすべて2026-07-24に参照した公式誌面または公式出版社ページである。

### Ecology and Evolution / Wiley

- https://onlinelibrary.wiley.com/journal/20457758
- https://onlinelibrary.wiley.com/page/journal/20457758/homepage/productinformation.html
- https://onlinelibrary.wiley.com/page/journal/20457758/homepage/forauthors.html
- https://onlinelibrary.wiley.com/page/journal/20457758/homepage/open-access
- https://onlinelibrary.wiley.com/doi/10.1002/ece3.9961
- https://authors.wiley.com/author-resources/Journal-Authors/open-access/affiliation-policies-payments/japan-agreement.html
- https://authors.wiley.com/author-resources/Journal-Authors/open-access/affiliation-policies-payments/institutional-funder-payments.html

### AoB PLANTS / Oxford University Press

- https://academic.oup.com/aobpla/pages/about
- https://academic.oup.com/aobpla/pages/General_Instructions
- https://academic.oup.com/aobpla/pages/publication_charges
- https://academic.oup.com/aobpla/pages/oa_copyright
- https://academic.oup.com/aobpla/pages/why_submit
- https://academic.oup.com/pages/open-research/read-and-publish-agreements/japan-alliance-of-university-library-consortia-justice

### Ecological Informatics / Elsevier

- https://www.sciencedirect.com/journal/ecological-informatics
- https://www.sciencedirect.com/journal/ecological-informatics/publish/guide-for-authors
- https://www.sciencedirect.com/journal/ecological-informatics/about/insights
- https://www.sciencedirect.com/journal/ecological-informatics/about/news/ecological-informatics-is-transitioning-to-gold-open-access

### Journal of Ecology / British Ecological Society and Wiley

- https://besjournals.onlinelibrary.wiley.com/hub/journal/13652745/aims-and-scope/read-full-aims-and-scope
- https://besjournals.onlinelibrary.wiley.com/hub/journal/13652745/author-guidelines
- https://besjournals.onlinelibrary.wiley.com/hub/journal/13652745/author-guidelines-revisions
- https://besjournals.onlinelibrary.wiley.com/hub/journal/13652745/fundedaccess
