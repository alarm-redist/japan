# 2020年 北海道 衆議院議員選挙 小選挙区の区割り改定

## 区割り改定時の条件
衆議院議員選挙区画定審議会が策定した [「区割り改定案の作成方針」](https://www.soumu.go.jp/main_content/000794997.pdf)では、以下のような条件が定められている。

* 2020年国勢調査に基づく各選挙区の日本人人口（総人口と外国人人口）は、鳥取２区の日本人人口の2倍未満とする
* 市区町村は原則として分割しない
* 郡は可能な限り分割しない
* 自然的社会的条件を考慮
* 飛び地をもつ選挙区は設けない

なお、「区割り改定案の作成方針」では明文化されていないものの、以下のことを考慮し、市区町村を3つ以上の選挙区に分割することは回避すべきとされていると解釈した。

* 現行の区割りにおいて市区町村が３分割されている例は、市区町村合併により合併後の市区町村が３分割されることとなったものである
* 平成29年の区割り改定時、東京都知事が市区町村の３分割を回避するように求め、それが[採択された](https://www.soumu.go.jp/main_content/000761504.pdf)

### 北海道特有の条件 

* [「区割り改定案の作成方針」](https://www.soumu.go.jp/main_content/000794997.pdf)では、「北海道の選挙区の改定案の作成に当たっては、総合振興局又は振興局の区域 を尊重するものとする。」とされている。
よって、北海道では総合振興局又は振興局を基本の単位として扱う。
* 石狩総合振興局の人口は、北海道の1議席あたりの人口（県の総人口 / 選挙区数）より大きいため、石狩総合振興局とその他の振興局において、別々に区割りシミュレーションを行うことにより、総合振興局又は振興局の区域にまたがる選挙区が作成されないようにした。
* その際、北海道の小選挙区数12を、石狩総合振興局とその他の振興局それぞれの日本人人口に基づき比例配分し、以下の通り定数を割り当てた。
  * 石狩総合振興局: 6
  * その他の振興局: 6

## 設定
* 各選挙区の人口 : 
  * 石狩総合振興局: (日本人人口 / 選挙区数:6) ± 10%
  * その他の振興局: (日本人人口 / 選挙区数:6) ± 40%
  
* 基本の単位：
  * 石狩総合振興局: 
    * 市区町村を基本の単位とする。
    * ただし、現状の区割りにおいて分割されている市区町村については、シミュレーションにおいても分割を許容し、国勢調査における小地域を基本の単位として扱う
    * 現状の区割りにおいて分割されている市区町村は札幌市北区、札幌市西区
    
  * その他の振興局: 
    * 総合振興局または振興局を基本の単位とする。
    
* 石狩総合振興局においては、市区町村の分割数の制限 (`add_constr_splits`)と、市区町村の複数選挙区への分割の制限(`add_constr_multisplits`) をそれぞれを設定
* 郡の分割は、現行の区割りにて分割されている場合のみ許容
* 離島は、航路で結ばれている本土の市区町村と同一の選挙区にする
* 逐次モンテカルロ法(SMC)による4つの独立したシミュレーションにより、石狩総合振興局とその他の振興局それぞれで20,000区割り改定案を作成
* 以下の案を棄却し有効な案を選定
  * 石狩総合振興局:
    * 市区町村が3つ以上の選挙区に分割される案
    * 現状の区割りにおいて分割されていない郡・市区町村が分割される案
  * その他:
    * 飛び地が生じる案
    * 市区町村が3つ以上の選挙区に分割される案

* 有効な案の中から5,000の案をそれぞれ無作為に抽出
  
## データの出典
* [「地図で見る統計(統計GIS)  境界データダウンロード」(政府統計の総合窓口(e-Stat)）](https://www.e-stat.go.jp/gis/statmap-search?page=1&type=2&aggregateUnitForBoundary=A&toukeiCode=00200521)
* [「令和2年国勢調査(小地域集計　人口等基本集計に関する集計)」(総務省統計局)](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136472)
* [「令和2年国勢調査(人口等基本集計)」（総務省統計局）](https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00200521&tstat=000001136464&cycle=0&year=20200&month=24101210&tclass1=000001136466)
* [「国土数値情報(湖沼データ)」(国土交通省)](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-W09-v2_2.html)
* [「国土数値情報(行政区域データ)」(国土交通省)](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03-v2_3.html)
* [「国土数値情報(定期旅客航路データ)」（国土交通省）](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N09.html)
* [衆議院議員選挙の小選挙区のポリゴンデータ (東京大学空間情報科学研究センター 西沢明）](https://home.csis.u-tokyo.ac.jp/~nishizawa/senkyoku/)

## データの処理
特になし

## ファイル内容
* `1_hokkaido_hr_2020_map_ishikari.rds`, `1_hokkaido_hr_2020_map_non_ishikari.rds`: 圧縮された `redist_map` オブジェクト。市区町村（及び小地域）単位のシェイプファイル及び人口データ。
* `1_hokkaido_hr_2020_plans_ishikari.rds`, `1_hokkaido_hr_2020_plans_non_ishikari.rds` :  圧縮された`redist_plans` オブジェクト。選挙区への割り当てを示す行列を含む。
* `1_hokkaido_hr_2020_stats_ishikari.csv`, `1_hokkaido_hr_2020_plans_non_ishikari.csv`: 区割り改定案の元での選挙区ごとの日本人人口（総人口-外国人人口）をまとめたファイル。

`redist_plans` 及び `redist_map` オブジェクトは Rパッケージ [redist](https://alarm-redist.github.io/redist/)を使用して分析する。

## コードブック
* `draw`:　区割り改定案のサンプルの識別子。
* `district`: 区割り改定案の元での選挙区を示す識別子。無作為に割り当てられるため、実際の区割りの元での選挙区の番号とは一致しない。
* `total_pop`: 区割り改定案の元での選挙区ごとの日本人人口（総人口-外国人人口）。
