# 2020年 東京都　衆議院議員選挙 小選挙区の区割り改定

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

### 東京都特有の条件 

* [「区割り改定案の作成方針」](https://www.soumu.go.jp/main_content/000794997.pdf)では、「区部及び多摩地域の区域を尊重するものとする」とされている。よって、23区地域と多摩地域に対し、別々に区割りシミュレーションを行うことにより、23区地域と多摩地域にまたがる選挙区が作成されないようにした。
* その際、東京の小選挙区数30を、23区地域と多摩地域それぞれの日本人人口に基づき比例配分し、以下の通り定数を割り当てた。
  * 23区地域: 21
  * 多摩地域: 9

## 設定
* 各選挙区の人口 : 
  * 23区地域: (日本人人口 / 選挙区数:21) ± 6%
  * 多摩地域: (日本人人口 / 選挙区数:9) ± 6%
* 国勢調査における小地域を基本の単位として扱う
* 市区町村の分割数の制限 (`add_constr_splits`)と、市区町村の複数選挙区への分割の制限(`add_constr_multisplits`) をそれぞれを設定
* 郡の分割は、現行の区割りにて分割されている場合のみ許容
* 離島は、航路で結ばれている本土の市区町村と同一の選挙区にする
* 区割り改定案を23区地域、多摩地域それぞれ25,000作成 
* 市区町村が3つ以上の選挙区に分割される案を棄却し有効な案を選定
* 有効な案の中から5,000の案を無作為に抽出

### 東京都特有の設定

* 以下の臨海部の地域は、橋や道路により結ばれているため、隣接するものとして扱った。
  * 大田区東海と大田区平和島
  * 大田区東海と大田区京浜島
  * 大田区昭和島と大田区大森東
  * 品川区八潮と品川区東八潮
  * 品川区八潮と品川区東大井
  * 品川区八潮と品川区東品川
  * 港区海岸と港区台場
  * 中央区晴海と江東区豊洲
  * 中央区晴海と中央区月島
  * 中央区月島と中央区明石町
  * 中央区明石町と中央区佃
  * 中央区築地と中央区勝どき
  * 中央区佃と中央区新川
  * 中央区佃と江東区越中島
  * 中央区新川と江東区永代
  * 中央区新川と中央区湊
  * 江東区豊洲と江東区有明
  * 江東区塩浜と江東区木場
  * 江東区塩浜と江東区東陽
  * 江東区塩浜と江東区新砂
  
## データの出典
* [「地図で見る統計(統計GIS)  境界データダウンロード」(政府統計の総合窓口(e-Stat)）](https://www.e-stat.go.jp/gis/statmap-search?page=1&type=2&aggregateUnitForBoundary=A&toukeiCode=00200521)
* [「令和2年国勢調査(小地域集計　人口等基本集計に関する集計)」(総務省統計局)](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136472)
* [「令和2年国勢調査(人口等基本集計)」（総務省統計局）](https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00200521&tstat=000001136464&cycle=0&year=20200&month=24101210&tclass1=000001136466)
* [「国土数値情報(湖沼データ)」(国土交通省)](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-W09-v2_2.html)
* [「国土数値情報(行政区域データ)」(国土交通省)](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03-v2_3.html)
* [「国土数値情報(定期旅客航路データ)」（国土交通省）](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N09.html)
* [衆議院議員選挙の小選挙区のポリゴンデータ (東京大学空間情報科学研究センター 西沢明）](https://home.csis.u-tokyo.ac.jp/~nishizawa/senkyoku/)

## データの処理
本プロジェクトでは、2015年国勢調査に基づくシェイプファイルに2020年国勢調査のデータを結合させている。住居表示実施に伴い住所が変更された地域に関しては、以下の通り処理をおこなった。

* 2020年国勢調査における港区(町字コード:310)の人口を2015年国勢調査シェイプファイルにおける港区台場(町字コード:300)の人口に算入
* 2020年国勢調査における新宿区四谷(町字コード:11,12)の人口を2015年国勢調査シェイプファイルにおける新宿区四谷(町字コード:10)の人口に算入
  * なお、四谷本塩町(町字コード:20)は2015年国勢調査シェイプファイルにおける本塩町(町字コード:20)に、四谷三栄町(町字コード:20)は2015年国勢調査シェイプファイルにおける三栄町(町字コード:30)にそれぞれ対応させた
* 2020年国勢調査における昭島市もくせいの杜(町字コード:230)の人口を2015年国勢調査シェイプファイルにおける昭島市福島町(町字コード:51)の人口に算入
* 2015年国勢調査シェイプファイルにおける町田市鶴間(町字コード:200)及び町田市小川(町字コード:40)のポリゴンを結合させ、2020年国勢調査における町田市南町田(町字コード:450)に対応させた
* 2015年国勢調査シェイプファイルにおける町田市木曽西(町字コード:112)と町田市木曽西(町字コード:114)のポリゴンを結合させ、2020年国勢調査における町田市木曽西(町字コード:112)に対応させた

## ファイル内容
* `13_tokyo_hr_2020_map_special_wards.rds`, `13_tokyo_hr_2020_map_tama.rds`: 圧縮された `redist_map` オブジェクト。市区町村（及び平成の大合併前の市区町村）単位のシェイプファイル及び人口データ。
* `13_tokyo_hr_2020_plans_special_wards.rds`, `13_tokyo_hr_2020_plans_tama.rds` :  圧縮された`redist_plans` オブジェクト。選挙区への割り当てを示す行列を含む。
* `13_tokyo_hr_2020_stats_special_wards.csv`, `13_tokyo_hr_2020_stats_tama.csv`: 区割り改定案の元での選挙区ごとの日本人人口（総人口-外国人人口）をまとめたファイル。

`redist_plans` 及び `redist_map` オブジェクトは Rパッケージ [redist](https://alarm-redist.github.io/redist/)を使用して分析する。

## コードブック
* `draw`:　区割り改定案のサンプルの識別子。
* `district`: 区割り改定案の元での選挙区を示す識別子。無作為に割り当てられるため、実際の区割りの元での選挙区の番号とは一致しない。
* `total_pop`: 区割り改定案の元での選挙区ごとの日本人人口（総人口-外国人人口）。