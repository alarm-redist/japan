# 2022 Redistricting for the House of Representatives Electoral Districts in Aichi Prefecture

## Redistricting Requirements

We incorporate as best as possible the rules that the Council on the House of Representatives Electoral Districts (hereafter “the commission”) follows by taking into account the following rules and practices:

1. The Act for Establishment of the Council on the House of Representatives Electoral Districts;
2. The guidelines adopted by the commission in the 2022 redistricting cycle; and
3. Other practices that the commission follows.

### 1. Act for Establishment of the Council on the House of Representatives Electoral Districts
According to the Act for Establishment of the Council on the House of Representatives Electoral Districts,

* The population ratio between the most populous and least populous districts in the country must not exceed two; and 
* “Circumstances such as administrative boundaries, geographical features, and transportation [routes]” must be taken into account.

### 2. Redistricting commission’s guidelines
According to the commission’s guidelines adopted in the 2022 redistricting cycle, known as the [policy for creating redistricting plans](https://www.soumu.go.jp/main_content/000794997.pdf),

* Districts must be contiguous;
* Municipalities must not be split in principle;
* Groups of towns and villages known as counties (*gun*) should not be split wherever possible; and 
* Natural and social conditions should be taken into account comprehensively

### 3. Redistricting commission’s practices
We follow the commission’s practices through:

* Only allowing municipalities to be split based on the boundaries of old municipalities that existed before 1999, wherever possible; and 
* Avoiding splitting municipalities into more than two districts. 

## Algorithmic Constraints

* We enforce a maximum population deviation of 30%.
* As noted later, we use a partial SMC method for Aichi, simulating Owari and Mikawa regions separately. We set the same maximum population deviation for both regions.
* We add a constraint that penalizes municipality splits as well as a constraint that penalizes the splitting of municipalities into more than two districts.
* We also add a constraint to encourage the creation of geographically compact districts.

## Pre-processing Notes
* We add edges between the following pairs of municipalities because they are connected via bridges.
  + 中川区 (Nakagawa-ku) and 中区 (Naka-ku)
  + 中村区 (Nakamura-ku) and 中区 (Naka-ku)
  + 熱田区 (Atsuta-ku) and 南区 (Minami-ku)
  + 熱田区 (Atsuta-ku) and 瑞穂区 (Mizuho-ku)
  + 熱田区 (Atsuta-ku) and 昭和区 (Showa-ku)
  + 港区 (Minato-ku) and 東海市 (Tokai-shi)
  + 港区 (Minato-ku) and 海部郡飛島町 (Tobishima-mura, Ama-gun)
  + 南区 (Minami-ku) and 東海市 (Tokai-shi)

## Simulation Notes
* We sample 80,000 redistricting plans for Owari region across 8 independent runs of the Sequential Monte Carlo (SMC) algorithm and 80,000 plans for Mikawa region across 8 independent runs of the SMC algorithm.
The areas are defined as follows based on administrative and historical groupings:
  + Mikawa region: 岡崎市 (Okazaki-shi), 碧南市 (Hekinan-shi), 刈谷市 (Kariya-shi), 豊田市, (Toyota-shi), 安城市 (Anjo-shi), 西尾市 (Nishio-shi), 知立市 (Chiryu-shi), 高浜市 (Takahama-shi), みよし市 (Miyoshi-shi), 幸田町 (Kota-cho), 豊橋市 (Toyohashi-shi), 豊川市 (Toyokawa-shi), 蒲郡市 (Gamagori-shi), 新城市 (Shinshiro-shi), 田原市 (Tahara-shi), 設楽町 (Shitara-cho), 東栄町 (Toei-cho), and 豊根村 (Toyone-mura).
  + Owari region: Others

  At each stage, we remove plans with discontiguous districts as well as those that divide municipalities into more than two districts.

* We do not allow municipalities to be split except for those that were split in the 2017 or 2022 map: 一宮市 (Ichinomiya-shi), 瀬戸市 (Seto-shi), and 豊田市 (Toyota-shi). We allow these municipalities to be split along the boundaries of Census tracts. 
* In addition, we do not allow counties (*gun*) to be split except for those that were split in the 2017 map. In the case of Aichi, we do not allow any counties (*gun*) to be split.
* To avoid bottlenecks, we set the population temperance at 0.04 for the Owari region and 0.03 for the Mikawa region to avoid bottlenecks. Also, for both regions, we set the sequence of alphas, or the amount by which the weights are adjusted at each resampling stage, at 0.95.

* Finally, we remove plans with discontiguous districts as well as plans that split municipalities into three or more districts, after which we thin the sample to down to 5,000 plans.

## Data Sources

- [2020 Census](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136472)
- [2000 Census](https://www.e-stat.go.jp/gis/statmap-search?page=1&type=2&aggregateUnitForBoundary=A&toukeiCode=00200521&toukeiYear=2000&serveyId=A002005212000&coordsys=1&format=shape&datum=2000)
- [Periodical Ferry Route: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N09.html)
- [Lake and Ponds: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-W09-v2_2.html)
- [Vote record of the House of Councilors election of July 21st 2019](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin25/index.html)
- [Vote record of the House of Councilors election of July 10th 2022](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin26/index.html)
