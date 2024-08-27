# 2022 Redistricting for the House of Representatives Electoral Districts in Tokyo

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

* Note that for Tokyo, the guidelines stipulate that the boundaries of the Special Wards area and Tama area must be respected.

### 3. Redistricting commission’s practices
We follow the commission’s practices through:

* Only allowing municipalities to be split based on the boundaries of old municipalities that existed before 1999, wherever possible; and 
* Avoiding splitting municipalities into more than two districts. 

## Algorithmic Constraints

* We enforce a maximum population deviation of 23%.
* As noted later, we use a partial SMC method for Tokyo, simulating the (A) Special Wards area (except for Koto Ward and Setagaya Ward), (B) Koto Ward, (C) Setagaya Ward, and (D) the Tama area separately. We set the same maximum population deviation for all of these regions.
* We add a constraint that penalizes the splitting of municipalities into more than two districts.
* We also add a constraint to encourage the creation of geographically compact districts.

## Pre-processing Notes

* We add edges between the islands to the south of Tokyo (島嶼部) and 港区海岸 (Kaigan, Minato-ku), as they are connected via ferry routes.
* When creating the adjacengy graph, after adding edges between disconnected Census blocks and their nearest neighbors, we manually add edges between the following Census blocks, as they are connected by roads or bridges:
  + 大田区東海 (Tokai, Ota-ku) and 大田区平和島 (Heiwajima, Ota-ku)
  + 大田区東海 (Tokai, Ota-ku) and 大田区京浜島 (Keihinjima, Ota-ku)
  + 大田区昭和島 (Showajima, Ota-ku) and 大田区大森東 (Omori-higashi, Ota-ku)
  + 大田区羽田空港 (Haneda Airport, Ota-ku) and 大田区京浜島 (Keihinjima, Ota-ku)
  + 大田区羽田空港 (Haneda Airport, Ota-ku) and 大田区大森東 (Omori-higashi, Ota-ku)
  + 大田区羽田空港 (Haneda Airport, Ota-ku) and 大田区東糀谷 (Higashi-kojiya, Ota-ku)
  + 大田区羽田空港 (Haneda Airport, Ota-ku) and 大田区羽田旭町 (Haneda-asahicho, Ota-ku)
  + 大田区令和島 (Reiwajima, Ota-ku) and 大田区城南島 (Jonanjima, Ota-ku)
  + 大田区平和島 (Heiwajima, Ota-ku) and 品川区勝島 (Katsushima, Shinagawa-ku)
  + 品川区八潮 (Yashio, Shinagawa-ku) and 品川区東八潮 (Higashi-yashio, Shinagawa-ku)
  + 品川区八潮 (Yashio, Shinagawa-ku) and 品川区東大井 (Higashi-oi, Shinagawa-ku)
  + 品川区八潮 (Yashio, Shinagawa-ku) and 品川区東品川 (Higashi-shinagawa, Shinagawa-ku)
  + 品川区東大井 and 品川区勝島 (Katsushima, Shinagawa-ku)
  + 港区海岸 (Kaigan, Minato-ku) and 港区台場  (Daiba, Minato-ku)

## Simulation Notes

* Given that no district can cut across the boundary between the Special Wards area and the Tama area, we group the municipalities in the following way and assign the seats to each region in proportion to the population:
  + (A) Special Wards area except for Koto Ward and Setagaya Ward (18 seats): Note that the islands to the south of Tokyo (島嶼部) are considered to be connected to Minato Ward.
  + (B) Koto Ward (1 seat): We set Koto Ward aside because its population exceeds the target population (i.e., the population of Tokyo divided by the total number of seats)
  + (C) Setagaya Ward (2 seats): We set Setagaya Ward aside and allocate 2 seats to it because its population is more than twice the size of the target population.
  + (D) Tama area (9 seats)
* After running the simulations for the Special Wards area, we remove plans with discontiguous districts as well as plans that split municipalities into three or more districts.
* We sample 160,000 plans for the (A) Special Wards area (except for Koto Ward and Setagaya Ward) across 8 independent runs of the Sequential Monte Carlo (SMC) algorithm. Similarly, we sample 40,000 plans across 4 independent runs of the SMC algorithm for (C) Setagaya Ward and 80,000 plans across 4 independent runs of the SMC algorithm for (D) Tama area. There is no need to conduct a simulation for (B) Koto Ward, as we allocate one seat to the entire ward.
* We do not allow municipalities to be split except for those that were split in the 2017 or 2022 map:  港区 (Minato-ku), 新宿区 (Shinjuku-ku), 台東区 (Taito-ku), 品川区 (Shinagawa-ku), 目黒区 (Meguro-ku), 大田区 (Ota-ku), 世田谷区 (Setagaya-ku), 中野区 (Nakano-ku), 杉並区 (Suginami-ku), 豊島区 (Toshima-ku), 板橋区 (Itabashi-ku), 練馬区 (Nerima-ku), 足立区 (Adachi-ku), 江戸川区 (Edogawa-ku), 八王子市 (Hachioji-shi), 多摩市 (Tama-shi), and 稲城市 (Inagi-shi). We allow these municipalities to be split along the boundaries of Census tracts. 
* In addition, we do not allow counties (*gun*) to be split except for those that were split in the 2017 or 2022 map. In the case of Tokyo, we do not allow any county splits.
* In order to avoid bottlenecks, we set the population temperance at 0.01 for the Special Wards area and Tama area.
* In addition, we set the sequence of alphas, or the amount by which the weights are adjusted at each resampling stage, at 0.95 for both the Special Wards area and Tama area.
* Finally, we remove plans with discontiguous districts as well as plans that split municipalities into three or more districts, after which we thin the sample to down to 5,000 plans.

## Data Sources

- [2020 Census](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136472)
- [2000 Census](https://www.e-stat.go.jp/gis/statmap-search?page=1&type=2&aggregateUnitForBoundary=A&toukeiCode=00200521&toukeiYear=2000&serveyId=A002005212000&coordsys=1&format=shape&datum=2000)
- [Periodical Ferry Route: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N09.html)
- [Lake and Ponds: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-W09-v2_2.html)
- [Vote record of the House of Councilors election of July 21st 2019](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin25/index.html)
- [Vote record of the House of Councilors election of July 10th 2022](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin26/index.html)
