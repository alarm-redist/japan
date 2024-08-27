# 2022 Redistricting for the House of Representatives Electoral Districts in Osaka

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

* We enforce a maximum population deviation of 23%.
* We do not add any municipality or county split constraints, as the 2017 and 2022 maps do not split any municipality or county.
* We add a constraint to encourage the creation of geographically compact districts.

## Pre-processing Notes

* We remove the following areas that correspond to Kansai International Airport from the shapefile. The population in all of these areas is 0:

  + 泉佐野市泉州空港北 (Senshukuko-kita, Izumisano-shi)
  + 泉南郡田尻町泉州空港中 (Senshukuko-naka, Tajiri-cho, Sennan-gun)
  + 泉南市泉州空港南 (Senshukuko-minami, Sennan-shi)

* When creating the adjacengy graph, we manually add edges between the following municipalities, as they are connected by roads or bridges:

  + 大阪市西淀川区 (Nishiyodogawa-ku, Osaka-shi) and 大阪市福島区 (Fukushima-ku, Osaka-shi)
  + 大阪市西淀川区 (Nishiyodogawa-ku, Osaka-shi) and 大阪市此花区 (Konohana-ku, Osaka-shi)
  + 大阪市西淀川区 (Nishiyodogawa-ku, Osaka-shi) and 大阪市福島区 (Fukushima-ku, Osaka-shi)
  + 大阪市港区 (Minato-ku, Osaka-shi) and 大阪市住之江区 (Suminoe-ku, Osaka-shi)
  + 大阪市大正区 (Taisho-ku, Osaka-shi) and 大阪市住之江区 (Suminoe-ku, Osaka-shi)
  

## Simulation Notes

* We sample 40,000 districting plans across 4 independent runs of the Sequential Monte Carlo (SMC) algorithm.
* We do not allow municipalities to be split except for those that were split in the 2017 or 2022 map.  In the case of Osaka, we do not allow any municipality to be split.
* In addition, we do not allow counties (*gun*) to be split except for those that were split in the 2017 map. In the case of Osaka, we do not allow any county (*gun*) to be split.
* In order to avoid bottlenecks, we set the population temperance at 0.02.
* Finally, we remove plans with discontiguous districts as well as plans that split municipalities into three or more districts, after which we thin the sample to down to 5,000 plans.

## Data Sources

- [2020 Census](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136472)
- [2000 Census](https://www.e-stat.go.jp/gis/statmap-search?page=1&type=2&aggregateUnitForBoundary=A&toukeiCode=00200521&toukeiYear=2000&serveyId=A002005212000&coordsys=1&format=shape&datum=2000)
- [Periodical Ferry Route: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N09.html)
- [Lake and Ponds: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-W09-v2_2.html)
- [Vote record of the House of Councilors election of July 21st 2019](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin25/index.html)
- [Vote record of the House of Councilors election of July 10th 2022](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin26/index.html)
