# 2022 Redistricting for the House of Representatives Electoral Districts in Saitama Prefecture

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

* We enforce a maximum population deviation of 20%.
* As noted later, we use a partial SMC method for Saitama, simulating the Northern and Southern region separately. We set the same maximum population deviation for both regions.
* We add a constraint that penalizes municipality splits as well as a constraint that penalizes the splitting of municipalities into more than two districts.
* We also add a constraint to encourage the creation of geographically compact districts.

## Pre-processing Notes

* No manual pre-processing decisions were necessary.

## Simulation Notes

* We sample 40,000 redistricting plans for the Southern region across 4 independent runs of the Sequential Monte Carlo (SMC) algorithm and 120,000 plans for the Northern region across 8 independent runs of the SMC algorithm.
We group 地域振興センター in the following way and assign the seats proportionally to each region.
The areas are defined as follows:
  + Southern region (5 seats): 南西部地域振興センター, 西部地域振興センター, and 川越比企地域振興センター.
  + Northern region (11 seats): Other areas. Note that since we don’t allow 秩父郡 to be split into different districts, we assign 秩父郡東秩父村 within 秩父地域振興センター.

  At each stage, we remove plans with discontiguous districts as well as those that divide municipalities into more than two districts.
  
* We do not allow municipalities to be split except for those that were split in the 2017 or 2022 map: さいたま市見沼区 (Minuma-ku, Saitama-shi), 熊谷市 (Kuagaya-shi), 川口市 (Kawaguchi-shi), 春日部市 (Kasukabe-shi), 鴻巣市 (Konosu-shi), 越谷市 (Koshigaya-shi), 久喜市 (Kuki-shi), and ふじみ野市 (Fujimino-shi). We allow these municipalities to be split along the boundaries of Census tracts.
* In addition, we do not allow counties (*gun*) to be split except for those that were split in the 2017 map: 入間郡 (Iruma-gun). We allow this county to be split along the boundaries of municipalities.
* We set the population temperance at 0.05 for the Southern region and 0.02 for the Northern region to avoid bottlenecks. Also, when simulating the Northern region, we set the sequence of alphas, or the amount by which the weights are adjusted at each resampling stage, at 0.90.
* Finally, we remove plans with discontiguous districts as well as plans that split municipalities into three or more districts, after which we thin the sample to down to 5,000 plans.

## Data Sources

- [2020 Census](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136472)
- [2000 Census](https://www.e-stat.go.jp/gis/statmap-search?page=1&type=2&aggregateUnitForBoundary=A&toukeiCode=00200521&toukeiYear=2000&serveyId=A002005212000&coordsys=1&format=shape&datum=2000)
- [Periodical Ferry Route: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N09.html)
- [Lake and Ponds: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-W09-v2_2.html)
- [Vote record of the House of Councilors election of July 21st 2019](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin25/index.html)
- [Vote record of the House of Councilors election of July 10th 2022](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin26/index.html)
