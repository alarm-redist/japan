# 2022 Redistricting for the House of Representatives Electoral Districts in {{**pref_name**}} Prefecture

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

* {{**Hokkaido --> Shinko-kyoku**}}
* {{**Tokyo --> Tama and 23 Wards**}}

### 3. Redistricting commission’s practices
We follow the commission’s practices through:

* Only allowing municipalities to be split based on the boundaries of old municipalities that existed before 1999, wherever possible; and 
* Avoiding splitting municipalities into more than two districts. 

## Algorithmic Constraints

* We enforce a maximum population deviation of {{**pop_tol**}}%.
* {{**If Hokkaido, Saitama, Tokyo, Aichi, and Hyogo --> As noted later, for xx, we use a partial SMC method for xx, simulating y and z separately. We set the pop_tol to xx for region x nd for yy for region y.**}}
* {{**Split constraints --> we add a constraint that penalizes municipality splits as well as a constraint that penalizes the splitting of municipalities into more than two districts.**}
* We also add a constraint to encourage the creation of geographically compact districts.

## Pre-processing Notes

* {{**Prefectures with ferries: We add edges between islands and municipalities/Census tracts to which the islands are connected via ferry routes**}}
* {{**Prefectures with bridges/air routes: we add edges between the following pairs of areas/municipalities because they are connected via bridges/air routes**}}
* {{**Other prefectures: we add edges between x and x, taking into account the fact that…*}}
* {{**Prefectures with lakes: We remove the areas corresponding to xx lake from the Census shapefile.}}
* {{**Otherwise, "No manual pre-processing decisions were necessary."**}}

## Simulation Notes

* We sample {{**n_sim**}} districting plans across {{**runs**}} independent runs of the Sequential Monte Carlo (SMC) algorithm.
* {{**We do not allow municipalities to be split except for those that were split in the 2017 or 2022 map: x, y, and z. We allow these municipalities to be split along the boundaries of pre-merger municipalities (rural prefecture)/Census tracts (urban prefecture). Note that even though XX’ city’s (enter the name of the ordinance-designated city) YY ward is split in the 2022 plan, we do not allow it to be split because it does not have any corresponding pre-merger municipalities.**}}
* {{**In addition, we do not allow counties (*gun*) to be split except for those that were split in the 2017 map: x, y, and z. We allow these counties to be split along the boundaries of municipalities.**}}

* {{**If we changed the population temperance to anything other than 0: we set the population temperance at x to avoid bottlenecks.**}}
* {{**Saitama, Tokyo, Aichi, and Hyogo: Partial SMC**
E.g. We sample x redistricting plans for x region across x independent runs of the Sequential Monte Carlo (SMC) algorithm and y plans for y region across y independent runs of the SMC algorithm.
The areas are defined as follows:
  + x region: A, B, and C
  + y region: D, E, and F

  After removing plans with discontiguous districts as well as those that divide municipalities into more than two districts, we thin the sample to down to 5,000 plans.
}}
<!-- See the following links for detail:
https://github.com/alarm-redist/fifty-states/blob/main/analyses/AZ_cd_2020/doc_AZ_cd_2020.md 
https://github.com/alarm-redist/fifty-states/blob/main/analyses/CA_cd_2020/doc_CA_cd_2020.md
https://github.com/alarm-redist/fifty-states/blob/main/analyses/FL_cd_2020/doc_FL_cd_2020.md
https://github.com/alarm-redist/fifty-states/blob/main/analyses/TX_cd_2020/doc_TX_cd_2020.md
--> 

* (Finally,) we remove plans with discontiguous districts as well as plans that split municipalities into three or more districts, after which we thin the sample to down to 5,000 plans.

## Data Sources

- [2020 Census](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136472)
- [2000 Census](https://www.e-stat.go.jp/gis/statmap-search?page=1&type=2&aggregateUnitForBoundary=A&toukeiCode=00200521&toukeiYear=2000&serveyId=A002005212000&coordsys=1&format=shape&datum=2000)
- [Periodical Ferry Route: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N09.html)
- [Lake and Ponds: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-W09-v2_2.html)
- [Vote record of the House of Councilors election of July 21st 2019](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin25/index.html)
- [Vote record of the House of Councilors election of July 10th 2022](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin26/index.html)
