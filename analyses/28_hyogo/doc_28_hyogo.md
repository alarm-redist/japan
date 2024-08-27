# 2022 Redistricting for the House of Representatives Electoral Districts in Hyogo

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

* We enforce a maximum population deviation of 25%.
* As noted later, we use a partial SMC method for Hyogo, simulating the (A) Western area (西播磨県民局, 中播磨県民センター, 東播磨県民局, and 淡路県民局) and (B) Eastern area (the remaining municipalities) separately.
* For the (A) Western area, we add a constraint that penalizes the splitting of municipalities into more than two districts. For the (B) Eastern area, we add a constraint that penalizes municipality splits as well as a constraint that penalizes the splitting of municipalities into more than two districts.
* We also add a constraint to encourage the creation of geographically compact districts.

## Pre-processing Notes

* We add edges between the islands of 姫路市 (Himeji-shi) and mainland 姫路市 (Himeji-shi).
* When creating the adjacengy graph, after adding edges between disconnected municipalities/Census blocks and their nearest neighbors, we manually add edges between the following municipalities/Census blocks, as they are connected by roads or bridges:
  + 芦屋市 (Ashiya-shi) and 西宮市西宮浜 (Nishinomiyahama, Nishinomiya-shi)
  + 西宮市西宮浜  (Nishinomiyahama, Nishinomiya-shi) and 西宮市甲子園浜 (Koshienhama, Nishinomiya-shi)
  + 西宮市甲子園浜 (Koshienhama, Nishinomiya-shi) and 西宮市鳴尾浜 (Naruohama, Nishinomiya-shi)
  + 西宮市鳴尾浜 (Naruohama, Nishinomiya-shi) and 尼崎市 (Amagasaki-shi)

## Simulation Notes

* We group the municipalities in the following way and assign the seats to each region in proportion to the population:
  + (A) Western area (4 seats): 
      + 西播磨県民局: 相生市 (Aioi-shi), たつの市 (Tatsuno-shi), 赤穂市 (Ako-shi), 宍粟市 (Shiso-shi), 揖保郡太子町 (Taishi-cho, Ibo-gun), 赤穂郡上郡町 (Kamigori-chom Ako-gun), and 佐用郡佐用町 (Sayo-cho, Sayo-gun)
      + 中播磨県民センター: 姫路市 (Himeji-shi), 神崎郡神河町 (Kamikawa-cho, Kanzaki-gun), 神崎郡市川町 (Ichikawa-cho, Kanzaki-gun), and 神崎郡福崎町 (Fukusaki-cho, Kanzaki-gun)
      + 東播磨県民局: 明石市 (Akashi-shi), 加古川市 (Kakogawa-shi), 高砂市 (Takasago-shi), 加古郡稲美町 (Inami-chom Kako-gun), and 加古郡播磨町 (Harima-cho, Kako-gun)
      + 淡路県民局: 洲本市 (Sumoto-shi), 南あわじ市 (Minami-awaji-shi), and 淡路市 (Himeji-shi)
  + (B) Eastern area (8 seat): all other municipalities
* After running the simulations for the Western area, we remove plans with discontiguous districts as well as plans that split municipalities into three or more districts.
* We sample 320,000 plans for the (A) Western area across 8 independent runs of the SMC algorithm. Similarly, we sample 160,000 plans across 8 independent runs of the SMC algorithm for the (B) Eastern area.
* We do not allow municipalities to be split except for those that were split in the 2017 or 2022 map: 西宮市 (Nishinomiya-shi), 川西市 (Kawanishi-shi), and 姫路市 (Himeji-shi). We allow these municipalities to be split along the boundaries of Census tracts. 
* In addition, we do not allow counties (*gun*) to be split except for those that were split in the 2017 or 2022 map. In the case of Hyogo, we do not allow any county splits.
* In order to avoid bottlenecks, we set the population temperance at 0.03 for both the Western area and Eastern area.
* In addition, we set the sequence of alphas, or the amount by which the weights are adjusted at each resampling stage, at 0.95 for both the Western area and Eastern area.
* Finally, we remove plans with discontiguous districts as well as plans that split municipalities into three or more districts, after which we thin the sample to down to 5,000 plans.

## Data Sources

- [2020 Census](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136472)
- [2000 Census](https://www.e-stat.go.jp/gis/statmap-search?page=1&type=2&aggregateUnitForBoundary=A&toukeiCode=00200521&toukeiYear=2000&serveyId=A002005212000&coordsys=1&format=shape&datum=2000)
- [Periodical Ferry Route: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N09.html)
- [Lake and Ponds: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-W09-v2_2.html)
- [Vote record of the House of Councilors election of July 21st 2019](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin25/index.html)
- [Vote record of the House of Councilors election of July 10th 2022](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin26/index.html)
