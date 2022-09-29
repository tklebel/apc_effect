---
title: "Relationship between OA publishing, APCs and IF"
author: "Thomas Klebel"
date: "29 September, 2022"
output: 
  html_document:
    keep_md: true
---



# Sample description 
## Institutions per country

```r
universities_per_country <- works %>% 
  distinct(country_code, institution_id) %>% 
  count(country_code, name = "n_universities") %>% 
  arrange(desc(n_universities)) %>% 
  collect() %>% 
  add_country()

universities_per_country %>% 
  select(country, n_universities) %>% 
  knitr::kable()
```



<<<<<<< HEAD
|country                        | n_universities|
|:------------------------------|--------------:|
|United States                  |           6808|
|India                          |           2061|
|China                          |           1985|
|United Kingdom                 |           1610|
|Japan                          |           1444|
|Germany                        |           1033|
|France                         |           1024|
|Brazil                         |            807|
|Canada                         |            746|
|Russian Federation             |            665|
|Spain                          |            642|
|Italy                          |            582|
|Australia                      |            538|
|Korea, Rep.                    |            478|
|NA                             |            433|
|Indonesia                      |            407|
|Poland                         |            333|
|Netherlands                    |            329|
|Switzerland                    |            293|
|Taiwan, China                  |            264|
|Pakistan                       |            240|
|Turkiye                        |            240|
|Mexico                         |            236|
|Iran, Islamic Rep.             |            234|
|Norway                         |            219|
|Colombia                       |            205|
|Ukraine                        |            199|
|Czech Republic                 |            195|
|Sweden                         |            186|
|Portugal                       |            181|
|Austria                        |            177|
|Nigeria                        |            171|
|South Africa                   |            168|
|Belgium                        |            158|
|Thailand                       |            156|
|Argentina                      |            151|
|Greece                         |            145|
|Finland                        |            144|
|Denmark                        |            142|
|Ireland                        |            138|
|Bangladesh                     |            132|
|Philippines                    |            122|
|Malaysia                       |            121|
|Israel                         |            120|
|Hungary                        |            112|
|New Zealand                    |            108|
|Vietnam                        |            103|
|Chile                          |             97|
|Singapore                      |             97|
|Egypt, Arab Rep.               |             96|
|Saudi Arabia                   |             91|
|Slovak Republic                |             89|
|Kenya                          |             86|
|Romania                        |             80|
|Bulgaria                       |             80|
|Peru                           |             78|
|Slovenia                       |             76|
|Ecuador                        |             65|
|Uganda                         |             64|
|United Arab Emirates           |             60|
|Serbia                         |             60|
|Croatia                        |             59|
|Ethiopia                       |             54|
|Ghana                          |             54|
|Iraq                           |             52|
|Kazakhstan                     |             51|
|Tanzania                       |             48|
|Algeria                        |             47|
|Nepal                          |             46|
|Belarus                        |             40|
|Venezuela, RB                  |             39|
|Latvia                         |             38|
|Cuba                           |             38|
|Sri Lanka                      |             38|
|Lithuania                      |             36|
|Estonia                        |             33|
|Tunisia                        |             33|
|Jordan                         |             32|
|Zimbabwe                       |             28|
|Morocco                        |             28|
|Uzbekistan                     |             27|
|Cameroon                       |             27|
|Uruguay                        |             27|
|Sudan                          |             27|
|Lebanon                        |             26|
|Cyprus                         |             26|
|Costa Rica                     |             25|
|Armenia                        |             23|
|Luxembourg                     |             23|
|Georgia                        |             22|
|Oman                           |             21|
|Albania                        |             20|
|Cambodia                       |             20|
|Bolivia                        |             20|
|Azerbaijan                     |             19|
|Bosnia and Herzegovina         |             19|
|Syrian Arab Republic           |             19|
|Myanmar                        |             19|
|Kuwait                         |             19|
|Qatar                          |             19|
|Iceland                        |             19|
|Congo, Dem. Rep.               |             18|
|West Bank and Gaza             |             17|
|Paraguay                       |             17|
|Zambia                         |             16|
|Panama                         |             15|
|Malawi                         |             15|
|Dominican Republic             |             15|
|Mongolia                       |             14|
|Yemen, Rep.                    |             14|
|Mozambique                     |             14|
|Moldova                        |             14|
|Kyrgyz Republic                |             14|
|Senegal                        |             14|
|Bahrain                        |             13|
|North Macedonia                |             13|
|Guatemala                      |             13|
|Burkina Faso                   |             13|
|Rwanda                         |             12|
|Hong Kong SAR, China           |             11|
|Botswana                       |             10|
|Afghanistan                    |             10|
|Cote d'Ivoire                  |             10|
|Libya                          |             10|
|Nicaragua                      |              9|
|Tajikistan                     |              9|
|El Salvador                    |              9|
|Somalia                        |              8|
|Trinidad and Tobago            |              8|
|Angola                         |              7|
|Namibia                        |              7|
|Papua New Guinea               |              7|
|Madagascar                     |              7|
|Niger                          |              6|
|Malta                          |              6|
|Mauritius                      |              6|
|Mali                           |              6|
|Lao PDR                        |              5|
|Montenegro                     |              5|
|Honduras                       |              5|
|Kosovo                         |              5|
|Jamaica                        |              5|
|Fiji                           |              5|
|Gambia, The                    |              5|
|Benin                          |              5|
|Korea, Dem. People's Rep.      |              4|
|South Sudan                    |              4|
|Togo                           |              4|
|Curacao                        |              4|
|Gabon                          |              4|
|Brunei Darussalam              |              4|
|Greenland                      |              3|
|Liechtenstein                  |              3|
|Bermuda                        |              3|
|Monaco                         |              3|
|Belize                         |              3|
|St. Kitts and Nevis            |              3|
|Mauritania                     |              3|
|Sierra Leone                   |              3|
|Bahamas, The                   |              3|
|Turkmenistan                   |              3|
|Liberia                        |              3|
|French Polynesia               |              3|
|Puerto Rico                    |              3|
|Guinea-Bissau                  |              3|
|Maldives                       |              3|
|Seychelles                     |              2|
|Congo, Rep.                    |              2|
|Sint Maarten (Dutch part)      |              2|
|NA                             |              2|
|Burundi                        |              2|
|Guyana                         |              2|
|Bhutan                         |              2|
|Haiti                          |              2|
|Eswatini                       |              2|
|New Caledonia                  |              2|
|Macao SAR, China               |              2|
|Suriname                       |              2|
|Chad                           |              2|
|NA                             |              2|
|St. Lucia                      |              2|
|Antigua and Barbuda            |              2|
|Andorra                        |              1|
|Timor-Leste                    |              1|
|Gibraltar                      |              1|
|Marshall Islands               |              1|
|Micronesia, Fed. Sts.          |              1|
|Lesotho                        |              1|
|St. Vincent and the Grenadines |              1|
|Eritrea                        |              1|
|NA                             |              1|
|NA                             |              1|
|British Virgin Islands         |              1|
|Faroe Islands                  |              1|
|NA                             |              1|
|Grenada                        |              1|
|Aruba                          |              1|
|Guinea                         |              1|
|Cayman Islands                 |              1|
|Palau                          |              1|
|Isle of Man                    |              1|
|Cabo Verde                     |              1|
|NA                             |              1|
|NA                             |              1|
|NA                             |              1|
|NA                             |              1|
|Central African Republic       |              1|
|Barbados                       |              1|
|Sao Tome and Principe          |              1|
=======
|country              | n_universities|
|:--------------------|--------------:|
|China                |            216|
|United States        |            199|
|United Kingdom       |             60|
|Japan                |             55|
|Germany              |             54|
|South Korea          |             46|
|Spain                |             42|
|Italy                |             41|
|India                |             38|
|Iran                 |             36|
|Australia            |             32|
|Brazil               |             31|
|Poland               |             31|
|Turkey               |             31|
|Canada               |             30|
|France               |             28|
|Taiwan               |             21|
|Netherlands          |             13|
|Sweden               |             12|
|Austria              |             12|
|Russia               |             10|
|South Africa         |              9|
|Belgium              |              8|
|Israel               |              8|
|Switzerland          |              8|
|Egypt                |              8|
|Greece               |              8|
|Czech Republic       |              7|
|Finland              |              7|
|New Zealand          |              7|
|Mexico               |              6|
|Thailand             |              6|
|Norway               |              6|
|Hungary              |              6|
|Malaysia             |              6|
|Ireland              |              6|
|Portugal             |              6|
|Saudi Arabia         |              5|
|Pakistan             |              5|
|Denmark              |              5|
|Singapore            |              3|
|Chile                |              3|
|Tunisia              |              3|
|Romania              |              3|
|Argentina            |              3|
|Colombia             |              3|
|Serbia               |              3|
|Jordan               |              2|
|Slovenia             |              2|
|United Arab Emirates |              2|
|Slovakia             |              2|
|Nigeria              |              2|
|Ghana                |              1|
|Lithuania            |              1|
|Uruguay              |              1|
|Algeria              |              1|
|Cyprus               |              1|
|Iceland              |              1|
|Oman                 |              1|
|Kuwait               |              1|
|Uganda               |              1|
|Qatar                |              1|
|Ethiopia             |              1|
|Viet Nam             |              1|
|Luxembourg           |              1|
|Croatia              |              1|
|Estonia              |              1|
|Morocco              |              1|
|Lebanon              |              1|
>>>>>>> main


```r
# papers per country
papers_per_country <- works %>% 
  distinct(country_code, id, work_frac, author_position, institution_id) %>% 
  group_by(country_code) %>% 
  summarise(sum_fractional_works = sum(work_frac) %>% round(digits = 1)) %>% 
  arrange(desc(sum_fractional_works)) %>% 
  collect() %>% 
  add_country()
```


```r
papers_per_country %>% 
  select(country, country_code, sum_fractional_works) %>% 
  knitr::kable()
```



<<<<<<< HEAD
|country                        |country_code | sum_fractional_works|
|:------------------------------|:------------|--------------------:|
|United States                  |US           |             203117.5|
|Brazil                         |BR           |             133621.1|
|China                          |CN           |             125471.6|
|United Kingdom                 |GB           |              52770.8|
|Spain                          |ES           |              42338.3|
|Germany                        |DE           |              41978.4|
|India                          |IN           |              35913.0|
|Canada                         |CA           |              33396.2|
|Japan                          |JP           |              32637.3|
|Australia                      |AU           |              26479.1|
|Korea, Rep.                    |KR           |              26068.3|
|Italy                          |IT           |              24002.1|
|Poland                         |PL           |              22138.4|
|France                         |FR           |              21485.3|
|Indonesia                      |ID           |              20531.2|
|South Africa                   |ZA           |              14615.0|
|Iran, Islamic Rep.             |IR           |              14492.1|
|Netherlands                    |NL           |              14466.7|
|Mexico                         |MX           |              14207.0|
|Taiwan, China                  |TW           |              13904.9|
|Russian Federation             |RU           |              13024.2|
|Colombia                       |CO           |              12862.2|
|Sweden                         |SE           |              12675.9|
|Turkiye                        |TR           |              11371.6|
|Switzerland                    |CH           |              10476.5|
|Malaysia                       |MY           |               9665.3|
|Argentina                      |AR           |               9441.5|
|Portugal                       |PT           |               8995.1|
|Belgium                        |BE           |               7863.8|
|Chile                          |CL           |               7724.4|
|Norway                         |NO           |               7441.1|
|Denmark                        |DK           |               7122.5|
|Saudi Arabia                   |SA           |               6564.3|
|Czech Republic                 |CZ           |               5953.0|
|Israel                         |IL           |               5819.2|
|Austria                        |AT           |               5562.1|
|Pakistan                       |PK           |               5545.0|
|Egypt, Arab Rep.               |EG           |               5508.9|
|Finland                        |FI           |               4861.7|
|Ukraine                        |UA           |               4487.1|
|Thailand                       |TH           |               4401.0|
|Nigeria                        |NG           |               4234.0|
|Singapore                      |SG           |               4192.0|
|New Zealand                    |NZ           |               3808.6|
|Greece                         |GR           |               3521.3|
|Ireland                        |IE           |               3369.2|
|NA                             |NA           |               3025.4|
|Hungary                        |HU           |               2867.2|
|Serbia                         |RS           |               2729.7|
|Ethiopia                       |ET           |               2692.8|
|Croatia                        |HR           |               2683.6|
|Romania                        |RO           |               2662.8|
|Peru                           |PE           |               2632.4|
|Slovenia                       |SI           |               2283.8|
|Slovak Republic                |SK           |               2183.5|
|Bangladesh                     |BD           |               1954.4|
|Costa Rica                     |CR           |               1879.4|
|Ecuador                        |EC           |               1547.1|
|Bulgaria                       |BG           |               1389.9|
|Lithuania                      |LT           |               1386.7|
|Ghana                          |GH           |               1339.3|
|Iraq                           |IQ           |               1281.8|
|Bahrain                        |BH           |               1078.6|
|Morocco                        |MA           |               1070.8|
|Nepal                          |NP           |               1056.8|
|Uruguay                        |UY           |               1006.5|
|United Arab Emirates           |AE           |                994.7|
|Jordan                         |JO           |                992.7|
|Vietnam                        |VN           |                986.5|
|Estonia                        |EE           |                985.8|
|Sri Lanka                      |LK           |                858.5|
|Kenya                          |KE           |                854.9|
|Philippines                    |PH           |                775.4|
|Qatar                          |QA           |                761.7|
|Lebanon                        |LB           |                738.6|
|Venezuela, RB                  |VE           |                724.4|
|Tunisia                        |TN           |                720.5|
|Uganda                         |UG           |                604.0|
|Mozambique                     |MZ           |                592.3|
|Cyprus                         |CY           |                582.4|
|Tanzania                       |TZ           |                549.3|
|Oman                           |OM           |                524.7|
|Kuwait                         |KW           |                510.6|
|Cameroon                       |CM           |                490.3|
|Latvia                         |LV           |                441.9|
|Bosnia and Herzegovina         |BA           |                425.8|
|Cuba                           |CU           |                403.0|
|Algeria                        |DZ           |                359.4|
|Iceland                        |IS           |                329.0|
|Zimbabwe                       |ZW           |                326.9|
|El Salvador                    |SV           |                316.0|
|Luxembourg                     |LU           |                303.4|
|Armenia                        |AM           |                289.9|
|Jamaica                        |JM           |                288.5|
|Kazakhstan                     |KZ           |                277.9|
|Hong Kong SAR, China           |HK           |                277.5|
|West Bank and Gaza             |PS           |                276.2|
|Benin                          |BJ           |                267.3|
|Belarus                        |BY           |                255.3|
|Sudan                          |SD           |                254.0|
|Antigua and Barbuda            |AG           |                238.8|
|Puerto Rico                    |PR           |                235.8|
|Paraguay                       |PY           |                209.9|
|Cambodia                       |KH           |                209.4|
|Syrian Arab Republic           |SY           |                197.8|
|Malta                          |MT           |                178.1|
|Botswana                       |BW           |                177.6|
|Panama                         |PA           |                168.5|
|Zambia                         |ZM           |                166.0|
|Bolivia                        |BO           |                159.1|
|Uzbekistan                     |UZ           |                153.0|
|Montenegro                     |ME           |                145.9|
|Lao PDR                        |LA           |                144.4|
|Azerbaijan                     |AZ           |                142.7|
|Albania                        |AL           |                141.6|
|Kosovo                         |XK           |                141.5|
|North Macedonia                |MK           |                141.4|
|Guatemala                      |GT           |                127.2|
|Malawi                         |MW           |                124.6|
|Angola                         |AO           |                123.5|
|Yemen, Rep.                    |YE           |                122.2|
|Nicaragua                      |NI           |                118.7|
|Georgia                        |GE           |                114.2|
|Senegal                        |SN           |                 98.7|
|Mongolia                       |MN           |                 82.4|
|Brunei Darussalam              |BN           |                 80.1|
|Libya                          |LY           |                 74.2|
|Sao Tome and Principe          |ST           |                 73.2|
|Bahamas, The                   |BS           |                 70.7|
|NA                             |RE           |                 70.6|
|Namibia                        |NA           |                 68.1|
|Rwanda                         |RW           |                 65.0|
|Myanmar                        |MM           |                 63.3|
|Burkina Faso                   |BF           |                 62.0|
|Moldova                        |MD           |                 58.6|
|St. Kitts and Nevis            |KN           |                 58.2|
|Dominican Republic             |DO           |                 57.1|
|Mauritius                      |MU           |                 56.6|
|Cote d'Ivoire                  |CI           |                 53.3|
|Fiji                           |FJ           |                 51.1|
|Grenada                        |GD           |                 46.4|
|Mali                           |ML           |                 45.8|
|NA                             |GP           |                 45.2|
|Congo, Dem. Rep.               |CD           |                 44.5|
|Barbados                       |BB           |                 41.1|
|Honduras                       |HN           |                 39.6|
|Tajikistan                     |TJ           |                 32.6|
|Kyrgyz Republic                |KG           |                 31.4|
|Togo                           |TG           |                 31.3|
|Papua New Guinea               |PG           |                 28.9|
|Gabon                          |GA           |                 25.7|
|Lesotho                        |LS           |                 25.3|
|Trinidad and Tobago            |TT           |                 23.6|
|NA                             |SJ           |                 22.6|
|Niger                          |NE           |                 21.5|
|French Polynesia               |PF           |                 20.3|
|Afghanistan                    |AF           |                 18.1|
|Bhutan                         |BT           |                 16.5|
|Korea, Dem. People's Rep.      |KP           |                 16.0|
|Eswatini                       |SZ           |                 15.9|
|British Virgin Islands         |VG           |                 15.1|
|Cabo Verde                     |CV           |                 15.0|
|Sierra Leone                   |SL           |                 14.7|
|Bermuda                        |BM           |                 14.5|
|Congo, Rep.                    |CG           |                 13.6|
|Gambia, The                    |GM           |                 12.2|
|Madagascar                     |MG           |                 12.1|
|Guinea-Bissau                  |GW           |                 12.0|
|Guinea                         |GN           |                 10.8|
|Suriname                       |SR           |                 10.3|
|Curacao                        |CW           |                  9.8|
|Faroe Islands                  |FO           |                  9.6|
|Greenland                      |GL           |                  9.1|
|New Caledonia                  |NC           |                  7.9|
|Guyana                         |GY           |                  7.9|
|Burundi                        |BI           |                  6.1|
|Liechtenstein                  |LI           |                  5.5|
|St. Vincent and the Grenadines |VC           |                  4.2|
|NA                             |GF           |                  4.2|
|Marshall Islands               |MH           |                  4.1|
|Sint Maarten (Dutch part)      |SX           |                  3.3|
|Somalia                        |SO           |                  3.2|
|Monaco                         |MC           |                  3.1|
|South Sudan                    |SS           |                  2.9|
|Turkmenistan                   |TM           |                  2.5|
|Mauritania                     |MR           |                  2.4|
|St. Lucia                      |LC           |                  2.3|
|Chad                           |TD           |                  2.3|
|Maldives                       |MV           |                  2.0|
|NA                             |JE           |                  2.0|
|Liberia                        |LR           |                  1.9|
|Macao SAR, China               |MO           |                  1.5|
|Belize                         |BZ           |                  1.5|
|Cayman Islands                 |KY           |                  1.0|
|Micronesia, Fed. Sts.          |FM           |                  1.0|
|Andorra                        |AD           |                  0.8|
|Timor-Leste                    |TL           |                  0.8|
|Haiti                          |HT           |                  0.8|
|Aruba                          |AW           |                  0.7|
|Palau                          |PW           |                  0.6|
|Gibraltar                      |GI           |                  0.6|
|Seychelles                     |SC           |                  0.5|
|Eritrea                        |ER           |                  0.5|
|NA                             |MQ           |                  0.5|
|Central African Republic       |CF           |                  0.4|
|NA                             |MS           |                  0.3|
|Isle of Man                    |IM           |                  0.3|
|NA                             |VA           |                  0.1|
|NA                             |AX           |                  0.1|
=======
|country              | sum_fractional_works|
|:--------------------|--------------------:|
|China                |             460832.3|
|United States        |             456678.0|
|Brazil               |             266241.3|
|United Kingdom       |             130906.7|
|Germany              |             114970.8|
|Japan                |              90599.2|
|Canada               |              87827.8|
|Spain                |              84138.8|
|South Korea          |              79297.7|
|Australia            |              78220.5|
|Italy                |              77622.7|
|Netherlands          |              42592.2|
|Taiwan               |              40111.3|
|Sweden               |              38855.0|
|Poland               |              37813.5|
|France               |              31836.3|
|Switzerland          |              29575.2|
|Iran                 |              28694.7|
|India                |              27611.5|
|South Africa         |              25886.0|
|Belgium              |              24577.5|
|Denmark              |              19803.7|
|Malaysia             |              18720.5|
|Turkey               |              18233.7|
|Mexico               |              17757.8|
|Austria              |              17341.0|
|Portugal             |              16621.8|
|Israel               |              15475.0|
|Norway               |              14952.8|
|Saudi Arabia         |              12935.7|
|Finland              |              12476.2|
|Russia               |              11144.3|
|Singapore            |              11055.0|
|Egypt                |              10171.3|
|Thailand             |              10076.0|
|Greece               |               9671.2|
|New Zealand          |               9474.0|
|Colombia             |               9076.3|
|Czech Republic       |               8389.5|
|Chile                |               8320.2|
|Ireland              |               7870.3|
|Argentina            |               7579.8|
|Hungary              |               6309.3|
|Serbia               |               6008.7|
|Pakistan             |               4285.5|
|Slovenia             |               4175.7|
|Croatia              |               3819.3|
|Nigeria              |               2140.8|
|Romania              |               1835.3|
|Uruguay              |               1702.5|
|Slovakia             |               1628.2|
|Estonia              |               1590.0|
|Uganda               |               1561.7|
|Jordan               |               1508.5|
|United Arab Emirates |               1500.0|
|Lebanon              |               1475.3|
|Oman                 |               1250.8|
|Lithuania            |               1221.0|
|Tunisia              |               1221.0|
|Ghana                |               1194.2|
|Ethiopia             |               1106.8|
|Kuwait               |                876.5|
|Iceland              |                778.3|
|Qatar                |                650.8|
|Luxembourg           |                643.8|
|Morocco              |                538.3|
|Viet Nam             |                463.5|
|Cyprus               |                383.8|
|Algeria              |                117.8|
>>>>>>> main


```r
# average apc
average_apc <- works %>%
  # first get rid of duplicates from concepts
  distinct(country_code, id, work_frac, author_position, institution_id,
           APC_in_dollar) %>% 
  group_by(country_code) %>%
  # compute the average APC using fractional authorships as weights
  mutate(sum_frac = sum(work_frac)) %>%
  group_by(country_code, sum_frac) %>%
  summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac) %>% 
  collect() %>% 
  add_country()
```

```
## `summarise()` has grouped output by 'country_code'. You can override using the
## `.groups` argument.
```


```r
# average APC over time
average_apc_time <- works %>%
  # first get rid of duplicates from concepts
  distinct(country_code, id, work_frac, author_position, institution_id,
           APC_in_dollar, publication_year) %>% 
  group_by(country_code, publication_year) %>%
  # compute the average APC using fractional authorships as weights
  mutate(sum_frac = sum(work_frac)) %>%
  group_by(country_code, sum_frac, publication_year) %>%
  summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac) %>% 
  collect() 
```

```
## `summarise()` has grouped output by 'country_code', 'sum_frac'. You can override
## using the `.groups` argument.
```


```r
average_apc_time %>% 
  left_join(wdi, by = c("country_code" = "iso2c")) %>% 
  ggplot(aes(publication_year, mean_apc)) +
  geom_line(aes(group = country), alpha = .3) +
  geom_smooth(se = FALSE, colour = "#007FA8") +
  facet_wrap(vars(region)) +
  scale_x_continuous(breaks = scales::pretty_breaks(6)) +
  coord_cartesian(ylim = c(0, 3000)) +
  labs(x = NULL, y = "Mean APC")
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](20-APC-analysis_files/figure-html/apc-region-time-1.png)<!-- -->



All three joined

```r
all_three_descriptives <- universities_per_country %>% 
  left_join(papers_per_country, by = c("country", "country_code")) %>% 
  left_join(average_apc, by = c("country", "country_code")) %>% 
  # remove missing values
  # these arise since the wdi data on country codes does not have a mapping for
  # all codes that are present in the data. in most cases, these are small 
  # countries, and in the case of a high university count (433) simply all
  # institutions that were not assigned a country code at all.
  drop_na()

all_three_descriptives %>% 
  arrange(desc(n_universities), desc(sum_fractional_works)) %>% 
  mutate(mean_apc = round(mean_apc, digits = 1),
         sum_fractional_works = scales::comma(sum_fractional_works)) %>% 
  select(Country = country, `n universities` = n_universities,
         `n fractional publications` = sum_fractional_works, 
         `Mean APC` = mean_apc) %>% 
  knitr::kable()
```



<<<<<<< HEAD
|Country                        | n universities|n fractional publications | Mean APC|
|:------------------------------|--------------:|:-------------------------|--------:|
|United States                  |           6808|203,117.500               |   1834.2|
|India                          |           2061|35,913.000                |    758.8|
|China                          |           1985|125,471.600               |   1823.6|
|United Kingdom                 |           1610|52,770.800                |   1792.4|
|Japan                          |           1444|32,637.300                |   1778.9|
|Germany                        |           1033|41,978.400                |   1838.9|
|France                         |           1024|21,485.300                |   1596.0|
|Brazil                         |            807|133,621.100               |    232.4|
|Canada                         |            746|33,396.200                |   1666.3|
|Russian Federation             |            665|13,024.200                |    474.8|
|Spain                          |            642|42,338.300                |    804.4|
|Italy                          |            582|24,002.100                |   1673.6|
|Australia                      |            538|26,479.100                |   1804.4|
|Korea, Rep.                    |            478|26,068.300                |   1676.7|
|Indonesia                      |            407|20,531.200                |    153.7|
|Poland                         |            333|22,138.400                |    779.0|
|Netherlands                    |            329|14,466.700                |   1851.9|
|Switzerland                    |            293|10,476.500                |   1912.1|
|Taiwan, China                  |            264|13,904.900                |   1800.1|
|Turkiye                        |            240|11,371.600                |    781.4|
|Pakistan                       |            240|5,545.000                 |   1083.2|
|Mexico                         |            236|14,207.000                |    582.3|
|Iran, Islamic Rep.             |            234|14,492.100                |    721.1|
|Norway                         |            219|7,441.100                 |   1597.1|
|Colombia                       |            205|12,862.200                |    198.6|
|Ukraine                        |            199|4,487.100                 |    290.1|
|Czech Republic                 |            195|5,953.000                 |   1054.1|
|Sweden                         |            186|12,675.900                |   1786.4|
|Portugal                       |            181|8,995.100                 |    784.8|
|Austria                        |            177|5,562.100                 |   1677.0|
|Nigeria                        |            171|4,234.000                 |    824.5|
|South Africa                   |            168|14,615.000                |    978.7|
|Belgium                        |            158|7,863.800                 |   1676.6|
|Thailand                       |            156|4,401.000                 |   1397.1|
|Argentina                      |            151|9,441.500                 |    398.2|
|Greece                         |            145|3,521.300                 |   1446.4|
|Finland                        |            144|4,861.700                 |   1598.4|
|Denmark                        |            142|7,122.500                 |   1778.9|
|Ireland                        |            138|3,369.200                 |   1738.8|
|Bangladesh                     |            132|1,954.400                 |    766.2|
|Philippines                    |            122|775.400                   |    931.1|
|Malaysia                       |            121|9,665.300                 |   1053.4|
|Israel                         |            120|5,819.200                 |   1917.2|
|Hungary                        |            112|2,867.200                 |   1250.0|
|New Zealand                    |            108|3,808.600                 |   1579.4|
|Vietnam                        |            103|986.500                   |   1254.8|
|Chile                          |             97|7,724.400                 |    521.6|
|Singapore                      |             97|4,192.000                 |   1874.8|
|Egypt, Arab Rep.               |             96|5,508.900                 |    912.7|
|Saudi Arabia                   |             91|6,564.300                 |   1308.2|
|Slovak Republic                |             89|2,183.500                 |    628.9|
|Kenya                          |             86|854.900                   |   1514.6|
|Romania                        |             80|2,662.800                 |    833.5|
|Bulgaria                       |             80|1,389.900                 |    513.7|
|Peru                           |             78|2,632.400                 |    155.5|
|Slovenia                       |             76|2,283.800                 |    961.9|
|Ecuador                        |             65|1,547.100                 |    261.7|
|Uganda                         |             64|604.000                   |   1754.4|
|Serbia                         |             60|2,729.700                 |    538.9|
|United Arab Emirates           |             60|994.700                   |   1404.5|
|Croatia                        |             59|2,683.600                 |    437.4|
|Ethiopia                       |             54|2,692.800                 |   1576.0|
|Ghana                          |             54|1,339.300                 |   1375.4|
|Iraq                           |             52|1,281.800                 |    658.6|
|Kazakhstan                     |             51|277.900                   |    968.9|
|Tanzania                       |             48|549.300                   |   1648.7|
|Algeria                        |             47|359.400                   |    505.7|
|Nepal                          |             46|1,056.800                 |    739.2|
|Belarus                        |             40|255.300                   |    509.3|
|Venezuela, RB                  |             39|724.400                   |    240.7|
|Sri Lanka                      |             38|858.500                   |   1159.4|
|Latvia                         |             38|441.900                   |    508.6|
|Cuba                           |             38|403.000                   |    386.5|
|Lithuania                      |             36|1,386.700                 |    819.0|
|Estonia                        |             33|985.800                   |    893.3|
|Tunisia                        |             33|720.500                   |   1068.2|
|Jordan                         |             32|992.700                   |   1195.4|
|Morocco                        |             28|1,070.800                 |   1562.6|
|Zimbabwe                       |             28|326.900                   |   1082.5|
|Uruguay                        |             27|1,006.500                 |    413.3|
|Cameroon                       |             27|490.300                   |   1434.3|
|Sudan                          |             27|254.000                   |   1272.5|
|Uzbekistan                     |             27|153.000                   |    488.3|
|Lebanon                        |             26|738.600                   |   1449.2|
|Cyprus                         |             26|582.400                   |   1292.7|
|Costa Rica                     |             25|1,879.400                 |    106.2|
|Luxembourg                     |             23|303.400                   |   1740.8|
|Armenia                        |             23|289.900                   |   1349.2|
|Georgia                        |             22|114.200                   |    853.7|
|Oman                           |             21|524.700                   |    528.2|
|Cambodia                       |             20|209.400                   |    975.8|
|Bolivia                        |             20|159.100                   |    166.9|
|Albania                        |             20|141.600                   |    434.2|
|Qatar                          |             19|761.700                   |   1327.9|
|Kuwait                         |             19|510.600                   |   1467.0|
|Bosnia and Herzegovina         |             19|425.800                   |    340.9|
|Iceland                        |             19|329.000                   |   1345.1|
|Syrian Arab Republic           |             19|197.800                   |    831.1|
|Azerbaijan                     |             19|142.700                   |    620.6|
|Myanmar                        |             19|63.300                    |    522.0|
|Congo, Dem. Rep.               |             18|44.500                    |   1426.9|
|West Bank and Gaza             |             17|276.200                   |   1419.5|
|Paraguay                       |             17|209.900                   |     79.2|
|Zambia                         |             16|166.000                   |   1527.3|
|Panama                         |             15|168.500                   |   1354.7|
|Malawi                         |             15|124.600                   |   1749.7|
|Dominican Republic             |             15|57.100                    |    439.5|
|Mozambique                     |             14|592.300                   |    257.5|
|Yemen, Rep.                    |             14|122.200                   |   1044.3|
|Senegal                        |             14|98.700                    |    942.6|
|Mongolia                       |             14|82.400                    |    554.6|
|Moldova                        |             14|58.600                    |    657.7|
|Kyrgyz Republic                |             14|31.400                    |   1126.3|
|Bahrain                        |             13|1,078.600                 |   1610.1|
|North Macedonia                |             13|141.400                   |    447.3|
|Guatemala                      |             13|127.200                   |    327.1|
|Burkina Faso                   |             13|62.000                    |   1704.4|
|Rwanda                         |             12|65.000                    |   1537.9|
|Hong Kong SAR, China           |             11|277.500                   |   1767.6|
|Botswana                       |             10|177.600                   |    845.7|
|Libya                          |             10|74.200                    |    855.0|
|Cote d'Ivoire                  |             10|53.300                    |    888.9|
|Afghanistan                    |             10|18.100                    |    891.3|
|El Salvador                    |              9|316.000                   |     27.4|
|Nicaragua                      |              9|118.700                   |    117.4|
|Tajikistan                     |              9|32.600                    |    180.4|
|Trinidad and Tobago            |              8|23.600                    |    892.8|
|Somalia                        |              8|3.200                     |    922.9|
|Angola                         |              7|123.500                   |    136.6|
|Namibia                        |              7|68.100                    |   1082.1|
|Papua New Guinea               |              7|28.900                    |   1175.2|
|Madagascar                     |              7|12.100                    |   1135.1|
|Malta                          |              6|178.100                   |   1060.9|
|Mauritius                      |              6|56.600                    |    948.3|
|Mali                           |              6|45.800                    |   1861.3|
|Niger                          |              6|21.500                    |   1341.9|
|Jamaica                        |              5|288.500                   |   1294.6|
|Benin                          |              5|267.300                   |   1542.1|
|Montenegro                     |              5|145.900                   |    443.6|
|Lao PDR                        |              5|144.400                   |   1535.2|
|Kosovo                         |              5|141.500                   |    657.2|
|Fiji                           |              5|51.100                    |   1053.1|
|Honduras                       |              5|39.600                    |    552.8|
|Gambia, The                    |              5|12.200                    |   1330.9|
|Brunei Darussalam              |              4|80.100                    |    866.3|
|Togo                           |              4|31.300                    |   1168.4|
|Gabon                          |              4|25.700                    |   2010.2|
|Korea, Dem. People's Rep.      |              4|16.000                    |    522.6|
|Curacao                        |              4|9.800                     |   1305.8|
|South Sudan                    |              4|2.900                     |   1242.2|
|Puerto Rico                    |              3|235.800                   |   1745.7|
|Bahamas, The                   |              3|70.700                    |     68.2|
|St. Kitts and Nevis            |              3|58.200                    |   1867.0|
|French Polynesia               |              3|20.300                    |   1685.9|
|Sierra Leone                   |              3|14.700                    |   1733.4|
|Bermuda                        |              3|14.500                    |   1786.1|
|Guinea-Bissau                  |              3|12.000                    |   2115.5|
|Greenland                      |              3|9.100                     |    925.7|
|Liechtenstein                  |              3|5.500                     |   1038.0|
|Monaco                         |              3|3.100                     |    559.7|
|Turkmenistan                   |              3|2.500                     |    583.0|
|Mauritania                     |              3|2.400                     |   2160.2|
|Maldives                       |              3|2.000                     |    561.7|
|Liberia                        |              3|1.900                     |   1894.5|
|Belize                         |              3|1.500                     |    493.6|
|Antigua and Barbuda            |              2|238.800                   |    782.5|
|Bhutan                         |              2|16.500                    |    284.9|
|Eswatini                       |              2|15.900                    |   1097.1|
|Congo, Rep.                    |              2|13.600                    |   1481.1|
|Suriname                       |              2|10.300                    |   1664.0|
|Guyana                         |              2|7.900                     |    841.2|
|New Caledonia                  |              2|7.900                     |   1129.7|
|Burundi                        |              2|6.100                     |    899.1|
|Sint Maarten (Dutch part)      |              2|3.300                     |    716.5|
|Chad                           |              2|2.300                     |   1534.5|
|St. Lucia                      |              2|2.300                     |   1605.0|
|Haiti                          |              2|0.800                     |   1184.9|
|Seychelles                     |              2|0.500                     |   1607.5|
|Sao Tome and Principe          |              1|73.200                    |    238.4|
|Grenada                        |              1|46.400                    |   1274.2|
|Barbados                       |              1|41.100                    |   1034.9|
|Lesotho                        |              1|25.300                    |    574.5|
|British Virgin Islands         |              1|15.100                    |   1298.5|
|Cabo Verde                     |              1|15.000                    |    221.9|
|Guinea                         |              1|10.800                    |   1612.1|
|Faroe Islands                  |              1|9.600                     |   1128.0|
|St. Vincent and the Grenadines |              1|4.200                     |   1091.3|
|Marshall Islands               |              1|4.100                     |    448.7|
|Micronesia, Fed. Sts.          |              1|1.000                     |   1288.9|
|Cayman Islands                 |              1|1.000                     |    422.1|
|Andorra                        |              1|0.800                     |   1800.6|
|Timor-Leste                    |              1|0.800                     |    830.0|
|Aruba                          |              1|0.700                     |   1493.4|
|Gibraltar                      |              1|0.600                     |   1149.9|
|Palau                          |              1|0.600                     |   1770.1|
|Eritrea                        |              1|0.500                     |    258.3|
|Central African Republic       |              1|0.400                     |   1483.0|
|Isle of Man                    |              1|0.300                     |   2235.4|

```r
# restrict data for plotting so we only plot countries with at least 5 universities
all_three_descriptives <- all_three_descriptives %>% 
  filter(n_universities >= 5)
```
=======
|Country              | n universities|n fractional publications | Mean APC|
|:--------------------|--------------:|:-------------------------|--------:|
|China                |            216|460,832.3                 |   2002.6|
|United States        |            199|456,678.0                 |   2099.0|
|United Kingdom       |             60|130,906.7                 |   2042.1|
|Japan                |             55|90,599.2                  |   1920.8|
|Germany              |             54|114,970.8                 |   2014.4|
|South Korea          |             46|79,297.7                  |   1843.7|
|Spain                |             42|84,138.8                  |   1176.2|
|Italy                |             41|77,622.7                  |   1919.2|
|India                |             38|27,611.5                  |   1052.3|
|Iran                 |             36|28,694.7                  |    818.1|
|Australia            |             32|78,220.5                  |   1982.5|
|Brazil               |             31|266,241.3                 |    429.0|
|Poland               |             31|37,813.5                  |   1149.0|
|Turkey               |             31|18,233.7                  |    933.4|
|Canada               |             30|87,827.8                  |   1926.1|
|France               |             28|31,836.3                  |   1963.1|
|Taiwan               |             21|40,111.3                  |   1920.0|
|Netherlands          |             13|42,592.2                  |   2055.0|
|Sweden               |             12|38,855.0                  |   2033.2|
|Austria              |             12|17,341.0                  |   1958.8|
|Russia               |             10|11,144.3                  |    734.9|
|South Africa         |              9|25,886.0                  |   1167.1|
|Switzerland          |              8|29,575.2                  |   2190.5|
|Belgium              |              8|24,577.5                  |   1955.2|
|Israel               |              8|15,475.0                  |   2189.4|
|Egypt                |              8|10,171.3                  |    991.9|
|Greece               |              8|9,671.2                   |   1643.8|
|Finland              |              7|12,476.2                  |   1899.1|
|New Zealand          |              7|9,474.0                   |   1861.0|
|Czech Republic       |              7|8,389.5                   |   1371.1|
|Malaysia             |              6|18,720.5                  |   1271.4|
|Mexico               |              6|17,757.8                  |   1072.4|
|Portugal             |              6|16,621.8                  |   1231.3|
|Norway               |              6|14,952.8                  |   1877.1|
|Thailand             |              6|10,076.0                  |   1673.7|
|Ireland              |              6|7,870.3                   |   1998.1|
|Hungary              |              6|6,309.3                   |   1791.9|
|Denmark              |              5|19,803.7                  |   2008.1|
|Saudi Arabia         |              5|12,935.7                  |   1561.1|
|Pakistan             |              5|4,285.5                   |   1190.0|
|Singapore            |              3|11,055.0                  |   2198.9|
|Colombia             |              3|9,076.3                   |    445.3|
|Chile                |              3|8,320.2                   |   1075.9|
|Argentina            |              3|7,579.8                   |    664.7|
|Serbia               |              3|6,008.7                   |    765.2|
|Romania              |              3|1,835.3                   |   1129.6|
|Tunisia              |              3|1,221.0                   |   1378.1|
|Slovenia             |              2|4,175.7                   |   1133.1|
|Nigeria              |              2|2,140.8                   |   1251.3|
|Slovakia             |              2|1,628.2                   |    918.8|
|Jordan               |              2|1,508.5                   |   1444.6|
|United Arab Emirates |              2|1,500.0                   |   1876.0|
|Croatia              |              1|3,819.3                   |    607.2|
|Uruguay              |              1|1,702.5                   |    690.7|
|Estonia              |              1|1,590.0                   |   1588.3|
|Uganda               |              1|1,561.7                   |   1895.4|
|Lebanon              |              1|1,475.3                   |   1832.5|
|Oman                 |              1|1,250.8                   |    618.6|
|Lithuania            |              1|1,221.0                   |   1351.3|
|Ghana                |              1|1,194.2                   |   1721.3|
|Ethiopia             |              1|1,106.8                   |   1750.5|
|Kuwait               |              1|876.5                     |   1673.2|
|Iceland              |              1|778.3                     |   1698.7|
|Qatar                |              1|650.8                     |   1742.2|
|Luxembourg           |              1|643.8                     |   2149.2|
|Morocco              |              1|538.3                     |    980.9|
|Viet Nam             |              1|463.5                     |   1518.1|
|Cyprus               |              1|383.8                     |   1760.4|
|Algeria              |              1|117.8                     |    838.8|
>>>>>>> main



```r
gdp <- WDI::WDI(start = 2019, end = 2019)

# plot n papers against average apc
p <- all_three_descriptives %>% 
  left_join(wdi, by = c("country_code" = "iso2c", "country" = "country")) %>% 
  ggplot(aes(sum_fractional_works, mean_apc, colour = region, label = country)) +
  geom_point() +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "top") +
  labs(y = NULL, colour = NULL, x = "Sum of fractional publications")
p
```

![](20-APC-analysis_files/figure-html/apc-vs-papers-1.png)<!-- -->


```r
plotly::ggplotly(p)
```

<<<<<<< HEAD
preserve39585f26cf238027
=======
preserve4278abdbc602ce84
>>>>>>> main


```r
pdata <- all_three_descriptives %>% 
  left_join(gdp, by = c("country_code" = "iso2c", "country" = "country")) %>% 
  left_join(wdi, by = c("country_code" = "iso2c", "country" = "country"))

labels <- pdata %>% 
  mutate(label = case_when(
    country %in% c("China", "India", "United States","Uganda",
                      "Brazil", "Switzerland", "Israel", "Spain",
                     "Saudi Arabia") ~ country,
    TRUE ~ ""))

p <- pdata %>% 
  ggplot(aes(NY.GDP.PCAP.KD, mean_apc, colour = region, label = country)) +
  geom_point(aes(alpha = sum_fractional_works)) +
  ggrepel::geom_text_repel(data = labels, aes(label = label),
                           show.legend = FALSE, max.overlaps = Inf,
                           box.padding = 1, min.segment.length = 0,
                           nudge_y = -10) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  scale_alpha_continuous(trans = "log10", range = c(.1, 1),
                         labels = scales::comma) +
  scale_colour_discrete_qualitative(palette = "Dark 3") +
  theme(legend.position = "top", legend.box = "vertical") +
  labs(y = "Mean APC", colour = NULL, x = "GDP per capita", 
       alpha = "Number of fractional publications")
p
```

![](20-APC-analysis_files/figure-html/apc-vs-gdp-1.png)<!-- -->


```r
plotly::ggplotly(p)
```

<<<<<<< HEAD
preservee99965d790746a16
=======
preserve0a6d836724b44147

>>>>>>> main

## Across topics within continents

```r
papers_per_country_per_field <- works %>% 
  mutate(total_weight = work_frac * concept_frac) %>% 
  distinct(country, country_code, id, author_position, institution_id, field,
           total_weight) %>% 
  group_by(country, country_code, field) %>% 
  summarise(sum_fractional_works = sum(total_weight) %>% round(digits = 1)) %>% 
  arrange(desc(sum_fractional_works)) %>% 
  collect()
```

```
## `summarise()` has grouped output by 'country', 'country_code'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'country', 'country_code'. You can override using the `.groups` argument.
```

```r
plot_data <- papers_per_country_per_field %>% 
  left_join(wdi, by = c("country_code" = "iso2c")) %>% 
  group_by(region, field) %>% 
  summarise(sum_fractional_works = sum(sum_fractional_works)) %>% 
  mutate(prop = sum_fractional_works / sum(sum_fractional_works),
         label = case_when(
           prop > .15 ~ scales::percent(prop, accuracy = .1),
           TRUE ~ "")
  )
```

```
## `summarise()` has grouped output by 'region'. You can override using the
## `.groups` argument.
```

```r
plot_data %>% 
<<<<<<< HEAD
=======
  mutate(frac_papers = scales::comma(frac_papers),
         prop = scales::percent(prop, accuracy = .1)) %>% 
  knitr::kable()
```



|field                 |frac_papers |prop  |label           |
|:---------------------|:-----------|:-----|:---------------|
|Medicine              |481,047     |30.6% |481,047 (30.6%) |
|Biology               |290,499     |18.5% |290,499 (18.5%) |
|Chemistry             |163,767     |10.4% |163,767 (10.4%) |
|Computer science      |140,438     |8.9%  |140,438 (8.9%)  |
|Materials science     |106,550     |6.8%  |106,550 (6.8%)  |
|Psychology            |98,461      |6.3%  |98,461 (6.3%)   |
|Environmental science |46,768      |3.0%  |46,768 (3.0%)   |
|Physics               |44,382      |2.8%  |44,382 (2.8%)   |
|Political science     |36,339      |2.3%  |36,339 (2.3%)   |
|Geography             |31,787      |2.0%  |31,787 (2.0%)   |
|Sociology             |29,340      |1.9%  |29,340 (1.9%)   |
|Mathematics           |24,093      |1.5%  |24,093 (1.5%)   |
|Art                   |21,573      |1.4%  |21,573 (1.4%)   |
|Business              |20,992      |1.3%  |20,992 (1.3%)   |
|Geology               |13,937      |0.9%  |13,937 (0.9%)   |
|Philosophy            |10,596      |0.7%  |10,596 (0.7%)   |
|Economics             |5,709       |0.4%  |5,709 (0.4%)    |
|History               |3,379       |0.2%  |3,379 (0.2%)    |
|Engineering           |2,760       |0.2%  |2,760 (0.2%)    |



```r
p_apc_field <- apc_field %>% 
>>>>>>> main
  drop_na() %>% 
  ggplot(aes(prop, fct_reorder(field, prop))) +
  geom_segment(aes(xend = 0, yend = field), colour = "grey70") +
<<<<<<< HEAD
  geom_point() +
  geom_text(aes(label = label), nudge_x = .015, hjust = "left") +
  facet_wrap(vars(region)) +
  scale_x_continuous(expand = expansion(mult = c(0.05, .25)),
                     labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = "% of fractional publications", y = NULL) +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "grey92"),
        axis.text.y = element_text(margin = margin(r = -2)))
=======
  geom_point() + 
  geom_text(aes(label = scales::comma(mean_apc, accuracy = 1)), nudge_x = 30, 
            hjust = "left") +
  labs(y = NULL, x = "Mean APC (in $)") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, .1)),
                     labels = scales::comma)
p_apc_field
```

![](20-APC-analysis_files/figure-html/apc-field-overall-1.png)<!-- -->


# Association between P_top10 and APC

```r
get_mean_apc_by_author_position <- function(df) {
  df %>%
    # first get rid of duplicates from concepts
    distinct(id, author_position, work_frac, APC_in_dollar, University, country,
             publication_year, P_top10) %>% 
    group_by(University, publication_year, country, P_top10) %>%
    # compute the average APC using fractional authorships as weights
    mutate(sum_frac = sum(work_frac)) %>%
    group_by(University, publication_year, country, P_top10, sum_frac,
             author_position) %>%
    summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac,
              fractional_works = sum(work_frac))
}

mean_apcs <- works %>% 
  filter(publication_year == last_year_of_period) %>% 
  get_mean_apc_by_author_position()

mean_apcs_local <- mean_apcs %>%
  collect()
```

```
## `summarise()` has grouped output by 'University', 'publication_year', 'country',
## 'P_top10', 'sum_frac'. You can override using the `.groups` argument.
```

```r
mean_apc_16_19 <- works %>% 
  filter(first_year_of_period == 2016) %>% 
  get_mean_apc_by_author_position()

mean_apc_16_19_local <- mean_apc_16_19 %>% 
  collect()
```

```
## `summarise()` has grouped output by 'University', 'publication_year', 'country',
## 'P_top10', 'sum_frac'. You can override using the `.groups` argument.
```


```r
# plot for 2016-19

# taking out the correlation, because they are incorrect given that the figure
# shows a non-linear relationship (x-axis logged), but the correlation is linear
# (and quite unsuitable to the skewed P_top10)
p1 <- mean_apc_16_19_local %>%
  mutate(author_position = recode(author_position, first = "First authors", 
                                  last = "Last authors")) %>% 
  ggplot(aes(P_top10, mean_apc, colour = fractional_works)) + 
  geom_point(aes(), alpha = .5) +
  scale_colour_continuous_sequential(palette = "Mako", trans = "log10",
                                     labels = comma) +
  geom_smooth(colour = "grey30") +
  facet_wrap(vars(author_position)) +
  scale_x_log10() +
  scale_y_continuous(labels = dollar) +
  labs(caption = "Fractional counting; 2016-2019", y = "Mean APC",
       colour = "Number of papers per institution",
       x = expression(P["top 10%"])) +
  theme(legend.position = "top",
        legend.key.width = unit(1.5, 'cm'))
p1
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/apc-first-last-1.png)<!-- -->


```r
p2 <- mean_apcs_local %>%
  mutate(author_position = recode(author_position, first = "First authors", 
                                  last = "Last authors")) %>% 
  group_by(publication_year, author_position) %>%
  mutate(ptop10_quantiles = cut_quartiles(P_top10)) %>%
  group_by(ptop10_quantiles, publication_year, author_position) %>%
  summarise(mean_apc = weighted.mean(mean_apc, sum_frac, na.rm = TRUE), 
            .groups = "drop_last") %>%
  ggplot(aes(publication_year, mean_apc, colour = ptop10_quantiles,
             group = ptop10_quantiles)) +
  geom_line() +
  facet_wrap(vars(author_position)) +
  scale_x_continuous(breaks = seq(2010, 2018, by = 4)) +
  scale_y_continuous(labels = dollar) +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  theme(legend.position = "top") +
  labs(caption = "Fractional counting", y = "Mean APC",
       colour = expression(P["top 10%"]), x = NULL)
p2
```

![](20-APC-analysis_files/figure-html/apc-first-last-time-1.png)<!-- -->


```r
p1 / p2 +
  plot_layout(heights = c(4.5, 4)) +
  plot_annotation(tag_levels = "A")
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/apc-composite-1.png)<!-- -->


```r
get_mean_apc_by_concept <- function(df) {
  df %>%
    distinct(id, University, publication_year, P_top10, field, work_frac, 
             APC_in_dollar, author_position) %>% 
    group_by(University, publication_year, P_top10, field) %>%
    # spark is unhappy for some reason, so coerce again to numeric
    mutate(work_frac = as.numeric(work_frac)) %>% 
    # compute the average APC using fractional authorships as weights
    mutate(sum_frac = sum(work_frac)) %>%
    group_by(University, publication_year, P_top10, sum_frac,
             author_position, field) %>%
    summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac)
}

mean_apc_concept <- works %>% 
  filter(publication_year == last_year_of_period) %>% 
  get_mean_apc_by_concept()

mean_apc_concept_local <- mean_apc_concept %>%
  collect()
```

```
## `summarise()` has grouped output by 'University', 'publication_year', 'P_top10',
## 'sum_frac', 'author_position'. You can override using the `.groups` argument.
```

```r
mean_apc_concept_16_19 <- works %>% 
  filter(first_year_of_period == 2016) %>% 
  get_mean_apc_by_concept()

mean_apc_concept_16_19_local <- mean_apc_concept_16_19 %>% 
  collect()
```

```
## `summarise()` has grouped output by 'University', 'publication_year', 'P_top10',
## 'sum_frac', 'author_position'. You can override using the `.groups` argument.
```

```r
mean_apc_concept_16_19_local <- mean_apc_concept_16_19_local %>% 
  mutate(author_position = recode(author_position, first = "First authors", 
                                  last = "Last authors"))
```




```r
# plot for 2016-2019
p <- mean_apc_concept_16_19_local %>%
  drop_na(field) %>% 
  ggplot(aes(P_top10, mean_apc, colour = field)) +
  geom_smooth(alpha = .15) +
  facet_wrap(vars(author_position), nrow = 1) +
  scale_x_log10() +
  scale_y_continuous(labels = dollar) + 
  labs(caption = "2016-2019", y = "Mean APC",
       x = expression(P["top 10%"]))
p
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/apc-concepts-1.png)<!-- -->



```r
plotly::ggplotly(p)
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

preserve3f25c3c602a60e2d


Using ggrepel

```r
field_selection <- mean_apc_concept_16_19_local %>%
  drop_na(field) %>% 
  filter(field %in% c("Medicine", "History", "Sociology", "Biology",
                      "Materials science", "Physics"))
  
final_ptop_apc_field <- mean_apc_concept_16_19_local %>% 
  anti_join(field_selection) %>% 
  ggplot(aes(P_top10, mean_apc, group = field)) +
  geom_smooth(alpha = .3, colour = "grey80", fill = "grey90") +
  geom_smooth(aes(colour = field), data = field_selection) +
  facet_wrap(vars(author_position), nrow = 1) +
  scale_x_log10() +
  scale_y_continuous(labels = dollar) + 
  scale_color_discrete_qualitative(palette = "Dark 3") +
  labs(caption = "Time period: 2016-2019", y = "Mean APC", colour = NULL,
       x = expression(P["top 10%"])) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(override.aes = list(alpha = 0),
                               nrow = 1))
```

```
## Joining, by = c("University", "publication_year", "P_top10", "sum_frac",
## "author_position", "field", "mean_apc")
```

```r
final_ptop_apc_field
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/field-selection-1.png)<!-- -->



```r
(final_ptop_apc_field + 
   # https://stackoverflow.com/a/65946462/3149349
   theme(axis.title.y = element_text(margin = margin(r = -120, unit = "pt")))
   ) / p_apc_field +
  plot_layout(heights = c(3.5, 5)) +
  plot_annotation(tag_levels = "A")
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/composite-field-1.png)<!-- -->


## Over time - first-authors

```r
mean_apc_concept_local %>%
  filter(!is.na(field), author_position == "first") %>% 
  group_by(publication_year, field) %>%
  mutate(ptop10_quantiles = cut_quartiles(P_top10)) %>%
  group_by(ptop10_quantiles, publication_year, field) %>%
  summarise(mean_apc = weighted.mean(mean_apc, sum_frac, na.rm = TRUE), 
            .groups = "drop_last") %>%
  ggplot(aes(publication_year, mean_apc, colour = ptop10_quantiles,
             group = ptop10_quantiles)) +
  geom_line() +
  facet_wrap(vars(field)) +
  scale_x_continuous(breaks = seq(2010, 2018, by = 4)) +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  theme(legend.position = "top") +
  labs(caption = "Fractional counting", y = "Mean APC",
       colour = expression(P["top 10%"]), x = NULL)
```

![](20-APC-analysis_files/figure-html/apc-time-concept-first-1.png)<!-- -->

## Over time - last-authors

```r
mean_apc_concept_local %>%
  filter(!is.na(field), author_position == "last") %>% 
  group_by(publication_year, field) %>%
  mutate(ptop10_quantiles = cut_quartiles(P_top10)) %>%
  group_by(ptop10_quantiles, publication_year, field) %>%
  summarise(mean_apc = weighted.mean(mean_apc, sum_frac, na.rm = TRUE), 
            .groups = "drop_last") %>%
  ggplot(aes(publication_year, mean_apc, colour = ptop10_quantiles,
             group = ptop10_quantiles)) +
  geom_line() +
  facet_wrap(vars(field)) +
  scale_x_continuous(breaks = seq(2010, 2018, by = 4)) +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  theme(legend.position = "top") +
  labs(caption = "Fractional counting", y = "Mean APC",
       colour = expression(P["top 10%"]), x = NULL)
```

![](20-APC-analysis_files/figure-html/apc-time-concept-last-1.png)<!-- -->

# Country comparison

```r
mean_apc_country_16_19 <- works %>%
  filter(first_year_of_period == 2016) %>% 
  # first get rid of duplicates from concepts
  distinct(id, work_frac, APC_in_dollar, University, country, P_top10, country_code) %>% 
  # spark is unhappy for some reason, so coerce again to numeric
  mutate(work_frac = as.numeric(work_frac)) %>% 
  group_by(University, country, P_top10) %>%
  # compute the average APC using fractional authorships as weights
  mutate(sum_frac = sum(work_frac)) %>%
  group_by(University, country, P_top10, sum_frac, country_code) %>%
  summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac)

mean_apc_country_16_19_local <- mean_apc_country_16_19 %>%
  collect()
```

```
## `summarise()` has grouped output by 'University', 'country', 'P_top10',
## 'sum_frac'. You can override using the `.groups` argument.
```


```r
mean_apc_country_16_19_local <- mean_apc_country_16_19_local %>% 
  left_join(wdi, by = c("country_code" = "iso2c"))
```


```r
mean_apc_country_16_19_local %>% 
  ggplot(aes(P_top10, mean_apc, colour = region)) +
  geom_point(alpha = .3, size = 1.2) +
  geom_smooth(alpha = .3) +
  scale_x_log10() +
  scale_y_continuous(labels = dollar) +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  labs(y = "Mean APC", x = expression(P["top 10%"]), colour = NULL) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(override.aes = list(alpha = 0)))
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
>>>>>>> main
```

![](20-APC-analysis_files/figure-html/topic-share-across-continents-1.png)<!-- -->



```r
spark_disconnect(sc)
```

