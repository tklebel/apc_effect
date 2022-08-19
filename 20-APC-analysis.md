---
title: "Relationship between OA publishing, APCs and IF"
author: "Thomas Klebel"
date: "19 August, 2022"
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

preserve39585f26cf238027


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
  scale_y_continuous(labels = scales::comma) +
  scale_alpha_continuous(trans = "log10", range = c(.1, 1),
                         labels = scales::comma) +
  scale_colour_discrete_qualitative(palette = "Dark 3") +
  theme(legend.position = "top", legend.box = "vertical") +
  labs(y = "Average APC", colour = NULL, x = "GDP per capita", 
       alpha = "Number of fractional publications")
p
```

![](20-APC-analysis_files/figure-html/apc-vs-gdp-1.png)<!-- -->


```r
plotly::ggplotly(p)
```

preservee99965d790746a16

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
  drop_na() %>% 
  ggplot(aes(prop, fct_reorder(field, prop))) +
  geom_segment(aes(xend = 0, yend = field), colour = "grey70") +
  geom_point() +
  geom_text(aes(label = label), nudge_x = .015, hjust = "left") +
  facet_wrap(vars(region)) +
  scale_x_continuous(expand = expansion(mult = c(0.05, .25)),
                     labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = "% of fractional publications", y = NULL) +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "grey92"),
        axis.text.y = element_text(margin = margin(r = -2)))
```

![](20-APC-analysis_files/figure-html/topic-share-across-continents-1.png)<!-- -->



```r
spark_disconnect(sc)
```

