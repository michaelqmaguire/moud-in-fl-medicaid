MOUD in Florida Medicaid Population
================
Michael Q. Maguire, MS
February 18th, 2022

## Packages Used

``` r
library(tidyverse)
library(data.table)
library(rvest)
library(hrbrthemes)
```

## Read in the SDUD data set

``` r
sdud <- fread(
  file_location,
  colClasses = c('proper_ndc' = 'character')
)
```

## Subset to Florida Only and Create MOUD Flags

Variable `moudFlag` represents whether the brand name has a pattern
match with the below values. Variable `medication` represents the
categorization according to AG.

``` r
sdudFL <- sdud[
  i = (state == 'FL' & data.table::between(year, 2016, 2021)),
  j = .(utilization.type, state, year, quarter, suppression, gennme, prodnme, numberrx)
][
  , 
  `:=` (moudFlag = ifelse(grepl(x = prodnme, pattern = "bupreno|suboxone|zubsolv|cassipa|bunavail|subutex|sublocade|subutex|probuphine", ignore.case = TRUE) == TRUE, 1, 0),
        medication = case_when(
          grepl(x = prodnme, pattern = 'bunavail', ignore.case = TRUE) == TRUE ~ "Bunavail",
          grepl(x = prodnme, pattern = 'subutex', ignore.case = TRUE) == TRUE ~ 'Subutex',
          grepl(x = prodnme, pattern = 'suboxone', ignore.case = TRUE) == TRUE ~ 'Suboxone',
          grepl(x = prodnme, pattern = 'cassipa', ignore.case = TRUE) == TRUE ~ 'Cassipa',
          grepl(x = prodnme, pattern = 'zubsolv', ignore.case = TRUE) == TRUE ~ 'Zubsolv',
          grepl(x = prodnme, pattern = 'buprenorphine', ignore.case = TRUE) == TRUE ~ 'Buprenorphine',
          grepl(x = prodnme, pattern = 'sublocade', ignore.case = TRUE) == TRUE ~ 'Sublocade',
          # grepl(x = prodnme, pattern = 'subutex', ignore.case = TRUE) == TRUE ~ 'Subutex',
          # grepl(x = prodnme, pattern = 'probuphine', ignore.case = TRUE) == TRUE ~ 'Probuphine',
          TRUE ~ 'Not an OUD Drug'
          ),
        yearQuarter = zoo::as.yearqtr(format(paste0(year, quarter)), "%Y%q")
        )
]

setorder(sdudFL, state, yearQuarter, gennme, prodnme, suppression)

sdudFL |> janitor::tabyl(moudFlag)
```

    ##  moudFlag      n     percent
    ##         0 643200 0.998185822
    ##         1   1169 0.001814178

``` r
sdudFL |> janitor::tabyl(medication)
```

    ##       medication      n      percent
    ##         Bunavail     26 4.034955e-05
    ##    Buprenorphine    826 1.281874e-03
    ##  Not an OUD Drug 643200 9.981858e-01
    ##        Sublocade     26 4.034955e-05
    ##         Suboxone    162 2.514087e-04
    ##          Zubsolv    129 2.001959e-04

``` r
sdudFL |> janitor::tabyl(medication, moudFlag)
```

    ##       medication      0   1
    ##         Bunavail      0  26
    ##    Buprenorphine      0 826
    ##  Not an OUD Drug 643200   0
    ##        Sublocade      0  26
    ##         Suboxone      0 162
    ##          Zubsolv      0 129

## Subset to only OUD medications and aggregate.

``` r
sdudFLAggregate <- sdudFL[
  i = moudFlag == 1 & suppression == 'F',
  j = .(totalRX = sum(numberrx)),
  by = .(yearQuarter, medication)
]

setorder(sdudFLAggregate, medication, yearQuarter)

sdudFLAggregate
```

    ##     yearQuarter    medication totalRX
    ##  1:     2016 Q1      Bunavail      15
    ##  2:     2016 Q3      Bunavail      16
    ##  3:     2016 Q4      Bunavail      29
    ##  4:     2017 Q1      Bunavail      30
    ##  5:     2017 Q2      Bunavail      19
    ##  6:     2017 Q3      Bunavail      13
    ##  7:     2017 Q4      Bunavail      17
    ##  8:     2018 Q2      Bunavail      14
    ##  9:     2016 Q1 Buprenorphine    1294
    ## 10:     2016 Q2 Buprenorphine    2198
    ## 11:     2016 Q3 Buprenorphine    1961
    ## 12:     2016 Q4 Buprenorphine    2106
    ## 13:     2017 Q1 Buprenorphine    1868
    ## 14:     2017 Q2 Buprenorphine    2354
    ## 15:     2017 Q3 Buprenorphine    2287
    ## 16:     2017 Q4 Buprenorphine    2153
    ## 17:     2018 Q1 Buprenorphine    2067
    ## 18:     2018 Q2 Buprenorphine    2594
    ## 19:     2018 Q3 Buprenorphine    2974
    ## 20:     2018 Q4 Buprenorphine    2938
    ## 21:     2019 Q1 Buprenorphine    4277
    ## 22:     2019 Q2 Buprenorphine    6185
    ## 23:     2019 Q3 Buprenorphine    6187
    ## 24:     2019 Q4 Buprenorphine    6530
    ## 25:     2020 Q1 Buprenorphine    5963
    ## 26:     2020 Q2 Buprenorphine    5727
    ## 27:     2020 Q3 Buprenorphine    6212
    ## 28:     2020 Q4 Buprenorphine    7000
    ## 29:     2021 Q1 Buprenorphine    7455
    ## 30:     2018 Q4     Sublocade      20
    ## 31:     2019 Q1     Sublocade      16
    ## 32:     2019 Q2     Sublocade      20
    ## 33:     2019 Q3     Sublocade      15
    ## 34:     2019 Q4     Sublocade      14
    ## 35:     2020 Q1     Sublocade      15
    ## 36:     2020 Q2     Sublocade      38
    ## 37:     2020 Q3     Sublocade      32
    ## 38:     2020 Q4     Sublocade      34
    ## 39:     2021 Q1     Sublocade      52
    ## 40:     2016 Q1      Suboxone    3870
    ## 41:     2016 Q2      Suboxone    4322
    ## 42:     2016 Q3      Suboxone    4173
    ## 43:     2016 Q4      Suboxone    4902
    ## 44:     2017 Q1      Suboxone    4609
    ## 45:     2017 Q2      Suboxone    5738
    ## 46:     2017 Q3      Suboxone    5443
    ## 47:     2017 Q4      Suboxone    5451
    ## 48:     2018 Q1      Suboxone    5419
    ## 49:     2018 Q2      Suboxone    6138
    ## 50:     2018 Q3      Suboxone    6250
    ## 51:     2018 Q4      Suboxone    6681
    ## 52:     2019 Q1      Suboxone    5942
    ## 53:     2019 Q2      Suboxone    4654
    ## 54:     2019 Q3      Suboxone    4759
    ## 55:     2019 Q4      Suboxone    4555
    ## 56:     2020 Q1      Suboxone    4990
    ## 57:     2020 Q2      Suboxone    6265
    ## 58:     2020 Q3      Suboxone    6690
    ## 59:     2020 Q4      Suboxone    7200
    ## 60:     2021 Q1      Suboxone    7553
    ## 61:     2016 Q1       Zubsolv     185
    ## 62:     2016 Q2       Zubsolv     240
    ## 63:     2016 Q3       Zubsolv     233
    ## 64:     2016 Q4       Zubsolv     281
    ## 65:     2017 Q1       Zubsolv     257
    ## 66:     2017 Q2       Zubsolv     288
    ## 67:     2017 Q3       Zubsolv     191
    ## 68:     2017 Q4       Zubsolv     191
    ## 69:     2018 Q1       Zubsolv     133
    ## 70:     2018 Q2       Zubsolv     143
    ## 71:     2018 Q3       Zubsolv     142
    ## 72:     2018 Q4       Zubsolv     137
    ## 73:     2019 Q1       Zubsolv     172
    ## 74:     2019 Q2       Zubsolv     260
    ## 75:     2019 Q3       Zubsolv     254
    ## 76:     2019 Q4       Zubsolv     232
    ## 77:     2020 Q1       Zubsolv     236
    ## 78:     2020 Q2       Zubsolv     216
    ## 79:     2020 Q3       Zubsolv     197
    ## 80:     2020 Q4       Zubsolv     208
    ## 81:     2021 Q1       Zubsolv     193
    ##     yearQuarter    medication totalRX

## Create table containing all years, quarters, and drugs.

``` r
yearQuarter <- unique(sdudFL$yearQuarter, by = yearQuarter)

allDrugs <- unique(sdudFLAggregate$medication, by = medication)

cartesianYearQuarterDrug <- CJ(yearQuarter, allDrugs)

cartesianYearQuarterDrug
```

    ##      yearQuarter      allDrugs
    ##   1:     2016 Q1      Bunavail
    ##   2:     2016 Q1 Buprenorphine
    ##   3:     2016 Q1     Sublocade
    ##   4:     2016 Q1      Suboxone
    ##   5:     2016 Q1       Zubsolv
    ##  ---                          
    ## 101:     2021 Q1      Bunavail
    ## 102:     2021 Q1 Buprenorphine
    ## 103:     2021 Q1     Sublocade
    ## 104:     2021 Q1      Suboxone
    ## 105:     2021 Q1       Zubsolv

## Full Join with the above table with the `sdudFLAggregate` to get values that don’t exist for a given yearQuarter.

``` r
yearQuarterAllFLAggregate <- merge.data.table(
  cartesianYearQuarterDrug,
  sdudFLAggregate,
  by.x = c('yearQuarter', 'allDrugs'),
  by.y = c('yearQuarter', 'medication'),
  all.x = TRUE
)

sum(is.na(yearQuarterAllFLAggregate$totalRX))
```

    ## [1] 24

``` r
yearQuarterAllFLAggregate[is.na(totalRX), totalRX := 0]

yearQuarterAllFLAggregate
```

    ##      yearQuarter      allDrugs totalRX
    ##   1:     2016 Q1      Bunavail      15
    ##   2:     2016 Q1 Buprenorphine    1294
    ##   3:     2016 Q1     Sublocade       0
    ##   4:     2016 Q1      Suboxone    3870
    ##   5:     2016 Q1       Zubsolv     185
    ##  ---                                  
    ## 101:     2021 Q1      Bunavail       0
    ## 102:     2021 Q1 Buprenorphine    7455
    ## 103:     2021 Q1     Sublocade      52
    ## 104:     2021 Q1      Suboxone    7553
    ## 105:     2021 Q1       Zubsolv     193

``` r
sum(is.na(yearQuarterAllFLAggregate$totalRX))
```

    ## [1] 0

``` r
write_csv(yearQuarterAllFLAggregate, file = paste0('./data/clean/', format(Sys.Date(), '%Y%m%d'), "_fl-prescriptions-aggregate.csv"), na = '')
```

# Plotting time!

``` r
plotRawDrugCount <- yearQuarterAllFLAggregate |>
  ggplot() +
  geom_point(aes(x = as.factor(yearQuarter), y = totalRX, color = allDrugs), size = 2) +
  geom_line(aes(x = as.factor(yearQuarter), y = totalRX, group = allDrugs, color = allDrugs), size = 1) +
  scale_color_manual(values = c("Bunavail" = "grey", "Buprenorphine" = "#FA841A", "Suboxone" = "#004DA9", "Zubsolv" = "chartreuse4", "Sublocade" = 'black')) +
  ggtitle("Florida MOUD Raw Prescription Drug Counts") +
  xlab("Year-Quarter") +
  ylab("Total Prescriptions") +
  theme_ipsum_rc() + 
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14, hjust = 0.5, vjust = -1),
    axis.title.y = element_text(size = 14, hjust = 0.5),
    legend.position = 'bottom',
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.key.size = unit(3, 'line')
  ) +
  labs(color = 'Drug Name')

plotRawDrugCount
```

![](02_moud-in-fl-medicaid_files/figure-gfm/plottingOUD-1.png)<!-- -->

## Iteratively download files from Medicaid

``` r
achaURL <- 'https://ahca.myflorida.com/medicaid/Finance/data_analytics/enrollment_report/' # url itself
achaRead <- read_html(achaURL) # read in the HTML
achaFiles <- achaRead |> html_nodes('a') |> html_attr('href') |> str_subset('ENR') # extract needed elements
achaTrimmed <- achaFiles |> str_extract(pattern = "docs.*") # extract anything after docs
achaYears <- str_subset(achaTrimmed, pattern = "2016|2017|2018|2019|2020|2021") # extract files with desired years
achaNames <- str_extract(achaYears, pattern = "ENR_.*") # get names for naming file on download.file
achaFilesDLURL <- paste0(achaURL, achaYears) # paste url and name together for download.file

# Iteration occurs here

map2(
  achaFilesDLURL,
  achaNames,
  ~download.file(.x, destfile = paste0("./data/raw/", .y), mode = 'wb')
)

# Renaming two pesky files.

file.rename("./data/raw/ENR_February_2016.xls", "./data/raw/ENR_201602.xls")
file.rename("./data/raw/ENR_January_2016.xls", "./data/raw/ENR_201601.xls")
```

## Showing files in directory

``` r
files <- list.files(path = './data/raw/', full.names = TRUE)
files
```

    ##  [1] "./data/raw/ENR_201601.xls" "./data/raw/ENR_201602.xls"
    ##  [3] "./data/raw/ENR_201603.xls" "./data/raw/ENR_201604.xls"
    ##  [5] "./data/raw/ENR_201605.xls" "./data/raw/ENR_201606.xls"
    ##  [7] "./data/raw/ENR_201607.xls" "./data/raw/ENR_201608.xls"
    ##  [9] "./data/raw/ENR_201609.xls" "./data/raw/ENR_201610.xls"
    ## [11] "./data/raw/ENR_201611.xls" "./data/raw/ENR_201612.xls"
    ## [13] "./data/raw/ENR_201701.xls" "./data/raw/ENR_201702.xls"
    ## [15] "./data/raw/ENR_201703.xls" "./data/raw/ENR_201704.xls"
    ## [17] "./data/raw/ENR_201705.xls" "./data/raw/ENR_201706.xls"
    ## [19] "./data/raw/ENR_201707.xls" "./data/raw/ENR_201708.xls"
    ## [21] "./data/raw/ENR_201709.xls" "./data/raw/ENR_201710.xls"
    ## [23] "./data/raw/ENR_201711.xls" "./data/raw/ENR_201712.xls"
    ## [25] "./data/raw/ENR_201801.xls" "./data/raw/ENR_201802.xls"
    ## [27] "./data/raw/ENR_201803.xls" "./data/raw/ENR_201804.xls"
    ## [29] "./data/raw/ENR_201805.xls" "./data/raw/ENR_201806.xls"
    ## [31] "./data/raw/ENR_201807.xls" "./data/raw/ENR_201808.xls"
    ## [33] "./data/raw/ENR_201809.xls" "./data/raw/ENR_201810.xls"
    ## [35] "./data/raw/ENR_201811.xls" "./data/raw/ENR_201812.xls"
    ## [37] "./data/raw/ENR_201901.xls" "./data/raw/ENR_201902.xls"
    ## [39] "./data/raw/ENR_201903.xls" "./data/raw/ENR_201904.xls"
    ## [41] "./data/raw/ENR_201905.xls" "./data/raw/ENR_201906.xls"
    ## [43] "./data/raw/ENR_201907.xls" "./data/raw/ENR_201908.xls"
    ## [45] "./data/raw/ENR_201909.xls" "./data/raw/ENR_201910.xls"
    ## [47] "./data/raw/ENR_201911.xls" "./data/raw/ENR_201912.xls"
    ## [49] "./data/raw/ENR_202001.xls" "./data/raw/ENR_202002.xls"
    ## [51] "./data/raw/ENR_202003.xls" "./data/raw/ENR_202004.xls"
    ## [53] "./data/raw/ENR_202005.xls" "./data/raw/ENR_202006.xls"
    ## [55] "./data/raw/ENR_202007.xls" "./data/raw/ENR_202008.xls"
    ## [57] "./data/raw/ENR_202009.xls" "./data/raw/ENR_202010.xls"
    ## [59] "./data/raw/ENR_202011.xls" "./data/raw/ENR_202012.xls"
    ## [61] "./data/raw/ENR_202101.xls" "./data/raw/ENR_202102.xls"
    ## [63] "./data/raw/ENR_202103.xls" "./data/raw/ENR_202104.xls"
    ## [65] "./data/raw/ENR_202105.xls" "./data/raw/ENR_202106.xls"
    ## [67] "./data/raw/ENR_202107.xls" "./data/raw/ENR_202108.xls"
    ## [69] "./data/raw/ENR_202109.xls" "./data/raw/ENR_202110.xls"
    ## [71] "./data/raw/ENR_202111.xls" "./data/raw/ENR_202112.xls"

## Iteratively determine how many columns exist in each file

``` r
numColumnsflMedicaid2016to2021 <- 
  files |>
  map_dfr(
    ~(data.frame
      (number_columns = ncol(
        readxl::read_excel(
          path = .,
          skip = 6,
          sheet = 'AGEGROUP'
        )
      ),
      id = .)
    )
  )

numColumnsflMedicaid2016to2021
```

    ##    number_columns                        id
    ## 1              18 ./data/raw/ENR_201601.xls
    ## 2              20 ./data/raw/ENR_201602.xls
    ## 3              20 ./data/raw/ENR_201603.xls
    ## 4              20 ./data/raw/ENR_201604.xls
    ## 5              20 ./data/raw/ENR_201605.xls
    ## 6              20 ./data/raw/ENR_201606.xls
    ## 7              20 ./data/raw/ENR_201607.xls
    ## 8              20 ./data/raw/ENR_201608.xls
    ## 9              20 ./data/raw/ENR_201609.xls
    ## 10             20 ./data/raw/ENR_201610.xls
    ## 11             20 ./data/raw/ENR_201611.xls
    ## 12             20 ./data/raw/ENR_201612.xls
    ## 13             20 ./data/raw/ENR_201701.xls
    ## 14             20 ./data/raw/ENR_201702.xls
    ## 15             20 ./data/raw/ENR_201703.xls
    ## 16             20 ./data/raw/ENR_201704.xls
    ## 17             20 ./data/raw/ENR_201705.xls
    ## 18             20 ./data/raw/ENR_201706.xls
    ## 19             20 ./data/raw/ENR_201707.xls
    ## 20             20 ./data/raw/ENR_201708.xls
    ## 21             20 ./data/raw/ENR_201709.xls
    ## 22             20 ./data/raw/ENR_201710.xls
    ## 23             20 ./data/raw/ENR_201711.xls
    ## 24             20 ./data/raw/ENR_201712.xls
    ## 25             20 ./data/raw/ENR_201801.xls
    ## 26             20 ./data/raw/ENR_201802.xls
    ## 27             20 ./data/raw/ENR_201803.xls
    ## 28             20 ./data/raw/ENR_201804.xls
    ## 29             20 ./data/raw/ENR_201805.xls
    ## 30             20 ./data/raw/ENR_201806.xls
    ## 31             20 ./data/raw/ENR_201807.xls
    ## 32             20 ./data/raw/ENR_201808.xls
    ## 33             20 ./data/raw/ENR_201809.xls
    ## 34             20 ./data/raw/ENR_201810.xls
    ## 35             20 ./data/raw/ENR_201811.xls
    ## 36             20 ./data/raw/ENR_201812.xls
    ## 37             20 ./data/raw/ENR_201901.xls
    ## 38             20 ./data/raw/ENR_201902.xls
    ## 39             20 ./data/raw/ENR_201903.xls
    ## 40             20 ./data/raw/ENR_201904.xls
    ## 41             20 ./data/raw/ENR_201905.xls
    ## 42             20 ./data/raw/ENR_201906.xls
    ## 43             20 ./data/raw/ENR_201907.xls
    ## 44             20 ./data/raw/ENR_201908.xls
    ## 45             20 ./data/raw/ENR_201909.xls
    ## 46             20 ./data/raw/ENR_201910.xls
    ## 47             20 ./data/raw/ENR_201911.xls
    ## 48             20 ./data/raw/ENR_201912.xls
    ## 49             20 ./data/raw/ENR_202001.xls
    ## 50             20 ./data/raw/ENR_202002.xls
    ## 51             20 ./data/raw/ENR_202003.xls
    ## 52             20 ./data/raw/ENR_202004.xls
    ## 53             20 ./data/raw/ENR_202005.xls
    ## 54             20 ./data/raw/ENR_202006.xls
    ## 55             20 ./data/raw/ENR_202007.xls
    ## 56             20 ./data/raw/ENR_202008.xls
    ## 57             20 ./data/raw/ENR_202009.xls
    ## 58             20 ./data/raw/ENR_202010.xls
    ## 59             20 ./data/raw/ENR_202011.xls
    ## 60             20 ./data/raw/ENR_202012.xls
    ## 61             20 ./data/raw/ENR_202101.xls
    ## 62             20 ./data/raw/ENR_202102.xls
    ## 63             20 ./data/raw/ENR_202103.xls
    ## 64             20 ./data/raw/ENR_202104.xls
    ## 65             20 ./data/raw/ENR_202105.xls
    ## 66             20 ./data/raw/ENR_202106.xls
    ## 67             20 ./data/raw/ENR_202107.xls
    ## 68             20 ./data/raw/ENR_202108.xls
    ## 69             20 ./data/raw/ENR_202109.xls
    ## 70             20 ./data/raw/ENR_202110.xls
    ## 71             24 ./data/raw/ENR_202111.xls
    ## 72             20 ./data/raw/ENR_202112.xls

``` r
numColumnsflMedicaid2016to2021 |>
  distinct(number_columns, .keep_all = TRUE)
```

    ##   number_columns                        id
    ## 1             18 ./data/raw/ENR_201601.xls
    ## 2             20 ./data/raw/ENR_201602.xls
    ## 3             24 ./data/raw/ENR_202111.xls

We can see above that the files do not have the same lengths across the
`AGEGROUP` sheet.

I looked at the files that had different lengths and determined how to
accommodate the code to each sheet.

I also found that the row value of `TOTAL OF MEDICAID` contained the
numbers we needed.

``` r
namesAcha18 <- c(
  'plan_name', 'type', 'region', 'blank', 'tanf_children_0_18', 'tanf_children_19_20', 'tanf_adult_21_over',
  'tanf_total', 'blank2', 'medicaid_only_0_18', 'medicaid_only_19_20', 'medicaid_21_over', 'duals_0_18', 'duals_19_20',
  'duals_21_over', 'medicaid_duals_total', 'blank3', 'children_percent'
)

namesAcha20 <- c(
  'plan_name', 'type', 'region', 'blank', 'tanf_children_0_18', 'tanf_children_19_20', 'tanf_adult_21_over',
  'tanf_total', 'blank2', 'medicaid_only_0_18', 'medicaid_only_19_20', 'medicaid_21_over', 'duals_0_18', 'duals_19_20',
  'duals_21_over', 'medicaid_duals_total', 'blank3' , 'tanf_medicaid_duals_total', 'blank4', 'children_percent'
)
```

## Extracting information from ACHA files

Next, I extracted the file paths containing 20 columns, read them in,
combined them, and selected the only row we needed.

``` r
colsGE20 <- numColumnsflMedicaid2016to2021$id[numColumnsflMedicaid2016to2021$number_columns == 20]

# Extract all 20 column files.

acha20 <- setNames(
  lapply(
    colsGE20,
    FUN = function(x) readxl::read_excel(
      path = x,
      sheet = 'AGEGROUP',
      skip = 6,
      col_names = namesAcha20,
      col_types = 'text'
      )
    ),
  colsGE20
  )

# Combine, and keep only necessary row. Extract date information from filename.

flMedicaidGE20 <- as_tibble(
  data.table::rbindlist(
    lapply(
      acha20,
      subset,
      plan_name == 'TOTAL OF MEDICAID'
    ),
    idcol = 'id'
  )
) |>
  select(
    plan_name, id, contains('21')
  ) |>
  mutate(
    idDate = str_extract(string = id, pattern = '[:digit:]+'),
    year = str_sub(idDate, 1, 4),
    month = str_sub(idDate, 5, 6)
  )
```

Next, I handled those files with differing column lengths.

The file that had 18 columns omitted certain columns that were in future
reports, while the file with 24 columns read in columns that didn’t
exist.

``` r
cols_24 <- numColumnsflMedicaid2016to2021 |> filter(number_columns == 24) |>
  select(id) |>
  as_vector()

acha_24 <- readxl::read_excel(path = cols_24, sheet = 'AGEGROUP', skip = 6, col_names = FALSE, col_types = 'text') |>
  select(-c(`...21`, `...22`, `...23`, `...24`)) 

names(acha_24) <- namesAcha20

acha_24_final <- acha_24 |>
  filter(plan_name == 'TOTAL OF MEDICAID') |>
  select(plan_name, contains('21')) |>
  mutate(
    id = cols_24,
    idDate = str_extract(string = cols_24, pattern = '[:digit:]+'),
    year = str_sub(idDate, 1, 4),
    month = str_sub(idDate, 5, 6)
  )

acha_24_final
```

    ## # A tibble: 1 x 8
    ##   plan_name   tanf_adult_21_o~ medicaid_21_over duals_21_over id    idDate year 
    ##   <chr>       <chr>            <chr>            <chr>         <chr> <chr>  <chr>
    ## 1 TOTAL OF M~ 1001003          383674           954584        ./da~ 202111 2021 
    ## # ... with 1 more variable: month <chr>

``` r
cols_18 <- numColumnsflMedicaid2016to2021 |> filter(number_columns == 18) |>
  select(id) |>
  as_vector()

acha_18_final <- readxl::read_excel(path = cols_18, sheet = 'AGEGROUP', skip = 6, col_names = namesAcha18, col_types = 'text') |>
  filter(plan_name == 'TOTAL OF MEDICAID') |>
  select(plan_name, contains('21')) |>
  mutate(
    id = cols_18,
    idDate = str_extract(string = cols_18, pattern = '[:digit:]+'),
    year = str_sub(idDate, 1, 4),
    month = str_sub(idDate, 5, 6)
  )

acha_18_final
```

    ## # A tibble: 1 x 8
    ##   plan_name   tanf_adult_21_o~ medicaid_21_over duals_21_over id    idDate year 
    ##   <chr>       <chr>            <chr>            <chr>         <chr> <chr>  <chr>
    ## 1 TOTAL OF M~ 618223           242625           756196        ./da~ 201601 2016 
    ## # ... with 1 more variable: month <chr>

Finally, I combined them all into a single file and created date fields
for aggregation/plotting.

``` r
flMedicaid2016to2021 <- bind_rows(
  flMedicaidGE20,
  acha_18_final,
  acha_24_final
) |>
  mutate(
    across(.cols = tanf_adult_21_over:duals_21_over, as.numeric),
    yearNum = zoo::as.yearmon(idDate, format = "%Y%m"),
    yearQtr = zoo::as.yearqtr(yearNum, format = "%Y%m")
  ) |> 
  arrange(yearNum)

flMedicaid2016to2021
```

    ## # A tibble: 72 x 10
    ##    plan_name  id    tanf_adult_21_o~ medicaid_21_over duals_21_over idDate year 
    ##    <chr>      <chr>            <dbl>            <dbl>         <dbl> <chr>  <chr>
    ##  1 TOTAL OF ~ ./da~           618223           242625        756196 201601 2016 
    ##  2 TOTAL OF ~ ./da~           625545           249567        758347 201602 2016 
    ##  3 TOTAL OF ~ ./da~           620401           249509        758279 201603 2016 
    ##  4 TOTAL OF ~ ./da~           616907           246055        759099 201604 2016 
    ##  5 TOTAL OF ~ ./da~           619192           251946        764651 201605 2016 
    ##  6 TOTAL OF ~ ./da~           612287           250391        765373 201606 2016 
    ##  7 TOTAL OF ~ ./da~           612672           247332        767279 201607 2016 
    ##  8 TOTAL OF ~ ./da~           618827           252607        770966 201608 2016 
    ##  9 TOTAL OF ~ ./da~           614316           249152        771034 201609 2016 
    ## 10 TOTAL OF ~ ./da~           607557           247551        771546 201610 2016 
    ## # ... with 62 more rows, and 3 more variables: month <chr>, yearNum <yearmon>,
    ## #   yearQtr <yearqtr>

# Start aggregating the ACHA Medicaid Population information

``` r
aggByMonth <- flMedicaid2016to2021 |>
  group_by(yearNum) |>
  mutate(
    total = sum(across(.cols = tanf_adult_21_over:duals_21_over))
  ) |>
  distinct(yearNum, .keep_all = TRUE)

aggByMonthDenom <- aggByMonth |>
  select(yearNum, tanf_adult_21_over:duals_21_over, total)

aggByMonthDenom
```

    ## # A tibble: 72 x 5
    ## # Groups:   yearNum [72]
    ##    yearNum   tanf_adult_21_over medicaid_21_over duals_21_over   total
    ##    <yearmon>              <dbl>            <dbl>         <dbl>   <dbl>
    ##  1 Jan 2016              618223           242625        756196 1617044
    ##  2 Feb 2016              625545           249567        758347 1633459
    ##  3 Mar 2016              620401           249509        758279 1628189
    ##  4 Apr 2016              616907           246055        759099 1622061
    ##  5 May 2016              619192           251946        764651 1635789
    ##  6 Jun 2016              612287           250391        765373 1628051
    ##  7 Jul 2016              612672           247332        767279 1627283
    ##  8 Aug 2016              618827           252607        770966 1642400
    ##  9 Sep 2016              614316           249152        771034 1634502
    ## 10 Oct 2016              607557           247551        771546 1626654
    ## # ... with 62 more rows

``` r
aggByYearQtr <- flMedicaid2016to2021 |>
  group_by(yearQtr) |>
  mutate(
    total = sum(across(.cols = tanf_adult_21_over:duals_21_over)) / 3
  ) |>
  distinct(yearQtr, .keep_all = TRUE)

aggByYearQtrDenom <- aggByYearQtr |>
  select(yearQtr, total)

aggByYearQtrDenom
```

    ## # A tibble: 24 x 2
    ## # Groups:   yearQtr [24]
    ##    yearQtr      total
    ##    <yearqtr>    <dbl>
    ##  1 2016 Q1   1626231.
    ##  2 2016 Q2   1628634.
    ##  3 2016 Q3   1634728.
    ##  4 2016 Q4   1631855.
    ##  5 2017 Q1   1637312 
    ##  6 2017 Q2   1628608 
    ##  7 2017 Q3   1613870 
    ##  8 2017 Q4   1611012 
    ##  9 2018 Q1   1598327.
    ## 10 2018 Q2   1582947.
    ## # ... with 14 more rows

``` r
write_csv(aggByMonthDenom, file = paste0('./data/clean/', format(Sys.Date(), '%Y%m%d'), "_fl-monthly-enrollment-denominator.csv"), na = '')
write_csv(aggByYearQtrDenom, file = paste0('./data/clean/', format(Sys.Date(), '%Y%m%d'), "_fl-quarterly-enrollment-denominator.csv"), na = '')
```

## Plotting the aggregated ACHA Medicaid Population information

First, aggregate by year and month.

``` r
plotAggByMonth <- aggByMonthDenom |>
  ggplot() +
  geom_col(aes(x = as.factor(yearNum), y = total, fill = as.factor(yearNum)), color = 'black') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_viridis_d() +
  ggtitle("Florida Adult (21+) Medicaid Population by Year and Month") +
  xlab("Month-Year") +
  ylab("Number of Enrollees Per Month") +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    axis.text.x = element_text(size = 14, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14, hjust = 0.5, vjust = -1),
    axis.title.y = element_text(size = 14, hjust = 0.5),
    legend.position = 'none'
  ) +
  labs(
    caption = 'Source: Florida Agency for Healthcare Administration \n
               Download URL: https://ahca.myflorida.com/medicaid/Finance/data_analytics/enrollment_report/'
  )

plotAggByMonth
```

![](02_moud-in-fl-medicaid_files/figure-gfm/plotAchaAggYearMonth-1.png)<!-- -->

Next, aggregate by year and quarter.

This calculation is done by summing enrollees across three months and
then dividing by three.

``` r
plotAggByYearQtr <- aggByYearQtrDenom |>
  ggplot() +
  geom_col(aes(x = as.factor(yearQtr), y = total, fill = as.factor(yearQtr)), color = 'black') +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Florida Adult (21+) Medicaid Population by Year and Quarter") +
  xlab("Year-Quarter") +
  ylab("Number of Enrollees per Quarter") +
  scale_fill_viridis_d() +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14, hjust = 0.5, vjust = -1),
    axis.title.y = element_text(size = 14, hjust = 0.5),
    legend.position = 'none'
  ) +
  labs(
    caption = 'Source: Florida Agency for Healthcare Administration \n
               Download URL: https://ahca.myflorida.com/medicaid/Finance/data_analytics/enrollment_report/ \n 
               Quarterly Population Calculation = (Month 1 + Month 2 + Month 3) / 3'
  )

plotAggByYearQtr
```

![](02_moud-in-fl-medicaid_files/figure-gfm/plotAchaAggYearQtr-1.png)<!-- -->

## Merge SDUD with ACHA Information

``` r
moudWithFlMedicaid <- yearQuarterAllFLAggregate |>
  inner_join(
    select(aggByYearQtrDenom, yearQtr, total),
    by = c('yearQuarter' = 'yearQtr')
  ) |>
  mutate(ratePer100k = (totalRX / total) * 100000)

moudWithFlMedicaid
```

    ##      yearQuarter      allDrugs totalRX   total ratePer100k
    ##   1:     2016 Q1      Bunavail      15 1626231   0.9223784
    ##   2:     2016 Q1 Buprenorphine    1294 1626231  79.5705078
    ##   3:     2016 Q1     Sublocade       0 1626231   0.0000000
    ##   4:     2016 Q1      Suboxone    3870 1626231 237.9736208
    ##   5:     2016 Q1       Zubsolv     185 1626231  11.3760000
    ##  ---                                                      
    ## 101:     2021 Q1      Bunavail       0 2091733   0.0000000
    ## 102:     2021 Q1 Buprenorphine    7455 2091733 356.4030400
    ## 103:     2021 Q1     Sublocade      52 2091733   2.4859769
    ## 104:     2021 Q1      Suboxone    7553 2091733 361.0881504
    ## 105:     2021 Q1       Zubsolv     193 2091733   9.2267990

``` r
write_csv(moudWithFlMedicaid, file = paste0('./data/clean/', format(Sys.Date(), '%Y%m%d'), "_fl-moud-aggregate.csv"), na = '')
```

## Plot the Normalized Rate

``` r
xPos <- aggByYearQtr |>
  distinct(yearQtr) |>
  rowid_to_column()

xMinimum <- xPos$rowid[xPos$yearQtr == "2020 Q1"]
xMaximum <- xPos$rowid[xPos$yearQtr == "2020 Q2"]

plotMOUDWithFLMedicaid <- moudWithFlMedicaid |>
  ggplot() + 
  geom_point(aes(x = as.factor(yearQuarter), y = ratePer100k, group = allDrugs, color = allDrugs), size = 2) +
  geom_line(aes(x = as.factor(yearQuarter), y = ratePer100k, group = allDrugs, color = allDrugs), size = 1) +
  annotate('rect', xmin = xMinimum, xmax = xMaximum, ymin = -Inf, ymax = Inf, fill = 'red', alpha = 0.1) +
  scale_color_manual(values = c("Bunavail" = "grey", "Buprenorphine" = "#FA841A", "Suboxone" = "#004DA9", "Zubsolv" = "chartreuse4")) +
  xlab('Year - Quarter') +
  ylab('Prescription Rate per 100,000 Adult Medicaid Enrollees') +
  ggtitle("Normalized MOUD Prescription Rate - Florida") +
  theme_ipsum_rc() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, hjust = 0.5, vjust = -1),
    axis.title.y = element_text(size = 16, hjust = 0.5),
    legend.position = 'bottom',
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.key.size = unit(3, 'line')
  ) +
  labs(color = 'Drug Name')
  
plotMOUDWithFLMedicaid
```

![](02_moud-in-fl-medicaid_files/figure-gfm/moudNormalized100k-1.png)<!-- -->
