cropgit
================

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(finalfit)
library(gtsummary)
```

    ## Warning: package 'gtsummary' was built under R version 4.2.1

``` r
library(ggplot2)
crop<-read.csv("crop.data.csv")
crop$fertilizer<-as.character(crop$fertilizer)
crop$fertilizer<-as.factor(crop$fertilizer)
summary(crop)
```

    ##     density        block      fertilizer     yield      
    ##  Min.   :1.0   Min.   :1.00   1:32       Min.   :170.0  
    ##  1st Qu.:1.0   1st Qu.:1.75   2:32       1st Qu.:176.4  
    ##  Median :1.5   Median :2.50   3:32       Median :177.1  
    ##  Mean   :1.5   Mean   :2.50              Mean   :177.0  
    ##  3rd Qu.:2.0   3rd Qu.:3.25              3rd Qu.:177.7  
    ##  Max.   :2.0   Max.   :4.00              Max.   :181.4

``` r
head(crop, 10)
```

    ##    density block fertilizer    yield
    ## 1        1     1          1 170.2287
    ## 2        2     2          1 177.5500
    ## 3        1     3          1 176.4085
    ## 4        2     4          1 177.7036
    ## 5        1     1          1 177.1255
    ## 6        2     2          1 176.7783
    ## 7        1     3          1 170.7463
    ## 8        2     4          1 177.0612
    ## 9        1     1          1 176.2749
    ## 10       2     2          1 177.9672

``` r
shapiro.test(crop$yield)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  crop$yield
    ## W = 0.75424, p-value = 2.238e-11

``` r
cropdes<-crop %>% group_by(fertilizer)%>%
  summarise(count = n(),
            mean = mean(yield),
            sd = sd(yield))

cropdes 
```

    ## # A tibble: 3 × 4
    ##   fertilizer count  mean    sd
    ##   <fct>      <int> <dbl> <dbl>
    ## 1 1             32  176. 2.45 
    ## 2 2             32  177. 0.914
    ## 3 3             32  178. 1.22

``` r
cropex<-crop %>% group_by(fertilizer) %>% summarise(max = max(yield, na.rm=TRUE))
cropex   
```

    ## # A tibble: 3 × 2
    ##   fertilizer   max
    ##   <fct>      <dbl>
    ## 1 1           178.
    ## 2 2           180.
    ## 3 3           181.

``` r
ggplot(crop,aes(x = fertilizer,y = yield)) + geom_bar(aes(fill = fertilizer),stat = "identity",position = "dodge") + theme_classic() +labs(y="Values",  x="fertilizers", title="Comparision of f1, f2, f3") + theme(text = element_text(size=15),  axis.text.x = element_text(angle=90, hjust=1))
```

![](cropgit_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
crop.aov1<-aov(yield~fertilizer+density+block, data= crop)
summary(crop.aov1)
```

    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## fertilizer   2  69.14   34.57  13.134 9.74e-06 ***
    ## density      1  17.66   17.66   6.709   0.0112 *  
    ## block        1   0.64    0.64   0.243   0.6235    
    ## Residuals   91 239.53    2.63                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
crop.aov2<-aov(yield~fertilizer+density+fertilizer*density, data= crop)
summary(crop.aov2)
```

    ##                    Df Sum Sq Mean Sq F value   Pr(>F)    
    ## fertilizer          2  69.14   34.57  12.985 1.11e-05 ***
    ## density             1  17.66   17.66   6.633   0.0116 *  
    ## fertilizer:density  2   0.55    0.28   0.104   0.9015    
    ## Residuals          90 239.62    2.66                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
crop.aov2<-aov(yield~factor(fertilizer)+ factor(density), data= crop)
TukeyHSD(crop.aov2, conf.level=.95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = yield ~ factor(fertilizer) + factor(density), data = crop)
    ## 
    ## $`factor(fertilizer)`
    ##          diff        lwr      upr     p adj
    ## 2-1 1.5511687  0.5889168 2.513421 0.0006553
    ## 3-1 1.9741256  1.0118736 2.936378 0.0000128
    ## 3-2 0.4229568 -0.5392951 1.385209 0.5492595
    ## 
    ## $`factor(density)`
    ##          diff       lwr      upr     p adj
    ## 2-1 0.8577894 0.2027656 1.512813 0.0108342

``` r
tbl_regression(crop.aov2,exponentiate = TRUE)
```

<div id="eqlkffplyv" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#eqlkffplyv .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#eqlkffplyv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#eqlkffplyv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#eqlkffplyv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#eqlkffplyv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eqlkffplyv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#eqlkffplyv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#eqlkffplyv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#eqlkffplyv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#eqlkffplyv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#eqlkffplyv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#eqlkffplyv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#eqlkffplyv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#eqlkffplyv .gt_from_md > :first-child {
  margin-top: 0;
}

#eqlkffplyv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#eqlkffplyv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#eqlkffplyv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#eqlkffplyv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#eqlkffplyv .gt_row_group_first td {
  border-top-width: 2px;
}

#eqlkffplyv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eqlkffplyv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#eqlkffplyv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#eqlkffplyv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eqlkffplyv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eqlkffplyv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#eqlkffplyv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#eqlkffplyv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eqlkffplyv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#eqlkffplyv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#eqlkffplyv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#eqlkffplyv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#eqlkffplyv .gt_left {
  text-align: left;
}

#eqlkffplyv .gt_center {
  text-align: center;
}

#eqlkffplyv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#eqlkffplyv .gt_font_normal {
  font-weight: normal;
}

#eqlkffplyv .gt_font_bold {
  font-weight: bold;
}

#eqlkffplyv .gt_font_italic {
  font-style: italic;
}

#eqlkffplyv .gt_super {
  font-size: 65%;
}

#eqlkffplyv .gt_two_val_uncert {
  display: inline-block;
  line-height: 1em;
  text-align: right;
  font-size: 60%;
  vertical-align: -0.25em;
  margin-left: 0.1em;
}

#eqlkffplyv .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#eqlkffplyv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#eqlkffplyv .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#eqlkffplyv .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#eqlkffplyv .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">factor(fertilizer)</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">factor(density)</td>
<td class="gt_row gt_center">0.011</td></tr>
  </tbody>
  
  
</table>
</div>

``` r
ggplot(crop, aes(fertilizer, yield)) + geom_boxplot(aes(fill = fertilizer))+
  theme(panel.grid.major = element_blank())
```

![](cropgit_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->
