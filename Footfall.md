Footfall Project
================
Niall Ryan

## Introduction

This report will explore the effect of Covid-19 on pedestrian footfall
count in Dublin City Centre through the use of exploratory data
analysis. This analysis will focus on the pedestrian footfall counts of
people walking past certain locations in Dublin City Centre in 2020 from
January to the end of the Novemeber.

All of the data was extracted from the Smart Dublin website via the
Dublin City Council who collected the raw data in a csv file which we
loaded in so we could produce an anlysis of this data.

## Importing and Inspecting the Dataset

Before exploratory analysis can begin, the data must be imported and its
structure inspected. The data was imported using read.csv and inspected
firstly using the str() function.

``` r
suppressMessages(library(tidyverse))

d<- read.csv("data/jan-nov-2020-ped-data.csv")
 
str(d)
```

    ## 'data.frame':    8040 obs. of  24 variables:
    ##  $ Date...Time                                    : chr  "01-01-2020 00:00:00" "01-01-2020 01:00:00" "01-01-2020 02:00:00" "01-01-2020 03:00:00" ...
    ##  $ O.Connell.St.Outside.Pennys                    : int  1662 1160 1110 1095 666 317 200 191 218 311 ...
    ##  $ O.Connell.St.Outside.Clerys                    : int  2195 990 856 842 351 102 127 158 208 338 ...
    ##  $ Mary.Street                                    : int  60 33 51 44 25 12 9 9 11 17 ...
    ##  $ Capel.Street...Removed.from.site.20.10         : int  277 214 219 176 98 55 37 18 30 45 ...
    ##  $ Aston.Quay                                     : int  1600 858 3499 1804 2562 883 500 1342 1279 3300 ...
    ##  $ Grafton.Street...CompuB                        : int  3481 1870 1626 1280 824 530 289 319 361 612 ...
    ##  $ Talbot.Street.North                            : int  109 53 71 49 40 31 9 21 53 51 ...
    ##  $ Doilier.Street..Burgh.Quay                     : int  994 745 710 1065 768 201 112 167 506 850 ...
    ##  $ Dawson.Street.Replacement                      : int  18 7 15 8 9 1 1 1 8 11 ...
    ##  $ Dame.Street..Counter.Missing.                  : int  1090 1148 1052 899 373 158 134 92 136 168 ...
    ##  $ Talbot.Street.South                            : int  1085 1296 1848 1783 1705 2073 1288 1168 2090 1388 ...
    ##  $ O.Connell.St..Parnell.St...AIB                 : int  821 469 347 373 209 110 107 73 95 179 ...
    ##  $ Grafton.Street...Nassau.Street...Suffolk.Street: int  1238 405 382 353 136 29 30 37 45 65 ...
    ##  $ College.Green..Bank.Of.Ireland                 : int  893 1091 1202 1115 1140 1164 596 377 418 481 ...
    ##  $ Henry.Street                                   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Westmoreland.Street.East                       : int  1577 566 474 410 120 41 43 44 55 107 ...
    ##  $ Dawson.Street                                  : logi  NA NA NA NA NA NA ...
    ##  $ Liffey.Street                                  : int  1180 1133 898 595 219 47 48 26 74 110 ...
    ##  $ Westmoreland.Street.West                       : int  2593 1917 1889 2101 873 464 241 306 354 652 ...
    ##  $ Grafton.Street                                 : int  1516 1169 1097 989 708 374 162 113 107 266 ...
    ##  $ Bachelors.Walk                                 : int  868 727 930 561 269 103 278 419 121 191 ...
    ##  $ College.Green...Church.Lane                    : int  1494 676 523 520 238 83 120 104 134 188 ...
    ##  $ College.Green...Dame.St.Side                   : int  1797 594 429 365 176 45 57 54 70 157 ...

The str() function shows there are 8040 observations across 24 columns,
column 1 containing chr values corresponding to date and time, and the
remaining 23 columns giving integer values of pedestrain count for
specific locations in Dublin.

No values for pedestrian count were obtained for Dawson Street, possibly
due to a malfunctioning counter. This column will be removed from
further analysis.

``` r
summary(d) 
```

    ##  Date...Time        O.Connell.St.Outside.Pennys O.Connell.St.Outside.Clerys
    ##  Length:8040        Min.   :   0.0              Min.   :   0.0             
    ##  Class :character   1st Qu.:  83.0              1st Qu.:  85.0             
    ##  Mode  :character   Median : 334.0              Median : 344.0             
    ##                     Mean   : 665.9              Mean   : 484.3             
    ##                     3rd Qu.: 996.5              3rd Qu.: 736.0             
    ##                     Max.   :3921.0              Max.   :2515.0             
    ##                     NA's   :1                   NA's   :1                  
    ##   Mary.Street    Capel.Street...Removed.from.site.20.10   Aston.Quay    
    ##  Min.   :  0.0   Min.   :   0.0                         Min.   :   0.0  
    ##  1st Qu.:  9.0   1st Qu.:  28.0                         1st Qu.: 260.0  
    ##  Median : 53.0   Median : 103.0                         Median : 587.0  
    ##  Mean   :127.3   Mean   : 150.4                         Mean   : 942.6  
    ##  3rd Qu.:181.0   3rd Qu.: 238.0                         3rd Qu.:1107.0  
    ##  Max.   :928.0   Max.   :2255.0                         Max.   :4953.0  
    ##  NA's   :4681    NA's   :1249                           NA's   :1       
    ##  Grafton.Street...CompuB Talbot.Street.North Doilier.Street..Burgh.Quay
    ##  Min.   :    0           Min.   :   0.0      Min.   :   0.0            
    ##  1st Qu.:  169           1st Qu.:  64.0      1st Qu.: 197.0            
    ##  Median :  653           Median : 187.0      Median : 486.0            
    ##  Mean   : 1113           Mean   : 456.7      Mean   : 623.3            
    ##  3rd Qu.: 1720           3rd Qu.: 560.0      3rd Qu.: 899.0            
    ##  Max.   :12188           Max.   :7726.0      Max.   :5995.0            
    ##  NA's   :1               NA's   :1           NA's   :1                 
    ##  Dawson.Street.Replacement Dame.Street..Counter.Missing. Talbot.Street.South
    ##  Min.   :  0.0             Min.   :   0.0                Min.   :   0.0     
    ##  1st Qu.: 34.0             1st Qu.:  28.0                1st Qu.:  52.0     
    ##  Median :100.0             Median : 130.0                Median : 377.0     
    ##  Mean   :118.6             Mean   : 305.8                Mean   : 695.8     
    ##  3rd Qu.:177.0             3rd Qu.: 444.0                3rd Qu.:1139.5     
    ##  Max.   :742.0             Max.   :2114.0                Max.   :5309.0     
    ##  NA's   :1                 NA's   :2143                  NA's   :1          
    ##  O.Connell.St..Parnell.St...AIB Grafton.Street...Nassau.Street...Suffolk.Street
    ##  Min.   :   0.0                 Min.   :   0.0                                 
    ##  1st Qu.:  55.0                 1st Qu.:  19.0                                 
    ##  Median : 284.0                 Median :  92.0                                 
    ##  Mean   : 392.1                 Mean   : 168.9                                 
    ##  3rd Qu.: 647.5                 3rd Qu.: 240.0                                 
    ##  Max.   :1924.0                 Max.   :1511.0                                 
    ##  NA's   :1                      NA's   :1                                      
    ##  College.Green..Bank.Of.Ireland  Henry.Street   Westmoreland.Street.East
    ##  Min.   :   0.0                 Min.   :    0   Min.   :   0.0          
    ##  1st Qu.: 109.0                 1st Qu.:  187   1st Qu.:  28.0          
    ##  Median : 356.0                 Median :  759   Median : 106.0          
    ##  Mean   : 423.2                 Mean   : 1247   Mean   : 180.6          
    ##  3rd Qu.: 585.2                 3rd Qu.: 1879   3rd Qu.: 245.5          
    ##  Max.   :2782.0                 Max.   :13212   Max.   :1577.0          
    ##  NA's   :936                    NA's   :853     NA's   :1               
    ##  Dawson.Street  Liffey.Street    Westmoreland.Street.West Grafton.Street  
    ##  Mode:logical   Min.   :   0.0   Min.   :   0.0           Min.   :   0.0  
    ##  NA's:8040      1st Qu.:  41.0   1st Qu.: 132.0           1st Qu.:  54.0  
    ##                 Median : 171.0   Median : 417.0           Median : 190.0  
    ##                 Mean   : 401.2   Mean   : 603.0           Mean   : 570.2  
    ##                 3rd Qu.: 521.0   3rd Qu.: 969.5           3rd Qu.: 806.0  
    ##                 Max.   :4700.0   Max.   :4941.0           Max.   :3862.0  
    ##                 NA's   :1        NA's   :1                NA's   :4587    
    ##  Bachelors.Walk   College.Green...Church.Lane College.Green...Dame.St.Side
    ##  Min.   :   0.0   Min.   :   0.0              Min.   :   0.0              
    ##  1st Qu.:  15.0   1st Qu.:  44.0              1st Qu.:  36.0              
    ##  Median :  50.0   Median : 170.0              Median : 154.0              
    ##  Mean   : 169.4   Mean   : 312.9              Mean   : 281.0              
    ##  3rd Qu.: 146.0   3rd Qu.: 419.0              3rd Qu.: 380.5              
    ##  Max.   :2129.0   Max.   :2134.0              Max.   :2259.0              
    ##  NA's   :1        NA's   :1                   NA's   :1

Next, the summary() function shows that there are many missing/NA values
in all columns but a very large portion of NA values were in Capel
Street, Mary Street, Dame Street, College Green, Henry Street and
Grafton Street columns.These values will be omitted in the analysis
also.

## Cleaning the Data

The next task before performing any exploritory data analysis on this
dataset was to clean and format the data. The first thing that needed to
be done was to remove the problematic Dawson Street column which was
entirely made up of missing values. After this, the replacement Dawson
Street counter was set as Dawson Street and other columns were renamed
in order to make them more presentable in our analysis.

``` r
ped<-d[,-18] 



ped<- ped %>% rename(`Capel Street`= Capel.Street...Removed.from.site.20.10,`Dame Street`= Dame.Street..Counter.Missing.,`Dawson Street`= Dawson.Street.Replacement)
```

However, there were still missing values present that needed to be
omitted from the dataset.The median was imputed to replace the missing
values. To ommit them a function was created to replace the missing
value with the median value of each street. We decided to impute the
Median for the missing values as it is not going to be skewed by any
outliers and in this case due to the type of count values being
recorded, it is therefore a more accurate prediction than imputing the
values with the mean(which can be skewed by outliers)or with 0
values(doesn’t give a true representation of the data).

``` r
replaceNA <-function(x){
  ifelse(is.na(x),median(x,na.rm =TRUE),x)
}

ped$O.Connell.St.Outside.Pennys <- replaceNA(ped$O.Connell.St.Outside.Pennys)
ped$O.Connell.St.Outside.Clerys<-replaceNA(ped$O.Connell.St.Outside.Clerys)
ped$Mary.Street<-replaceNA(ped$Mary.Street)
ped$`Capel Street`<-replaceNA(ped$`Capel Street`)
ped$Aston.Quay<-replaceNA(ped$O.Connell.St.Outside.Clerys)
ped$Grafton.Street...CompuB<- replaceNA(ped$Grafton.Street...CompuB)
ped$Talbot.Street.North<- replaceNA(ped$Talbot.Street.North)
ped$Doilier.Street..Burgh.Quay<- replaceNA(ped$Doilier.Street..Burgh.Quay)
ped$`Dawson Street`<- replaceNA(ped$`Dawson Street`)
ped$`Dame Street`<- replaceNA(ped$`Dame Street`)
ped$Talbot.Street.South <- replaceNA(ped$Talbot.Street.South)
ped$O.Connell.St..Parnell.St...AIB<- replaceNA(ped$O.Connell.St..Parnell.St...AIB)
ped$Grafton.Street...Nassau.Street...Suffolk.Street<- replaceNA(ped$Grafton.Street...Nassau.Street...Suffolk.Street)
ped$College.Green..Bank.Of.Ireland<- replaceNA(ped$College.Green..Bank.Of.Ireland)
ped$Henry.Street<- replaceNA(ped$Henry.Street)
ped$Westmoreland.Street.East<-replaceNA(ped$Westmoreland.Street.East)
ped$Liffey.Street<- replaceNA(ped$Liffey.Street)
ped$Westmoreland.Street.West<- replaceNA(ped$Westmoreland.Street.West)
ped$Grafton.Street<-replaceNA(ped$Grafton.Street)
ped$Bachelors.Walk<-replaceNA(ped$Grafton.Street)
ped$College.Green...Church.Lane<-replaceNA(ped$College.Green...Church.Lane)
ped$College.Green...Dame.St.Side<-replaceNA(ped$College.Green...Dame.St.Side)
```

After the missing values were ommitted from the dataset, additions could
be made and some last formatting issues needed to be dealt with before
the exploritory data analysis could occur. A total column was added to
the dataset to aid us in graphing the total amount of pedestrian
footfall for each hour of every day. Also date and time were seperated
and date was given a date format so we could make use of this time
series data in our analysis.

``` r
ped <- mutate(ped, total =O.Connell.St.Outside.Pennys+ O.Connell.St.Outside.Clerys+ ped$Mary.Street+ `Capel Street`+ Aston.Quay+ Grafton.Street...CompuB+ Talbot.Street.North+ Doilier.Street..Burgh.Quay+ `Dawson Street`+ `Dame Street`+ Talbot.Street.South+ O.Connell.St..Parnell.St...AIB+ Grafton.Street...Nassau.Street...Suffolk.Street+College.Green..Bank.Of.Ireland+ Henry.Street+ Westmoreland.Street.East+ Liffey.Street+ Westmoreland.Street.West+Grafton.Street+Bachelors.Walk+ College.Green...Church.Lane+ College.Green...Dame.St.Side )


ped <- separate(ped,Date...Time, c("Date", "Time"), sep=" ", remove=F)

ped$Date<-as.Date(ped$Date)
```

## Overview

Using the DCC footfall counter locations data provided, the following
map of Dublin and the counter locations was produced using a google maps
API. Firstly, the Dawson Street location was removed from the data as a
Dawson Street replacement counter at a different location was included.
Next, a base map of Dublin was formed using the getmap function, and the
Latitude and Longitude values from the original dataset was converted to
a geom value, “counters” with the st_assf function in the sf library.
Qmplot was then used to map the counter locations. The locations of the
counters are clearly clustered around Dublin city center, except for 1
at Talbot Street North which is seperate from the others. As the Talbot
Street North counter is further outside of the city center, it is
unsurpriseing that it has a relatively low median value for pedestrian
count at 187.

``` r
location<- read.csv("data/dcc-footfall-counter-locations-14082020.csv")
location<-location[-18,]

suppressMessages(library(ggmap))

ggmap::register_google(key = "AIzaSyAbeYjOXHG-rGqRbzQ66JaB9d5iMfXTSxM")
dublin <- get_map(location = "dublin", zoom = 13)
```

    ## ℹ <https://maps.googleapis.com/maps/api/staticmap?center=dublin&zoom=13&size=640x640&scale=2&maptype=terrain&language=en-EN&key=xxx-rGqRbzQ66JaB9d5iMfXTSxM>

    ## ℹ <https://maps.googleapis.com/maps/api/geocode/json?address=dublin&key=xxx-rGqRbzQ66JaB9d5iMfXTSxM>

``` r
counters <- st_as_sf(location, coords = c("Longitude", "Latitude"), crs = 4326) 

qmplot(Longitude,  Latitude, data =location,  source="google",maptype="roadmap",color = I("red"),zoom = 13)
```

    ## ℹ <https://maps.googleapis.com/maps/api/staticmap?center=53.344319,-6.298305&zoom=13&size=640x640&scale=2&maptype=roadmap&language=en-EN&key=xxx-rGqRbzQ66JaB9d5iMfXTSxM>

![](Footfall_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> The
following heatmap is a repesentation of the median value of pedestrian
count on each street, which are numbered 1-22. As shown, Mary Street(3)
had the lowest median value of pediestrians at 53, and Henry Street(15)
had the highest overall at 759. The remaining streets are also colour
graded from Yellow to Red based on their median values.

``` r
Medians<-c(lapply(ped[4:25], median))
location<- mutate(location, Medians)
Medians<-as.data.frame(Medians)
Medians_t <- t(Medians)
Medians_t<-as.data.frame(Medians_t)

Street.no<-c(1:22)
Street.no<-as.data.frame(Street.no)
Medians_t<-mutate(Medians_t, Street.no)
Medians_t<-as.matrix(Medians_t)
heatmap(Medians_t, scale="none")
```

![](Footfall_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Comparing Lockdown footfall

Focusing inon the data, we wanted to look specifically at the effect of
pedestrian numbers during the lockdown imposed at the end of March and
the lockdown imposed just in Dubin in September. To make an accurate
comparison, we looked at the first full month of each lockdown,
filtering our dataset accordingly.

``` r
street.nam <-rep(colnames(ped)[4:25])
street.nam <- as.factor(street.nam)

April<- colSums(ped[months(ped$Date)=='April',c(4:25)])

lock1 <- data.frame(street.nam,totapr=c(April))

lock1$Month<- c(rep('April',22))
lock1$Month<-as.factor(lock1$Month)



ggplot(data=lock1, aes(x=street.nam, y=totapr)) +
  geom_col(col='green')+coord_flip()+
  theme_classic()+
  labs(title = "Lockdown - Nationwide Level 5",
          subtitle = "Number of Pedestrians during this lockdown - April",
           y = "Total Pedestrians",
           x = "Street")+
  scale_y_continuous(breaks=c(50000,150000,250000,350000,450000))
```

![](Footfall_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

In the graph produced above, we see the pedestrian levels during the
first month of the level 5 lockdown. We see the effect on pedestrian on
each street showing a stark decrease in volume with 80% of the streets
data recorded from this month maintaining a total pedestrian count below
150,000 level. This shows there was a compliance towards the
Government’s “Stay at home” message.

The low numbers could also be down to the fact that all non-essential
businesses had to shut down during this period, so there was nothing
really to entice people to walk around Dublin City Centre.

In comparison, we looked at the lockdown imposed on Dublin (only) later
in the year, which was imposed in September. This was to see had there
been any changes in behaviour in the form of pedestrian numbers from the
first full month during the lockdown in April, we can see the second
lockdown graph shown below.

``` r
September<- colSums(ped[months(ped$Date)=='September',c(4:25)])

lock2 <- data.frame(street.nam,totsep=c(September))

lock2$Month<- c(rep('September',22))
lock2$Month<-as.factor(lock2$Month)




ggplot(data=lock2, aes(x=street.nam, y=totsep)) + geom_col(col='yellow')+coord_flip()+theme_classic()+labs(title = "Dublin Only Lockdown -  Level 3", 

           subtitle = "Number of Pedestrians during this lockdown - September", 

           y = "Total Pedestrians", 

           x = "Street") 
```

![](Footfall_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

We see there has been a change in numbers walking around as the total
amount has increased for each street from April. However, there is a
number of factors in which to explain this increase in numbers:

-The lockdown level in September was set to Level 3 so people had less
restrictions to abide by and therefore had more freedom (Level 5 – stay
at home,maximum 5km away vs Level 3 – stay within your county)

-Retail shops and restaurants were able to open in level 3 restrictions
so people had more reason to go into Dublin City Centre and walk around
then they did in April.

## Comparing post Lockdown Footfall

After looking at the number of pedestrians during both lockdowns, next
we focused our attention on the month post lockdown to see if there
there was increase or decrease after both lockdowns were lifted and then
compare these months after in this case May and October to see if there
was a change in pedestrian volume between them.

<img src="Footfall_files/figure-gfm/unnamed-chunk-10-1.png" width="1026" height="528" />

The graph above shows the difference between the number of pedestrians
in the first month post lockdowns. We see in May that people were still
reluctant to go out into Dublin continuing the same low numbers trend we
saw during the lockdown in April, which we seen in the previous section
of the report, even after the lockdown was lifted.

Our assumptions would suggest because there was an increase in the
number of pedestrians between the first lockdown and the second lockdown
during these restrictions, we would assume to see that we would see the
same trend post lockdown, and so there was no surprise that there was an
increase between post lockdowns when restrictions were lifted as October
shows an increase number of pedestrians compared to May, with numbers
doubling in Henry street, Grafton Street and Talbot Street South when
comparing the two months.

``` r
sumped<- ped[months(ped$Date)=='May'|months(ped$Date)=='October',c(1,2,3,26)]

 


sumped$month <- c(rep('May',744),rep('October',744))

 

sumped %>% group_by(Date,month) %>%
summarise(pedmean = mean(total)) %>%
ggplot(aes(x = Date, y=pedmean,color=month))+
  geom_point(size=2,color='pink')+
  geom_line(aes(group= factor(month)))+
  ggtitle('The Average Pedestrains Post Lockdown - Trends')+
  ylab('Average footfall')+theme_classic()
```

    ## `summarise()` has grouped output by 'Date'. You can override using the
    ## `.groups` argument.

![](Footfall_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

The line graph above shows the average amount of pedestrians in May and
October which show the first month after two different levels of
lockdowns. The latter lockdown was less restrictive than the former, so
it could be predicted that the mean value in 31 days after the first
lockdown did fluctuate slightly, but less intense than October.

After the first lockdown the pedestrian volume in Dublin City on average
remained between about 2800 and 5500 people per day. However, after the
second lockdown, the average amount raised to almost twice that number
of pedestrians in May. We see that the average amount of pedestrians
decreased dramatically after the 21st of October, the main reason for
this being Ireland once again went into a level 5 nationwide lockdown.

## Conclusion

This report looked to answer our research question which was what effect
did Covid-19 have on pedestrian footfall in Dublin City Centre in 2020.
What we learned from carrying out this exploratory data analysis is that
when Covid-19 struck it had a massive effect on the numbers of
pedestrians as this dramatically decreased when the first lockdown in at
the end of March start of April was imposed and even when lockdown
restrictions were lifted the numbers remained low showing there was
compliance about decreasing the spread of the virus.

In the latter half of the year, we see a steady increase in pedestrian
numbers even when Dublin was placed into a level 3 lockdown. When this
lockdown was lifted numbers continued to increase, we assume that this
was mainly due to non-essential business opening up and so people were
enticed into going into Dublin City Centre, this increase continued
until the end of October when there was a decrease as a nationwide level
5 lockdown was introduced.
