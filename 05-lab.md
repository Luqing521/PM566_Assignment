Lab 05 - Data Wrangling
================

# Learning goals

  - Use the `merge()` function to join two datasets.
  - Deal with missings and impute data.
  - Identify relevant observations using `quantile()`.
  - Practice your GitHub skills.

# Lab description

For this lab we will be, again, dealing with the meteorological dataset
downloaded from the NOAA, the `met`. In this case, we will use
`data.table` to answer some questions regarding the `met` dataset, while
at the same time practice your Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup the Git project and the GitHub repository

1.  Go to your documents (or wherever you are planning to store the
    data) in your computer, and create a folder for this project, for
    example, “PM566-labs”

2.  In that folder, save [this
    template](https://raw.githubusercontent.com/USCbiostats/PM566/master/content/assignment/05-lab.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository, hopefully of
    the same name that this folder has, i.e., “PM566-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir PM566-labs
cd PM566-labs

# Step 2
wget https://raw.githubusercontent.com/USCbiostats/PM566/master/content/assignment/05-lab.Rmd 
mv 05-lab.Rmd README.md

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/PM566-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("PM566-labs")
setwd("PM566-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/USCbiostats/PM566/master/content/assignment/05-lab.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/PM566-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you
    plan to work with those).

<!-- end list -->

``` r
library(data.table)
met<- fread("/Users/zhenglong/OneDrive - University of Southern California/courses/PM566/week5/lab/met.gz")
```

2.  Load the met data from
    <https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz>,
    and also the station data. For the later, you can use the code we
    used during lecture to pre-process the stations data:

<!-- end list -->

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

3.  Merge the data as we did during the lecture.

## Question 1: Representative station for the US

What is the median station in terms of temperature, wind speed, and
atmospheric pressure? Look for the three weather stations that best
represent continental US using the `quantile()` function. Do these three
coincide?

``` r
#obtaining averages per station
met<- merge( x= met, y= stations, by.x= "USAFID", by.y = "USAF",
             all.x = TRUE, all.y = FALSE)
met[1:5, .(USAFID, WBAN, STATE)]
```

    ##    USAFID  WBAN STATE
    ## 1: 690150 93121    CA
    ## 2: 690150 93121    CA
    ## 3: 690150 93121    CA
    ## 4: 690150 93121    CA
    ## 5: 690150 93121    CA

``` r
met_stations <- met[ , .(temp = mean(temp, na.rm = T),
                        wind.sp = mean(wind.sp,na.rm = T),
                        atm.press = mean(atm.press,na.rm = T)), 
                        by = .(USAFID, STATE)]
# computering the median
met_stations[, temp50  :=quantile(temp,probs = .5, na.rm = T)]
met_stations[, windsp50  :=quantile(wind.sp,probs = .5, na.rm = T)]
met_stations[, atmp50  :=quantile(atm.press,probs = .5, na.rm = T)]

#filter the data
met_stations[which.min(abs(temp-temp50))]
```

    ##    USAFID STATE     temp  wind.sp atm.press   temp50 windsp50   atmp50
    ## 1: 720458    KY 23.68173 1.209682       NaN 23.68406 2.461838 1014.691

``` r
met_stations[which.min(abs(wind.sp-windsp50))]
```

    ##    USAFID STATE     temp  wind.sp atm.press   temp50 windsp50   atmp50
    ## 1: 720929    WI 17.43278 2.461838       NaN 23.68406 2.461838 1014.691

``` r
met_stations[which.min(abs(atm.press-atmp50))]
```

    ##    USAFID STATE     temp  wind.sp atm.press   temp50 windsp50   atmp50
    ## 1: 722238    AL 26.13978 1.472656  1014.691 23.68406 2.461838 1014.691

These three do not coincide.

Knit the document, commit your changes, and Save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

``` r
# computing the median
met_stations[, temp50s  :=quantile(temp,probs = .5, na.rm = T), by = STATE]
met_stations[, windsp50s  :=quantile(wind.sp,probs = .5, na.rm = T), by = STATE]
met_stations[, atmp50s  :=quantile(atm.press,probs = .5, na.rm = T), by = STATE]

#temperature 
met_stations[, tempdif := which.min(abs(temp-temp50s)), by = STATE]
met_stations[, recordid := 1:.N, by = STATE]
met_temp <- met_stations[recordid == tempdif, .(USAFID,temp,temp50, STATE)]
met_temp
```

    ##     USAFID     temp   temp50 STATE
    ##  1: 720202 17.16329 23.68406    OR
    ##  2: 720254 19.24684 23.68406    WA
    ##  3: 720284 20.51970 23.68406    MI
    ##  4: 720328 21.94820 23.68406    WV
    ##  5: 720545 22.44858 23.68406    CT
    ##  6: 720592 26.31534 23.68406    AL
    ##  7: 720605 25.87364 23.68406    SC
    ##  8: 720636 23.99322 23.68406    MO
    ##  9: 720855 18.45570 23.68406    ND
    ## 10: 720964 27.57697 23.68406    FL
    ## 11: 722041 27.84758 23.68406    LA
    ## 12: 722133 27.14427 23.68406    OK
    ## 13: 722142 20.32324 23.68406    ID
    ## 14: 722188 26.07275 23.68406    AR
    ## 15: 722197 26.70404 23.68406    GA
    ## 16: 722218 24.89883 23.68406    MD
    ## 17: 722322 23.98226 23.68406    KY
    ## 18: 722358 26.54093 23.68406    MS
    ## 19: 722550 29.74982 23.68406    TX
    ## 20: 722692 24.37799 23.68406    VA
    ## 21: 722745 30.31538 23.68406    AZ
    ## 22: 722931 22.66268 23.68406    CA
    ## 23: 723060 24.70791 23.68406    NC
    ## 24: 723273 25.01262 23.68406    TN
    ## 25: 723658 24.94447 23.68406    NM
    ## 26: 724090 23.47238 23.68406    NJ
    ## 27: 724180 24.56026 23.68406    DE
    ## 28: 724200 22.03309 23.68406    OH
    ## 29: 724386 22.32575 23.68406    IN
    ## 30: 724555 24.21648 23.68406    KS
    ## 31: 724699 21.94228 23.68406    CO
    ## 32: 724855 24.34157 23.68406    NV
    ## 33: 724988 20.44142 23.68406    NY
    ## 34: 725064 21.40933 23.68406    MA
    ## 35: 725070 22.53551 23.68406    RI
    ## 36: 725130 21.69177 23.68406    PA
    ## 37: 725305 22.36831 23.68406    IL
    ## 38: 725526 21.87354 23.68406    NE
    ## 39: 725570 21.36209 23.68406    IA
    ## 40: 725724 24.39332 23.68406    UT
    ## 41: 726073 18.82098 23.68406    ME
    ## 42: 726115 18.60548 23.68406    VT
    ## 43: 726116 19.23920 23.68406    NH
    ## 44: 726438 18.85524 23.68406    WI
    ## 45: 726589 19.58483 23.68406    MN
    ## 46: 726627 20.35662 23.68406    SD
    ## 47: 726650 19.75554 23.68406    WY
    ## 48: 726777 19.15492 23.68406    MT
    ##     USAFID     temp   temp50 STATE

``` r
#wind.sp
met_stations[, tempdif :=which.min(abs(wind.sp-windsp50s)), by = STATE]
met_stations[recordid == tempdif, .(USAFID,wind.sp,windsp50, STATE)]
```

    ##     USAFID  wind.sp windsp50 STATE
    ##  1: 720254 1.268571 2.461838    WA
    ##  2: 720328 1.617823 2.461838    WV
    ##  3: 720386 2.617071 2.461838    MN
    ##  4: 720422 3.679474 2.461838    KS
    ##  5: 720492 1.408247 2.461838    VT
    ##  6: 720532 3.098777 2.461838    CO
    ##  7: 720602 1.616549 2.461838    SC
    ##  8: 720858 3.972789 2.461838    ND
    ##  9: 720951 1.493666 2.461838    GA
    ## 10: 720971 3.873392 2.461838    WY
    ## 11: 721031 1.513550 2.461838    TN
    ## 12: 722029 2.699017 2.461838    FL
    ## 13: 722076 2.244115 2.461838    IL
    ## 14: 722165 1.599550 2.461838    MS
    ## 15: 722202 3.404683 2.461838    TX
    ## 16: 722218 1.883499 2.461838    MD
    ## 17: 722275 1.662132 2.461838    AL
    ## 18: 722486 1.592840 2.461838    LA
    ## 19: 722676 3.776083 2.461838    NM
    ## 20: 722740 3.125322 2.461838    AZ
    ## 21: 722899 2.561738 2.461838    CA
    ## 22: 723010 1.641749 2.461838    NC
    ## 23: 723415 1.875302 2.461838    AR
    ## 24: 723545 3.852697 2.461838    OK
    ## 25: 723860 2.968539 2.461838    NV
    ## 26: 724006 1.650539 2.461838    VA
    ## 27: 724090 2.148606 2.461838    NJ
    ## 28: 724180 2.752929 2.461838    DE
    ## 29: 724303 2.606462 2.461838    OH
    ## 30: 724350 1.930836 2.461838    KY
    ## 31: 724373 2.347673 2.461838    IN
    ## 32: 724458 2.459746 2.461838    MO
    ## 33: 724700 3.180628 2.461838    UT
    ## 34: 725016 2.376050 2.461838    NY
    ## 35: 725079 2.583469 2.461838    RI
    ## 36: 725087 2.126514 2.461838    CT
    ## 37: 725088 2.773018 2.461838    MA
    ## 38: 725103 1.784167 2.461838    PA
    ## 39: 725464 2.679227 2.461838    IA
    ## 40: 725624 3.192539 2.461838    NE
    ## 41: 725867 2.702517 2.461838    ID
    ## 42: 725975 2.080792 2.461838    OR
    ## 43: 726056 1.556907 2.461838    NH
    ## 44: 726077 2.337241 2.461838    ME
    ## 45: 726284 2.273423 2.461838    MI
    ## 46: 726504 2.053283 2.461838    WI
    ## 47: 726519 3.665638 2.461838    SD
    ## 48: 726770 4.151737 2.461838    MT
    ##     USAFID  wind.sp windsp50 STATE

``` r
#atm.press
met_stations[, tempdif :=which.min(abs(atm.press-atmp50s)), by = STATE]
met_stations[recordid == tempdif, .(USAFID,atm.press,atmp50, STATE)]
```

    ##     USAFID atm.press   atmp50 STATE
    ##  1: 720254       NaN 1014.691    WA
    ##  2: 720858       NaN 1014.691    ND
    ##  3: 722029  1015.335 1014.691    FL
    ##  4: 722085  1015.298 1014.691    SC
    ##  5: 722093  1014.906 1014.691    MI
    ##  6: 722181  1015.208 1014.691    GA
    ##  7: 722269  1014.926 1014.691    AL
    ##  8: 722320  1014.593 1014.691    LA
    ##  9: 722340  1014.842 1014.691    MS
    ## 10: 722479  1012.464 1014.691    TX
    ## 11: 722745  1010.144 1014.691    AZ
    ## 12: 722899  1012.557 1014.691    CA
    ## 13: 723109  1015.420 1014.691    NC
    ## 14: 723300  1014.522 1014.691    MO
    ## 15: 723346  1015.144 1014.691    TN
    ## 16: 723436  1014.591 1014.691    AR
    ## 17: 723537  1012.567 1014.691    OK
    ## 18: 723600  1012.404 1014.691    NM
    ## 19: 724037  1015.158 1014.691    VA
    ## 20: 724040  1014.824 1014.691    MD
    ## 21: 724075  1014.825 1014.691    NJ
    ## 22: 724120  1015.757 1014.691    WV
    ## 23: 724180  1015.046 1014.691    DE
    ## 24: 724237  1015.236 1014.691    KY
    ## 25: 724286  1015.351 1014.691    OH
    ## 26: 724373  1015.063 1014.691    IN
    ## 27: 724586  1013.389 1014.691    KS
    ## 28: 724660  1013.334 1014.691    CO
    ## 29: 724860  1011.947 1014.691    NV
    ## 30: 725040  1014.810 1014.691    CT
    ## 31: 725053  1014.887 1014.691    NY
    ## 32: 725064  1014.721 1014.691    MA
    ## 33: 725070  1014.837 1014.691    RI
    ## 34: 725109  1015.474 1014.691    PA
    ## 35: 725440  1014.760 1014.691    IL
    ## 36: 725461  1014.957 1014.691    IA
    ## 37: 725555  1014.345 1014.691    NE
    ## 38: 725686  1013.157 1014.691    WY
    ## 39: 725755  1012.243 1014.691    UT
    ## 40: 725784  1012.908 1014.691    ID
    ## 41: 725895  1014.726 1014.691    OR
    ## 42: 726114  1014.792 1014.691    VT
    ## 43: 726155  1014.689 1014.691    NH
    ## 44: 726196  1014.323 1014.691    ME
    ## 45: 726425  1014.893 1014.691    WI
    ## 46: 726545  1014.497 1014.691    SD
    ## 47: 726559  1015.042 1014.691    MN
    ## 48: 726777  1014.299 1014.691    MT
    ##     USAFID atm.press   atmp50 STATE

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

``` r
met_stations <- unique(met[, .(USAFID, STATE, lon, lat)])

met_stations[, n := 1:.N, by = USAFID]
met_stations <- met_stations[n ==1]
# .SD is a short cut code
# met_stations[, .SD[1], by = USAFID]

met_stations[ , lat_mid := (max(lat) - min(lat))/2, by = STATE]
met_stations[ , lon_mid := (max(lon) - min(lon))/2, by = STATE]

#looking at the distance
met_stations[, distance := sqrt((lat - lat_mid)^2 + (lon - lon_mid)^2)]
met_stations[, minrecord := which.min(distance), by = STATE]
met_stations[, n := 1:.N, by = STATE]
met_location<- met_stations[n == minrecord, .(USAFID, STATE, lat,lon)]
met_location
```

    ##     USAFID STATE    lat      lon
    ##  1: 720369    ID 43.743 -111.097
    ##  2: 720388    WA 47.104 -122.287
    ##  3: 720436    KS 37.450  -94.733
    ##  4: 720458    KY 37.751  -82.637
    ##  5: 720613    SC 33.828  -79.122
    ##  6: 720717    MS 29.117  -89.550
    ##  7: 720741    NV 35.947 -114.861
    ##  8: 720918    LA 29.296  -88.842
    ##  9: 722004    ND 46.244  -96.607
    ## 10: 722049    FL 26.250  -80.108
    ## 11: 722137    GA 31.152  -81.391
    ## 12: 722268    AL 31.317  -85.450
    ## 13: 722688    NM 32.693 -103.212
    ## 14: 722720    AZ 31.458 -109.606
    ## 15: 723139    NC 35.233  -75.622
    ## 16: 723290    MO 37.225  -89.571
    ## 17: 723350    TN 36.480  -82.399
    ## 18: 723409    AR 35.940  -89.831
    ## 19: 723629    TX 30.069  -93.804
    ## 20: 723759    OK 33.909  -94.859
    ## 21: 723805    CA 34.768 -114.618
    ## 22: 723980    MD 38.341  -75.513
    ## 23: 724020    VA 37.933  -75.483
    ## 24: 724084    NJ 40.183  -74.133
    ## 25: 724085    PA 40.079  -75.013
    ## 26: 724093    DE 38.690  -75.363
    ## 27: 724177    WV 39.404  -77.945
    ## 28: 724646    CO 37.283 -102.613
    ## 29: 724776    UT 38.750 -109.762
    ## 30: 725014    NY 41.073  -71.923
    ## 31: 725046    CT 41.328  -72.049
    ## 32: 725060    MA 41.253  -70.061
    ## 33: 725079    RI 41.533  -71.283
    ## 34: 725250    OH 41.267  -80.683
    ## 35: 725336    IN 40.234  -85.394
    ## 36: 725342    IL 38.764  -87.606
    ## 37: 725384    MI 42.911  -82.529
    ## 38: 725473    IA 41.831  -90.329
    ## 39: 725533    NE 40.080  -95.592
    ## 40: 725763    WY 42.061 -104.158
    ## 41: 725976    OR 42.161 -120.399
    ## 42: 726055    NH 43.083  -70.817
    ## 43: 726077    ME 44.450  -68.367
    ## 44: 726115    VT 43.344  -72.518
    ## 45: 726424    WI 42.761  -87.814
    ## 46: 726510    SD 43.583  -96.733
    ## 47: 726588    MN 44.077  -91.708
    ## 48: 726777    MT 46.358 -104.250
    ##     USAFID STATE    lat      lon

``` r
all_stations <- met[, .(USAFID, STATE, lat,lon)][, .SD[1], by ="USAFID"]
 #recording lon and lat from the original dataset
met_temp <- merge(
  x = met_temp, y= all_stations,
  by = "USAFID",
  all.x = TRUE, all.y = FALSE
)
```

``` r
# combine the data set
library(leaflet)
dat1 <- met_location[, .(lon,lat)]
dat1[, type := "center of the state"]

dat2 <- met_temp[, .(lon,lat)]
dat2[, type := "center of the temperature"]

dat <- rbind(dat1, dat2)
rh_pal <- colorFactor(c('blue','red'), domain = as.factor(dat$type))

leaflet(dat) %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addCircles(lng = ~lon, lat = ~lat, color=~rh_pal(type), opacity=1, fillOpacity=1, radius=500)
```

<!--html_preserve-->

<div id="htmlwidget-29f390e782cf3027e9c0" class="leaflet html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-29f390e782cf3027e9c0">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["OpenStreetMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircles","args":[[43.743,47.104,37.45,37.751,33.828,29.117,35.947,29.296,46.244,26.25,31.152,31.317,32.693,31.458,35.233,37.225,36.48,35.94,30.069,33.909,34.768,38.341,37.933,40.183,40.079,38.69,39.404,37.283,38.75,41.073,41.328,41.253,41.533,41.267,40.234,38.764,42.911,41.831,40.08,42.061,42.161,43.083,44.45,43.344,42.761,43.583,44.077,46.358,45.417,46.683,42.574,39,41.384,30.46,34.717,38.35,48.784,30.033,29.445,35.438,44.523,35.211,33.355,38.533,37.033,31.183,28.85,38.586,32.167,32.867,35.867,36.009,36.744,40.033,39.674,40.82,40.412,39.135,39.909,38.051,42.571,41.91,41.733,41.333,41.914,40.717,42.4,40.219,44.533,43.344,43.626,43.156,43.683,45.604,44.339,46.358],[-111.097,-122.287,-94.733,-82.637,-79.122,-89.55,-114.861,-88.842,-96.607,-80.108,-81.391,-85.45,-103.212,-109.606,-75.622,-89.571,-82.399,-89.831,-93.804,-94.859,-114.618,-75.513,-75.483,-74.133,-75.013,-75.363,-77.945,-102.613,-109.762,-71.923,-72.049,-70.061,-71.283,-80.683,-85.394,-87.606,-82.529,-90.329,-95.592,-104.158,-120.399,-70.817,-68.367,-72.518,-87.814,-96.733,-91.708,-104.25,-123.817,-122.983,-84.811,-80.274,-72.506,-87.877,-79.95,-93.683,-97.632,-85.533,-90.261,-94.803,-114.215,-91.738,-84.567,-76.033,-85.95,-90.471,-96.917,-77.711,-110.883,-117.133,-78.783,-86.52,-108.229,-74.35,-75.606,-82.518,-86.937,-96.679,-105.117,-117.09,-77.713,-70.729,-71.433,-75.717,-88.246,-99,-96.383,-111.723,-69.667,-72.518,-72.305,-90.678,-93.367,-103.546,-105.541,-104.25],500,null,null,{"interactive":true,"className":"","stroke":true,"color":["#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000"],"weight":5,"opacity":1,"fill":true,"fillColor":["#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000"],"fillOpacity":1},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]}],"limits":{"lat":[26.25,48.784],"lng":[-123.817,-68.367]}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

  - low: temp \< 20
  - Mid: temp \>= 20 and temp \< 25
  - High: temp \>= 25

<!-- end list -->

``` r
#computing the states' average temperature
#temp_ave <- met[ , .(temp = mean(temp, na.rm = T)), by = .(USAFID, STATE)]
#temp_ave[, .temp_level := ifelse(temp <20, low),]
#temp_ave[, .temp_leve:= ifelse(temp >=25, High)]
#temp_ave[, .temp_leve := ifelse((temp <25)|(temp >=20), Mid)]
```

Once you are done with that, you can compute the following:

  - Number of entries (records),
  - Number of NA entries,
  - Number of stations,
  - Number of states included, and
  - Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

Knit the document, commit your changes, and push them to GitHub. If
you’d like, you can take this time to include the link of [the issue
of the week](https://github.com/USCbiostats/PM566/issues/23) so that you
let us know when you are done, e.g.,

``` bash
git commit -a -m "Finalizing lab 5 https://github.com/USCbiostats/PM566/issues/23"
```
