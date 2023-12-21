---
title: "SAS vs. R for data analysis"
output: html_document
date: "`r Sys.Date()`"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(eval = FALSE, message = FALSE, warning = FALSE)
```


Moving from a SAS environment to the R programming language requires a paradigm change in thinking. I hope that this documents helps you in deciding whether to stay with SAS or move to R. This document is written for the intermediate SAS epidemiologist who is exploring the use of R.

### Difference in approach

The biggest difference between the two for data analysis is that R is a full programming language and works with numbers, lists, matrices, and tables. R treats these as objects, and you write functions to change these objects. There are often many ways to achieve your intended result, and that flexibility may initially be overwhelming. In SAS, you primarily work with data tables and manipulate data through the data step, which steps through the data row by row. This difference in changing or mutating different types of objects as a whole, vs. stepping through the dataset row by row means the functions you use in R are more abstract, especially when used in combination with other functions.

The other big difference is that to summarize data in SAS, you use procedure steps that guide you in generating reports and summary statistics. In R, the summary functions are more rudimentary, giving you the flexibility to format your table in a wide variety of options, but which comes with a higher learning curve. 

### A quick note about packages

The R programming language is very extensible, meaning that just like many browsers, you can download and install extensions that extend the functionality beyond the base R language. We will be using many of the popular packages in our lessons below. The benefits of using packages is ease of use. The functions included with many of these packages simplify or combine base R functions. Many end users rely heavily on these packages in day to day work. The negative is that these packages may hide much of the complexities of the underlying programming. 

Data problems can be solved without any packages, using only base R functions that come with the standard install. But that requires in-depth knowledge of base R functions (as you will see in some of the examples below). Here's a quick example below that shows sorting two columns using packages vs. base R functions only. 

Base R functions only:
```
# import data set using base R code only
file_location <- paste0(getwd(), "/", "surveillance_linelist_20141201.csv")
df <- read.csv(file = file_location)

# this code will will sort the onset date column, but will ignore all the other columns
sorted <- sort(as.Date(df$onset.date, format = "%m/%d/%Y"), na.last = FALSE)

# instead, create an sorting index based on two variables
sort_index <- order(as.Date(df$onset.date, format = "%m/%d/%Y"), df$gender, na.last = FALSE)

# re-arrange dataset based on sort index
df <- df[sort_index, ,drop=FALSE]
```

We cannot use the base R sort function because that function only sorts the specified column in the dataset, without re-ordering the other columns. Instead we create a vector that shows what order rows should come in based off the columns we specify before re-arranging the rows using subsetting. Using only base R functions requires knowledge of a functions options, even for a standard operation for sorting. Here is the same code using the dplyr and lubridate package:
```
# install helper packages
pacman::p_load(
  rio,  here,  dplyr,  janitor,  lubridate,  epikit
  )
  
# re-load dataset using helper functions
df <- import(here("surveillance_linelist_20141201.csv")) 

# sort using dplyr function arrange
df <- df %>% arrange(mdy(`onset date`), gender)
```
You can see this code that uses packages is very readable and similar to the SAS code:
```
/* set recommended options */
options compress=yes;
options nofmterr;

/*Creating relative path to data folders*/
%let a=%sysget(SAS_EXECFILEPATH);
%let b=%sysget(SAS_EXECFILENAME);
%let folder= %sysfunc(tranwrd(&a,&b,));

/* import data file */
proc import datafile = "&folder\surveillance_linelist_20141201.csv" dbms=csv out=df replace; 
run;

/* sort on onset date and gender */
proc sort data=df;
	by gender onset_date;
run;
```


## Longer code comparison between SAS and R

Let's say I want to take a line listing, and number each patient 1, 2, 3..., grouped by day, and region i.e. the numbers restart for each new day and region. This could be useful if, for instance, you later want to find all the first, or third patient that presented symptoms for a given day at a particular region.

This is of moderate difficulty in SAS. We would first sort the data by date and region. SAS would then step through the data row-by-row and determine if the date and/or the region changes. This reflects how we would do this ourselves on pen and paper. 

SAS creates two hidden columns that writes the value `1` if the date or region column is different from the preceding row, thus specifying a new date or region. It writes `0` elsewhere. If the value is `1` (meaning a new date or region), then it resets the count to `0`. There are a few peculiarities to this that we won't cover, but this is in summary how the code works. 

You can copy the code below directly into your SAS editor and run it.

```         
/*Using by statement requires sorting*/
proc sort data=df;
	by onset_date adm3_name_det /* this is the region variable */;
run;

data df_onset_region_count;
	set df;
	where onset_date ne .;
	by onset_date adm3_name_det;
	retain running_count 0;

/*	unhide hidden helper columns*/
	first_det = first.adm3_name_det;
	first_onset = first.onset_date;

	if first.adm3_name_det = 1 then running_count=0;
	running_count = running_count + 1;
	keep onset_date adm3_name_det first_det first_onset running_count;
run;
```

By using the retain function, SAS retains the row count when the PDV re-initializes, before adding the 1 for the next row and outputting the result in the `running_count` column. SAS resets the `running_count` column when encountering a `1` in the hidden column, which indicates that the date or region has changed compared to the previous row (which is why sorting is required).

In R, you write functions that apply to the entire dataset. 

```    
# here is the code to create a running count column
df_onset_region_count <- df 
  %>% clean_names() %>% 
  group_by(mdy(onset_date), adm3_name_det) %>% 
  mutate(count = row_number()) %>% 
  select(onset_date, adm3_name_det, count)
```

While the code may look cleaner, figuring out which functions to use is more difficult. Behind the scenes, R is translating this function into the C programming language (which better mirrors how a computer calculates sums at a machine level) and still steps through the data but in a more efficient method. While this method in R is not any faster than SAS (the SAS program has been heavily optimized over the decades), in SAS you are always limited to stepping through the data set row by row. 

### A quick note about looping

You can always force R to loop through each row, counting the rows by group, similar to the SAS data step. But in many ways, understanding this code is more difficult than in SAS because it uses base R functions more heavily. And looping through each row in R is hundreds or even thousands of times slower than using functions that utilizing vectorized R functions as shown above. The example below only groups by unique date, not on region. 

```
# Get unique groups of dates
unique_groups <- unique(df$`onset date`)

# Add a new column for row numbers
df$row_number <- NA

# Find the row numbers for each unique date, and then assign row numbers
for (i in unique_groups) {
  group_rows <- which(df$`onset date` == i)
  df$row_number[group_rows] <- seq_along(group_rows)
}
df[,c(3,19,25)]
```
Hopefully this introductory example gives you a good idea of the major differences in approach when coding in SAS vs. R. The section below gives side-by-side examples of SAS and R code to complete the same data processing steps. 


## Comparing the Data Step

This and the next few sections showcase the same data manipulation steps in both SAS and R. These are designed to be inputted directly in your SAS and R console windows. The goal is to give you a peek into how SAS and R code can be different, but also alike. 

As you begin your journey in the R language, use these sections as a quick-start reference. We hope that the functions we show below represent the most commonly used functions in your day-to-day data analysis. 

First we manipulate the line listing using commonly used SAS functions. 
```{SAS}
/*Rename variables. This is separate datastep because of how SAS initializes variables*/
data df2;
	set df;
	/* Rename variables */
    rename onset_date = date_onset
           date_of_report = date_report
           adm3_name_res = district_res
           adm3_name_det = district_det;
run;
    
/*create a new dataframe called df2 from df*/
data df3;
    set df2;
    
    /* Drop unnecessary column */
    drop row_num;

     /* Denote missing if weight < 0 and if gender is Unknown */
    if wt__kg_ < 0 then wt__kg_ = .;
    age = input(age, best12.);
    if gender = 'Unknown' then gender = '';
    
    /* Recoding hospital names */
    if hospital = 'Mitilary Hospital' then hospital = 'Military Hospital';
    else if hospital = 'Port Hopital' then hospital = 'Port Hospital';
    else if hospital = 'Port' then hospital = 'Port Hospital';
    else if hospital = "St. Mark's Maternity Hospital (SMMH)" then hospital = "SMMH";
    
    /* Recoding gender */
    if gender = 'm' then gender = 'Male';
    else if gender = 'f' then gender = 'Female';
    
    /* Case definition */
    if lab_confirmed = 'TRUE' then case_def = 'Confirmed';
    else if epilink = 'yes' and fever = 'yes' then case_def = 'Suspect';
    else case_def = 'To investigate';
    
    /* Age in years */
    if age_unit = 'months' then age_years = age/12;
    else age_years = age;
    
    /* Age categories - assuming a custom function or format for 'age_categories' */
	format age_group $10.;
	if age <= 4 then age_group = '0-4'; 
 	else if age >=5 and age  <=17 then age_group = '5-17'; 
 	else if age >=18 and age <=24 then age_group = '18-24'; 
 	else if age >=25 and age <=49 then age_group = '25-49'; 
 	else if age >=50 and age <=64 then age_group = '50-64'; 
 	else if age >=65 and age <=150 then age_group = '65+';  

    /* Date difference */
    diff = date_report - date_onset;
    
    /* Location movement */
    if district_det ne district_res then moved = 1;
    else moved = 0;
    
    /* Keeping only confirmed cases */
    if case_def ne 'Confirmed' then delete;
    
run;
```
And here is the same code, but written in R language, using the lubridate and dplyr packages:
```
df3 <- df %>% 
  
  # clean names - SAS does this automatically
  clean_names() %>% 

  # rename variables 
  rename(
    date_onset = onset_date,
    date_report = date_of_report,
    district_res = adm3_name_res,
    district_det = adm3_name_det
  ) %>% 
  
  # drop unnecessary column
  select(-row_num) %>% 

  # R importer did not specify date and age type correctly
  mutate(date_onset = mdy(date_onset),
         date_report = mdy(date_report),
         age = as.numeric(age)
         ) %>% 
  
  # Denote missing if weight < 0 and if gender is Unknown
  mutate(wt_kg = ifelse(wt_kg <0, NA, wt_kg),
         gender = na_if(gender, 'Unknown')) %>% 
  
  # Recoding hospital names
  mutate(hospital = recode(hospital,
                           'Mitilary Hospital' = 'Military Hospital',
                           'Port Hopital' = 'Port Hospital',
                           'Port' = 'Port Hospital',
                           "St. Mark's Maternity Hospital (SMMH)" = "SMMH")) %>% 
  
  #recode gender
  mutate(gender = recode(gender,
                         'm' = 'Male', 
                         'f' = 'Female')) %>% 
  
  #categorize case def
  mutate(
    case_def = case_when(
      lab_confirmed == TRUE ~ 'Confirmed',
      epilink == 'yes' & fever == 'yes' ~ 'Suspect',
      TRUE ~ 'To investigate'
    )
  ) %>% 
  
  #column for age years
  mutate(age_years = case_when(
    age_unit == 'months'         ~      age/12,
    TRUE                         ~      age
  )) %>% 
  
  #age categories
  mutate(age_cat = ifelse(age <= 4, "0-4",
                   ifelse(age <= 17, "5-17",
                   ifelse(age <= 24, "18-24",
                   ifelse(age <= 49, "25-49",
                   ifelse(age <= 64, "50-64", "65+")))))) %>% 
  
  #date diff
  mutate(diff = date_report - date_onset) %>% 
  
  #location
  mutate(moved = district_det != district_res) %>% 
  
  #filter rows
  filter(case_def == 'Confirmed')
```
At the basic level, both codes look roughly similar, and that was likely the intention with the dplyr package, which hides much of the programming complexity behind easy to use functions. 

## Comparing PROC tables

The power of SAS lies in the numerous summary statistic reports that are able to be easily generated by simple proc statements. We try to re-create the most commonly used proc tables. SAS code on the left, and R code on the right.

### PROC Contents and PROC Summary
These procedures give you an initial idea of the makeup of the dataset. 

::::: columns
::: column
```SAS
/*general information and list of variables */
proc contents data=df3;
run;

/*summary statistics of all numeric variables*/
proc summary data=df3 print;
var _numeric_;
run;
```
:::

::: column
```r
str(df3) # shows type of each variable, and sample data
summary(df3) # summary statistics for numeric variables
```
:::
:::::

### PROC Freq
Proc Freq is one of the most powerful procedures that is commonly used. It is very flexible and can create a wide variety of different types of summary statistics. The code below produces a frequency table with percentages on one variable. 

::::: columns
::: column
```SAS
/*one way frequency table in descending order*/
proc freq data = df3 order=freq;
	tables hospital;
run;
```
:::

::: column
```r
# using dply and janitor to get a one way freq table
df3 %>% tabyl(hospital) %>% arrange(-n)
```
:::
:::::
