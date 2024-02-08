# SAS vs. R for epi data analysis

```{r setup, include=FALSE}
knitr::opts_chunk$set(eval = FALSE, message = FALSE, warning = FALSE)
```


Moving from a SAS environment to the R programming language requires a paradigm change in thinking. I hope that this documents helps you in deciding whether to stay with SAS or move to R. This document is written for the intermediate SAS epidemiologist who is exploring the use of R. This guide is not intended to teach you R from scratch. Rather I see two main purposes: 1) having someone more knowledgeable in R walk you through the differences between SAS and R using this document. This way you can see if R is something you would like to learn. And 2) reviewing concepts you have already learned in a basic intro course to R. 

### Difference in approach

The biggest difference between the two for data analysis is that R is a full programming language and works with numbers, lists, matrices, and tables. Tables can also be thought of as multiple lists of numbers put together. R treats all of these as objects, and you write functions to change these objects. There are often many ways to achieve your intended result, and that flexibility may initially be overwhelming. 

In SAS, you primarily work with data tables and and the outputs are either reports or new tables. The advantage is that this is just like Excel: everything you work with is a table, which is easy to understand and view. The limitation is that more complex programs are harder to write, where for instance, you would need one column of data to serve as the input of another program. 

The other big difference is that to summarize data and produce reports in SAS, you use procedure (PROC) steps that guide you in generating reports and summary statistics. In R, the summary functions are more rudimentary, giving you the flexibility to format your table in a wide variety of options, but which comes with a higher learning curve. 

### Sample Dataset

The same Github repository has a sample dataset that is used for this guide. Please download and put in the same working directory as your SAS and R code file. This dataset was provided to me from [Applied Epi](https://appliedepi.org/), a nonprofit organization strengthening epidemiological practice through training, tools, and support. 

### About packages

The R programming language is very extensible, meaning that just like many browsers, you can download and install extensions that extend the functionality beyond the base R language. We will be using many of the popular packages in our lessons below. The benefits of using packages is ease of use. The functions included with many of these packages simplify or combine base R functions. Many end users rely heavily on these packages in day to day work. The negative is that these packages may hide some of the complexities of the underlying programming. 

The ability to write custom packages for very specialized use cases is one reason many have converted to R. While you may be able to do everything in SAS as in R, the ability for R to improve and adapt through the use of new packages written by fellow epis means the pace of advancement is very fast. 

Data problems can be solved without any packages, using only base R functions that come with the standard install. But that requires in-depth knowledge of base R functions (as you will see in some of the examples below). Here's a quick example below that shows sorting two columns using packages vs. base R functions only. 

Base R functions only:
```r
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

You can see that the sort function fails. Why? We cannot use the base R sort function because that function only sorts the specified column in the dataset, without re-ordering the other columns. Instead we create a vector that shows what order rows should come in based off the columns we specify before re-arranging the rows using subsetting. Using only base R functions requires knowledge of a functions options, even for a standard operation for sorting. Here is the same code using the dplyr and lubridate package:
```r
# install helper packages
pacman::p_load(
  rio,  here,  dplyr,  janitor,  lubridate,  epikit
  )
  
# re-load dataset using helper functions
df <- import(here("surveillance_linelist_20141201.csv")) 

# sort using dplyr function arrange
df <- df %>% arrange(mdy(`onset date`), gender)
```
You can see this code that uses packages is very readable and similar to the SAS code below. Make sure you have saved your SAS file somewhere for the relative path macro to work. 
```SAS
/* set recommended options */
options compress=yes;
options nofmterr;

/*Creating relative path to data folders*/
%let a=%sysget(SAS_EXECFILEPATH);
%let b=%sysget(SAS_EXECFILENAME);
%let folder= %sysfunc(tranwrd(&a,&b,));

/* import data file */
proc import datafile = "&folder.surveillance_linelist_20141201.csv" dbms=csv out=df replace; 
run;

/* sort on onset date and gender */
proc sort data=df;
	by gender onset_date;
run;
```


## Longer code comparison between SAS and R

Here we show the difference between using vectorized functions in R, vs the data step in SAS that loops through each row in the data set. Vectorized operations in many programming languages allow a mathematical operation to be applied to multiple elements of a data set, meaning an entire data set, a column or row, or whatever subset of the data set you specify. 

Let's say I want to take a line listing, and number each patient 1, 2, 3..., grouped by day, and region i.e. the numbers restart for each new day and region. This could be useful if, for instance, you later want to find all the first, or third patient that presented symptoms for a given day at a particular region. In other words, we want a running count that restarts for every day or region. 

This is of moderate difficulty in SAS. We would first sort the data by date and region. SAS would then step through the data row-by-row and determine if the date and/or the region changes. This reflects how we would do this ourselves on pen and paper. 

SAS creates two hidden columns that writes the value `1` if the date or region column is different from the preceding row, thus specifying a new date or region. It writes `0` elsewhere. If the value is `1` (meaning a new date or region), then it resets the count to `0`. There are a few peculiarities to this that we won't cover, but this is in summary how the code works. 

You can copy the code below directly into your SAS editor and run it. Make sure you have already run some of the recommended options and relative folder macros in the previous section.  

```SAS      
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

In R, we can do the same by giving to a specialized function called `row_number()` groups of rows disaggregated by onset date and region. 

```  r  
# here is the code to create a running count column
df_onset_region_count <- df 
  %>% clean_names() %>% 
  group_by(mdy(onset_date), adm3_name_det) %>% 
  mutate(count = row_number()) %>% 
  select(onset_date, adm3_name_det, count)
```

While the code may look cleaner, figuring out which functions to use may be more more difficult. Behind the scenes, R is translating this function into the C programming language (which better mirrors how a computer calculates sums at a machine level) and still steps through the data but in a more efficient vectorized method. While this method in R is not any faster than SAS (the SAS program has been heavily optimized over the decades), in SAS you are always limited to stepping through the data set row by row. 

### A quick note about looping

You can always force R to loop through each row, counting the rows by group, similar to the SAS data step. But in many ways, understanding this code is more difficult than in SAS because it uses base R functions more heavily. And looping through each row in R is hundreds or even thousands of times slower than using functions that utilizing vectorized R functions as shown above. The example below only groups by unique date, not on region. 

```r
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

This and the next few sections showcase the same data manipulation steps in both SAS and R. These are designed to be inputted directly in your SAS and R console windows. The goal is to give you a peek into how SAS and R code can be different, but also alike, through the use of R packages. 

As you begin your journey in the R language, use these sections as a quick-start reference. We hope that the functions we show below represent the most commonly used functions in your day-to-day data analysis. 

First we manipulate the line listing using commonly used SAS functions. 
```SAS
/*Rename variables. This is separate datastep because of how SAS initializes variables*/
data df2;
	set df;
	/* Rename variables. Syntax is old_name = new_name */
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
```r
df3 <- df %>% 
  
  # clean names - SAS does this automatically
  clean_names() %>% 

  # rename variables; syntax is new_name = old_name
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
  filter(epilink == 'yes')
```
At the basic level, both codes look roughly similar, and that was likely the intention with the dplyr package, which hides much of the programming complexity behind easy to use functions. 

## Comparing PROC tables and reports

The power of SAS lies in the numerous summary statistic reports that are able to be easily generated by simple proc statements. We try to re-create the most commonly used proc tables. SAS code first, then the R code right below it:

### PROC Contents and PROC Summary
These procedures give you an initial idea of the makeup of the dataset. 

SAS:
```SAS
/*general information and list of variables */
proc contents data=df3;
run;

/*summary statistics of all numeric variables*/
proc summary data=df3 print;
var _numeric_;
run;
```

R:
```r
str(df3) # short for structure, this shows type of each variable, and sample data
summary(df3) # summary statistics for numeric variables
```


### PROC Freq
Proc Freq is one of the most powerful procedures commonly used. It is very flexible and can create a wide variety of different types of summary statistics. The code below produces a frequency table with percentages on one variable. 

SAS:
```SAS
/*one way frequency table in descending order*/
proc freq data = df3 order=freq;
	tables hospital / missing;
	where case_def = 'Confirmed';
run;
```

R:
```r
# using dply and janitor to get a one way freq table
df3 %>% filter(case_def == 'Confirmed') %>% tabyl(hospital) %>% arrange(-n)
```

This code produces a two-way frequency table:

SAS:
```SAS
/*two way frequency table for fever vs. chills, disaggregated by cough*/
proc sort data = df3; by cough; run;
proc freq data = df3;
	tables fever*chills / nocol norow;
	by cough;
run; 
```

R:
```r
# two way frequency table
df3 %>% tabyl(fever, chills, cough) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("all") %>% 
  adorn_pct_formatting(digits=2) %>% 
  adorn_ns()
```
Another powerful use for Proc Freq is creating multiple tables all at once. This is helpful if you have many variables you want to check at the same time.

SAS:

```SAS
proc freq data=df3;
	tables gender hospital fever--vomit district_res lab_confirmed case_def age_group;
run;
```
In R, there is no existing report that can easily generate separate tables for each column. Instead, you need some base R knowledge of the apply functions. The apply function runs a customized function on each list (in this scenario each list is a column of data). 

R:

```r
lapply(df3[,c(6, 9:13, 16, 19, 25, 27)], function(x) tabyl(x))
```

### PROC Sql

Proc SQL is another power procedure that is often used to create summary statistics. 
Here we calculate the average weight and max weight, by age group. 

SAS:
```SAS
proc sql;
  select sum(wt__kg_) / count(wt__kg_) as average_weight, max(wt__kg_) as max_weight, age_group
  from df3
  where case_def = 'Confirmed'
  group by age_group;
quit;
```

R:
```r
df3 %>% 
  group_by(age_cat) %>% 
  summarise(
    average_weight = mean(wt_kg, na.rm=T),
    max_weight = max(wt_kg, na.rm=T)
  )
```

Another powerful use of Proc SQL is de-duplicating multiple rows into one. There are many ways to approach this problem. Here is just one suggested solution. The first things to do would be to examine how many duplicates we have based on the columns we choose. The code below creates a table of the number of duplicates by `case_id`. 

SAS:
```SAS
proc sql;
create table dup as
select distinct case_id, date_onset, count(*) as unique_count
from df3
group by case_id, date_onset
order by unique_count desc;
quit;
```

Here is the same code in R:
```r
dup <- df3 %>% 
  group_by(case_id, date_onset) %>% 
  summarize(count = n()) %>% 
  arrange(-count)
```

Given this information, next we would like to view all the duplicates together. This will allow us to examine each duplicate in detail. In SAS we would filter the list to only those with `unique_count = 2`. Then inner join with the main dataset to include only the duplicates.

SAS:
```SAS
data dup_list;
	set dup;
	where unique_count = 2;
run;

proc sql;
create table dup2 as
select * from df3
join dup_list on df3.case_id = dup_list.case_id;
quit;
```

In R, we may take a slightly different approach. We would create a list of values from `case_id` where `unique_count = 2`. We would then filter the main table based off this list of `case_id`. 

R:
```r
dup_list <- dup %>% 
  filter(count == 2) %>% 
  pull(case_id)

dup2 <- df3 %>% 
  filter(case_id %in% dup_list2)
```

After examining the duplicates side-by-side, and if everything looks correct, we would then go ahead and remove the duplicates. 

SAS:
```SAS
proc sort data=df3 nodupkey dupout=dupout out=df3_distinct;
	by case_id date_onset;
run;
```

R:
```r
df3_distinct <- df3 %>% 
  distinct(case_id, date_onset)
```

## Macro functions
Some of the true capabilities of R shine when developing more complex functions. Let's say we want to automate a report for each district of residence showing the first five patients and their home latitude and longitude. 

There is a simple way to do this using PROC Print. This code below prints all rows disaggregated by district of residence. 
```SAS
proc sort data=df3; by district_res; run;
proc print data = df3;
	var case_id date_onset gender lat lon;
	by district_res;
run;
```
But let's say we want something more automated and extensible. That means writing a function where we can enter parameters such as the disease of interest or in this case the specific hospital of interest. In other words, we want a function that allows us to customize the report according to parameters set at runtime. 

Let's start with R first this time. In R, because the language has been built to handle objects other than datasets, such as vectors and lists, the code is somewhat simple. 

```r
print_bydistrict <- function (hospital_want) {
  # create a vector that contains all the unique districts
  district_res_unique <- unique(df3$district_res)
  
  #create a for loop to loop through the unique districts
  for (item in district_res_unique) {
    
    # create title
    title <- paste("First five line listings for district:", item)
    cat(title, "\n")
    
    # create table
    print(df3 %>% 
            filter(district_res == item,
                   hospital == hospital_want) %>% 
            select(case_id, date_onset, gender, lat, lon) %>% 
            head(5))
    cat("\n")
  }
}

print_bydistrict('Port Hospital')
```

Thinking through similar steps in SAS: we would first need a list of the unique district of residence because each district would have their own report. We would then run the report for each unique district. 

This is somewhat difficult because while PROC Freq may immediately display all of the unique value for district of residence, it is in a format that is not usable for the computer. Instead, we have to rely on PROC Sql to insert all unique values of `district_res` into a macro variable.

```SAS
proc sql noprint;
select distinct district_res
	into :district_res1-
	from df3
	where district_res ne '';
quit;

%put _user_;
```
The name of these macro variables are `&district_res1`, `&district_res2`,... all the way to that last unique value, which in this case is `&district_res9`. 

Finally we can use a macro code to run a PROC Print for each unique value of `district_res`. Note that creating macro variables using Proc SQL automatically created a macro variable named `&sqlobs` that counts how many macro variables were created. 

```SAS
%macro print_bydistrict (start, stop, hospital_want='Port Hospital');
	%do num=&start %to &stop;
	%let district_res_num = %sysfunc(putn(&num, 1.));
	title "First five line listings for district: &&district_res&district_res_num";
	proc print data = df3 (obs=5);
	where district_res = "&&district_res&district_res_num" and hospital = &hospital_want;
	var case_id date_onset gender lat lon;
	run;
	%end;
%mend;

%print_bydistrict(1,&sqlobs)
```

This program is difficult to read and understand visually. It runs a do loop from a numeric parameter: `start` to another numeric parameter `stop`. In this case, when we call this macro, we are running from the values `1` to `&district_count`, which is 9. 

The program merges this value with the macro variable name `&district_res`, to become `district_res1`, `district_res2`, etc. So in the first loop, the macro processor sees this: `where district_res = "&district_res1`. Which then resolves into the first district of `Central I`. 


## Graphing Charts
When it comes to graphs and charts, both languages are equally difficult to learn and write. We've all had the experience where a simple change in axis values required hours of fine tuning. This is still generally the case. Below we will just produce a few examples of basic charts and graphs in each program. 

```SAS
/*create a table to count rows by district*/
proc sql;
create table district_det_summary as
select distinct district_det, count(*) as count
from df3
where case_def = 'Confirmed'
group by district_det;
quit;

/*create the chart using the summary table created above*/
proc sgplot data=district_det_summary;
	vbar district_det / response=count;
run;
```

In R, we use the popular ggplot package to create charts:
```r
case_counts_district <- surv %>% 
  group_by(district) %>% 
  summarize(ncases=n())

ggplot(data = case_counts_district, mapping = aes(
  x = district,
  y = ncases
)) + 
  geom_col()
```

Next we create histograms. The first histogram graphs the weight distribution of the cases. The second code chunk creates multiple panels of histograms, disaggregated by district of detection. 

```SAS
/*one histogram of all weights*/
proc sgplot data=df3;
	histogram wt__kg_;
run;

/*histogram panel by district*/
proc sgpanel data=df3;
	panelby district_det;
	histogram wt__kg_;
run;
```

R:
```r
# base R functions only
hist(df3$wt_kg)

# using ggplot
ggplot(data = df3, mapping = aes(
  x = wt_kg
  ))+
  geom_histogram()

/*histogram panel by district*/
df3 %>% 
ggplot(mapping = aes(
  x = wt_kg
)) + geom_histogram() + 
  facet_wrap(~district)
```

## Simple Statistics


## Conclusions



