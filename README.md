# SAS vs. R for data analysis

Moving from a SAS environment to the R programming language requires a paradigm change in thinking. I hope that this documents helps you in deciding whether to stay with SAS or move to R. This document is written for the intermediate SAS epidemiologist who is exploring the use of R.

### Difference in approach

The biggest difference between the two for data analysis is that R is a full programming language and works with numbers, lists, matrices, and tables. R treats these as objects, and you write functions to change these objects. There are often many ways to achieve your intended result, and that flexibility may initially be overwhelming. In SAS, you primarily work with data tables and manipulate data through the data step, which steps through the data row by row. 

This difference in changing or mutating an object as a whole, vs. stepping through the data row by row means the functions you use in R are more abstract, especially when used in combination with other functions. Likewise in SAS, you use procedure steps that guide you in generating reports and summarizing the data. In R, the reporting functions are more basic, giving you the flexibility to change the inputs, but which comes with a higher learning curve. 

### A quick note about packages

The R programming language is very extensible, meaning that just like many browsers, you can download and install extensions that extend the functionality beyond the base R language. We will be using many of the popular packages in our lessons below. The benefits of using packages is ease of use. They make the code easier to read. Many end users rely heavily on these packages in day to day work. The negative is that these packages may hide much of the complexities of the underlying programming. 

Data problems can be solved without any packages, using only base R functions that come with the standard install, but that requires in-depth knowledge of base R functions (as you will see in the looping function below). Len Greski showcases a [good comparison](https://github.com/lgreski/datasciencectacontent/blob/master/markdown/exampleSortRvsSAS.md) of using base R syntax to simply sort a variable.  


## Code comparison between SAS and R

Let's say I want to take a line listing, and number each patient 1, 2, 3..., grouped by day, i.e. the numbers restart for each new day. This could be useful if, for instance, you later want to find all the first, or third patient that presented symptoms for a given day. 

This is easy in SAS because the data step can count row by row. SAS would create a hidden column (named first) that writes the value `1` if the date column is different from the preceding row, thus specifying a new day. It writes `0` elsewhere. If the value is `1` (meaning a new day), then it resets the count to `0`. Stepping through the data row by row is very intuitive and that is how we would do it on pen and paper. But it can get complicated fast if you need to group by multiple columns. 

You can copy the code below directly into your SAS editor and run it.

```         
/*set recommended options*/
options compress=yes;
options nofmterr;
/*Creating relative path to data folders*/
%let a=%sysget(SAS_EXECFILEPATH);
%let b=%sysget(SAS_EXECFILENAME);
%put &a;
%put &b;
%let folder= %sysfunc(tranwrd(&a,&b,));
%put &folder;

/*import data file*/
proc import datafile = "&folder\surveillance_linelist_20141201.csv" dbms=csv out=df replace; 
run;

/*Using by statement requires sorting*/
proc sort data=df;
	by onset_date;
run;

data df2;
	set df;
	where onset_date ne .;
	by onset_date;
	retain running_count 0;
	if first.onset_date = 1 then running_count=0;
	running_count = running_count + 1;
run;
```

As a reminder, this data step works by creating two new columns, one visible called `running_count`, and another hidden column called `first`. Using the retain option, SAS retains the row count when the PDV re-initializes, before adding the 1 for the next row and outputting the result in the `running_count` column. SAS resets the `running_count` column when encountering a `1` in the `first` column, which indicates that the date has changed compared to the previous row (which is why sorting is required).

In R, the functions you write apply to the entire dataset. 

```    
# install helper packages
pacman::p_load(
  rio,  here,  dplyr,  janitor,  lubridate,  epikit
  )

# import data set
df <- import(here("surveillance_linelist_20141201.csv"))

#here is the code to create a running count column
df2 <- df %>% clean_names() %>% 
  group_by(onset.date) %>% 
  mutate(count = row_number())
```

Behind the scenes, R is translating this function into the C programming language (which better mirrors how a computer calculates sums at a machine level) and uses specialized algorithms to calculate the output. While R is not any faster than SAS at counting the rows (the SAS program has been heavily optimized over the decades), the SAS paradigm of stepping through the data set row by row begins to show limitations when there are multiple groups. 

### Grouping by multiple variables

Let's say I want to group the counts by day and region, so a count of `1` means that this was the first person to present with symptoms on this day in this particular region. In SAS, grouping by multiple variables requires the same number of lines of code, but the readability decreases. 
```
proc sort data=df;
	by onset_date adm3_name_det /* this is the region variable */;
run;

data df_onset_region_count;
	set df;
	where onset_date ne .;
	by onset_date adm3_name_det;
	retain running_count 0;
	if first.adm3_name_det = 1 then running_count=0;
	running_count = running_count + 1;
run;
```


### A quick note about looping

You can always force R to loop through each row, counting the rows by group, similar to the SAS data step. But in many ways, understanding this code is more difficult than in SAS. And looping through each row in R is hundreds or even thousands of times slower than using functions that utilizing vectorized R functions. 

```
# Get unique groups of dates
unique_groups <- unique(df$onset.date)

# Add a new column for row numbers
df$row_number <- NA

# Find the row numbers for each unique date, and then assign row numbers
for (i in unique_groups) {
  group_rows <- which(df$onset.date == i)
  df$row_number[group_rows] <- seq_along(group_rows)
}
```
Hopefully this introductory example gives you a good idea of the major differences in approach when coding in SAS vs. R. The section below gives side-by-side examples of SAS and R code to complete the same data processing steps. 


## Comparing the Data Step

This and the next few sections showcase the same data manipulation steps in both SAS and R. These are designed to be inputted directly in your SAS and R console windows. The goal is to give you a peek into how SAS and R code can be different, but also alike. 

As you begin your journey in the R language, use these sections as a quick-start reference. We hope that the functions we show below represent the most commonly used functions in your day-to-day data analysis. 

First we manipulate the timedata SAS table using commonly used SAS functions. 
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
    /* SAS does not have a direct equivalent for this, you may need to write a custom format or use PROC FORMAT */

    /* Date difference */
    diff = date_report - date_onset;
    
    /* Location movement */
    if district_det ne district_res then moved = 1;
    else moved = 0;
    
    /* Keeping only confirmed cases */
    if case_def ne 'Confirmed' then delete;
    
    /* Selecting columns - SAS automatically retains all columns unless explicitly dropped */
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
  mutate(age_cat = age_categories(age_years, breakers = c(0, 5, 10, 15, 20, 30, 40, 50))) %>% 
  
  #date diff
  mutate(diff = date_report - date_onset) %>% 
  
  #location
  mutate(moved = district_det != district_res) %>% 
  
  #filter rows
  filter(case_def == 'Confirmed')
```
At the basic level, both codes look roughly similar, and that was likely the intention with the dplyr package, which hides much of the programming complexity behind easy to use functions. 

## Comparing PROC tables

The power of SAS lies in the numerous reports that are able to be easily generated by simple proc statements. We try to re-create the most commonly used proc tables. 

### PROC Contents and PROC Summary
These procedures give you an initial idea of the makeup of the dataset. 
```
/*general data table information and list of variables */
proc contents data=df3;
run;

/*summary statistics of all numeric variables*/
proc summary data=df3 print;
var _numeric_;
run;

```
Here are similar functions in R:
```
str(df3) # shows type of each variable, and sample data
summary(df3) # summary statistics for numeric variables
```
### Proc Freq
Proc Freq is one of the most powerful procs that is commonly used. It is very flexible 

```
df3 %>%
  group_by(hospital) %>%
  summarise(
    freq_fever = list(table(fever)),
    freq_chills = list(table(chills)),
    freq_cough = list(table(cough)),
    freq_aches = list(table(aches)),
    freq_vomit = list(table(vomit))
  )
```






