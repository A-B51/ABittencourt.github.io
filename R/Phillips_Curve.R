#######################################################################
# Imports data from: Swiss National Bank (SNB) and the Federal Statistical Office of Switzerland
# Input: Downloaded Excel Spreadsheets
# Output: A plotted Phillips Curve for the Swiss Economy (2000 until 2019)
# St. Gallen, 2020
#######################################################################

#SETUP---------
# those are the libraries we normally work with. For this project we arent using all of them, but is always good to have them loaded.
library(RCurl)
library(rvest)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(tidyverse)
library(readxl)
library(kableExtra)
library(gridExtra)
library(rio)


#INFLATION--------------
#Data Import----------
# the link copied in the first argument gives all the information avaiable on inflation from the SNB in the form of an excel spreadsheet.
download_xml("https://data.snb.ch/json/table/getFile?fileId=6971f48f34d864653027b63434b746efdbb77371ca0a74987c59e90db9d2f604&pageViewTime=20200316_164217&lang=en",
             # save it where you want in your computer. It is useful to set a working directory for your project.
             ## Before the ".xlsx" you define the name of the file ("yourpath/filename.xlsx). We decided to name it "Inflation".
             file = "C:/Users/andre/Desktop/Bachelor/2. Semester (FS 2020/Monetary Economics/Paper/Inflation.xlsx",
             quiet = FALSE,
             mode = "wb")

# Now we want to parse the file. Make sure to input the right adress for the file, as well as file name!
data_in <- read_excel("C:/Users/andre/Desktop/Bachelor/2. Semester (FS 2020/Monetary Economics/Paper/Inflation.xlsx",
                      # it is important to parse the columns as "text" (<chr> data), so we make sure none of the data goes missing
                      col_types = "text",
                      # by default, the first 14 rows of any SNB Excel spreadsheet is a bunch of "bla-bla-bla". The data part with
                      ## its real columns specifications begins at row 15 (R15). The columns of interest, that is, where the data
                      ### is, are from Column 1 (C1) to Column 5 (C5). So we want to read from row 15, column 1, until row 1000
                      #### (the document is actually not that long, but then we make sure we got all the data), Column 5, so we get: "R15C1:R1000C5".
                      range = "R15C1:R1000C5")
data_in
view(data_in)


#Cleaning-------------
# take out the first row (it says nothing) and rename "Overview" to "Period"
data_in_sliced <- slice(data_in, -c(1)) %>%
  rename("Period" = "Overview")

# correcting column values - first the inflations
view(data_in_sliced)
# to see the data types from the columns run the function below
sapply(data_in_sliced, mode)
# now we change the values from <chr> to >dbl>, so we can run calculations with the columns
data_in_sliced$`SNB - Core inflation, trimmed mean` <- as.double(data_in_sliced$`SNB - Core inflation, trimmed mean`)
data_in_sliced$`SFSO - Core inflation 1` <- as.double(data_in_sliced$`SFSO - Core inflation 1`)
data_in_sliced$`SFSO - Core inflation 2` <- as.double(data_in_sliced$`SFSO - Core inflation 2`)
data_in_sliced$`SFSO - Inflation according to the national consumer price index` <- as.double(data_in_sliced$`SFSO - Inflation according to the national consumer price index`)

# getting rid of the columns which are completely filled with empty values (NA) and adding an id column to merge it with years
data_in_noNA <- drop_na(data_in_sliced, Period) %>%
  rownames_to_column(var = "id")
view(data_in_noNA)

# it would be useful to have a "year" column. So we replace all the months with nothing (empty strings "")
year_column <- select(data_in_noNA, Period)%>%
  rename("Year" = "Period")
year_column$Year <- str_replace(year_column$Year, pattern = "-01", replacement = "") %>%
  str_replace(pattern = "-02", replacement = "")%>%
  str_replace(pattern = "-03", replacement = "")%>%
  str_replace(pattern = "-04", replacement = "")%>%
  str_replace(pattern = "-05", replacement = "")%>%
  str_replace(pattern = "-06", replacement = "")%>%
  str_replace(pattern = "-07", replacement = "")%>%
  str_replace(pattern = "-08", replacement = "")%>%
  str_replace(pattern = "-09", replacement = "")%>%
  str_replace(pattern = "-10", replacement = "")%>%
  str_replace(pattern = "-11", replacement = "")%>%
  str_replace(pattern = "-12", replacement = "")

# adding also here an id column to merge later with the rest of the data
year_column <- year_column %>% rownames_to_column(var = "id")

# now we merge it to the rest of our data
data_in_long <- merge(year_column, data_in_noNA, by = "id", sort = FALSE) %>%
  select(-c("id"))
view(data_in_long)
head(data_in_long)

# Spreading the dataset: we quickly group each column by year. We have thus all the inflation data on Switzerland, by years
in_year_period <- select(data_in_long, Year, Period)%>%
  group_by(Year) %>%
  mutate(id = row_number()) %>%
  spread(key = "Year", value = "Period")

in_year_CoreMean <- select(data_in_long, Year, `SNB - Core inflation, trimmed mean`)%>%
  group_by(Year) %>%
  mutate(month_expept_1983 = row_number())%>%
  spread(key = "Year", value = "SNB - Core inflation, trimmed mean")%>%
  rename("Month (exept for 1983)" = "month_expept_1983")

in_year_ConPriceIndex <- select(data_in_long, Year, `SFSO - Inflation according to the national consumer price index`)%>%
  group_by(Year) %>%
  mutate(month_expept_1983 = row_number())%>%
  spread(key = "Year", value = "SFSO - Inflation according to the national consumer price index")%>%
  rename("Month (exept for 1983)" = "month_expept_1983")

in_year_Core1 <- select(data_in_long, Year, `SFSO - Core inflation 1`)%>%
  group_by(Year) %>%
  mutate(month_expept_1983 = row_number())%>%
  spread(key = "Year", value = "SFSO - Core inflation 1")%>%
  rename("Month (exept for 1983)" = "month_expept_1983")

in_year_Core2 <- select(data_in_long, Year, `SFSO - Core inflation 2`)%>%
  group_by(Year) %>%
  mutate(month_expept_1983 = row_number())%>%
  spread(key = "Year", value = "SFSO - Core inflation 2")%>%
  rename("Month (exept for 1983)" = "month_expept_1983")


#Special case: Data on CPI--------------
# the SNB also have additional years on CPI rates in a sepparate dataset. Lets also import and clean it repeating all the steps above:

download_xml("https://data.snb.ch/json/table/getFile?fileId=6aadd54260f8f5f384358e6e8eb142f8621f686b5bb2bd8b285e19542bb85944&pageViewTime=20200328_084516&lang=en",
             # save it where you want in your computer. It is useful to set a working directory for your project.
             ## Before the ".xlsx" you define the name of the file ("yourpath/filename.xlsx).
             file = "C:/Users/andre/Desktop/Bachelor/2. Semester (FS 2020/Monetary Economics/Paper/CPI.xlsx",
             quiet = FALSE,
             mode = "wb")

# Now we want to parse the file. Make sure to input the right adress for the file, as well as file name!
data_CPI <- read_excel("C:/Users/andre/Desktop/Bachelor/2. Semester (FS 2020/Monetary Economics/Paper/CPI.xlsx",
                       # it is important to parse the columns as "text" (<chr> data), so we make sure none of the data goes missing
                       col_types = "text",
                       range = "R16C1:R1200C2")
data_CPI
view(data_CPI)


#Cleaning-------------
# rename "...1 " to "Period"
data_CPI_sliced <- rename(data_CPI, "Period" = "...1")

# correcting column values - first the inflations
view(data_CPI_sliced)

# getting rid of the columns which are completely filled with empty values (NA) and adding an id column to merge it with years
data_CPI_noNA <- drop_na(data_CPI_sliced, Period) %>%
  rownames_to_column(var = "id")
view(data_CPI_noNA)

# it would be useful to have a "year" column. So we replace all the months with nothing (empty strings "")
year_column_CPI <- select(data_CPI_noNA, Period)%>%
  rename("Year" = "Period")
year_column_CPI$Year <- str_replace(year_column_CPI$Year, pattern = "-01", replacement = "") %>%
  str_replace(pattern = "-02", replacement = "")%>%
  str_replace(pattern = "-03", replacement = "")%>%
  str_replace(pattern = "-04", replacement = "")%>%
  str_replace(pattern = "-05", replacement = "")%>%
  str_replace(pattern = "-06", replacement = "")%>%
  str_replace(pattern = "-07", replacement = "")%>%
  str_replace(pattern = "-08", replacement = "")%>%
  str_replace(pattern = "-09", replacement = "")%>%
  str_replace(pattern = "-10", replacement = "")%>%
  str_replace(pattern = "-11", replacement = "")%>%
  str_replace(pattern = "-12", replacement = "")

# adding also here an id column to merge later with the rest of the data
year_column_CPI <- year_column_CPI %>% rownames_to_column(var = "id")

# now we merge it to the rest of our data
data_CPI_long <- merge(year_column_CPI, data_CPI_noNA, by = "id", sort = FALSE) %>%
  select(-c("id"))
view(data_CPI_long)
head(data_CPI_long)

# correct the format from <chr> to <dbl>
data_CPI_long$Year <- as.double(data_CPI_long$Year)
data_CPI_long$CPI <- as.double(data_CPI_long$CPI)

# Spreading the dataset: we quickly group each column by year. We have thus all the CPI data on Switzerland, by years
CPI_years <- select(data_CPI_long, Year, CPI)%>%
  group_by(Year) %>%
  mutate(id = row_number()) %>%
  spread(key = "Year", value = "CPI")



#UNEMPLOYMENT----
#Data Import------
# basically exatly what we did with the inflation data
download_xml("https://data.snb.ch/json/table/getFile?fileId=2d4c778e281855c59edbe1bed90e4181c896bfd90a01a2f9768f63b6652855d4&pageViewTime=20200316_164217&lang=en",
             file = "C:/Users/andre/Desktop/Bachelor/2. Semester (FS 2020/Monetary Economics/Paper/Unemployment.xlsx",
             quiet = FALSE,
             mode = "wb")

# Again, we want to parse the file. Make sure to input the right adress for the file, as well as file name! Another thing: since
## the data from admin.ch work with several sheets in excel, we have to change our function to read_excel_allsheets
data_un <- read_excel_allsheets("C:/Users/andre/Desktop/Bachelor/2. Semester (FS 2020/Monetary Economics/Paper/Unemployment.xlsx",
                                # it is important to parse the columns as "text" (<chr> data), so we make sure none of the data goes missing
                                col_types = "text",
                                # The "bla-bla-bla" for the labour market spreadsheets of the SNB stops at row 19 and we have 10 columns of interest. So we input: "R15C1:R1000C10".
                                range = "R20C1:R1000C10")
data_un
view(data_un)


#Cleaning------
# take out the first row (it says nothing) and rename "Overview" with "Period"
data_un_sliced <- slice(data_un, -c(1)) %>%
  rename("Period" = "Overview")
# correcting column values, as we did before
view(data_un_sliced)
sapply(data_un_sliced, mode)
data_un_sliced$`Workers affected by short-time working` <- as.double(data_un_sliced$`Workers affected by short-time working`)
data_un_sliced$`Registered unemployed - Total` <- as.double(data_un_sliced$`Registered unemployed - Total`)
data_un_sliced$`Registered unemployed - Seasonally adjusted` <- as.double(data_un_sliced$`Registered unemployed - Seasonally adjusted`)
data_un_sliced$`Jobless rate - Total` <- as.double(data_un_sliced$`Jobless rate - Total`)
data_un_sliced$`Jobless rate - Seasonally adjusted` <- as.double(data_un_sliced$`Jobless rate - Seasonally adjusted`)
data_un_sliced$`Notified job vacancies - Total` <- as.double(data_un_sliced$`Notified job vacancies - Total`)
data_un_sliced$`Notified job vacancies - Seasonally adjusted` <- as.double(data_un_sliced$`Notified job vacancies - Seasonally adjusted`)
data_un_sliced$`Registered job seekers` <- as.double(data_un_sliced$`Registered job seekers`)
data_un_sliced$`Labour force` <- as.double(data_un_sliced$`Labour force`)


# getting rid of the columns which are completely filled with empty values (NA) and adding an id column to merge it with years
data_un_noNA <- drop_na(data_un_sliced, Period) %>%
  rownames_to_column(var = "id")
view(data_un_noNA)

# it would be useful to have an "year" column. So we replace all the months by nothing
year_column_un <- select(data_un_noNA, Period)%>%
  rename("Year" = "Period")
year_column_un$Year <- str_replace(year_column_un$Year, pattern = "-01", replacement = "") %>%
  str_replace(pattern = "-02", replacement = "")%>%
  str_replace(pattern = "-03", replacement = "")%>%
  str_replace(pattern = "-04", replacement = "")%>%
  str_replace(pattern = "-05", replacement = "")%>%
  str_replace(pattern = "-06", replacement = "")%>%
  str_replace(pattern = "-07", replacement = "")%>%
  str_replace(pattern = "-08", replacement = "")%>%
  str_replace(pattern = "-09", replacement = "")%>%
  str_replace(pattern = "-10", replacement = "")%>%
  str_replace(pattern = "-11", replacement = "")%>%
  str_replace(pattern = "-12", replacement = "")

# adding also here an id column
year_column_un <- year_column_un %>% rownames_to_column(var = "id")

# now we merge it to our dataset
data_un_long <- merge(year_column_un, data_un_noNA, by = "id", sort = FALSE) %>%
  select(-c("id"))
view(data_un_long)
head(data_un_long)

# Spreading the dataset: we quickly group each column by year. We have thus all the unemployment data avaiable on Switzerland, by years.
un_year_period <- select(data_un_long, Year, Period)%>%
  group_by(Year) %>%
  mutate(id = row_number()) %>%
  spread(key = "Year", value = "Period")

un_year_workers_aff_st_working <- select(data_un_long, Year, `Workers affected by short-time working`)%>%
  group_by(Year) %>%
  mutate(id = row_number())%>%
  spread(key = "Year", value = "Workers affected by short-time working")%>%
  rename("Month" = "id")

un_year_reg_un_tot <- select(data_un_long, Year, `Registered unemployed - Total`)%>%
  group_by(Year) %>%
  mutate(id = row_number())%>%
  spread(key = "Year", value = "Registered unemployed - Total")%>%
  rename("Month" = "id")

un_year_reg_un_seas_adj <- select(data_un_long, Year, `Registered unemployed - Seasonally adjusted`)%>%
  group_by(Year) %>%
  mutate(id = row_number())%>%
  spread(key = "Year", value = "Registered unemployed - Seasonally adjusted")%>%
  rename("Month" = "id")

un_year_un_rate <- select(data_un_long, Year, `Jobless rate - Total`)%>%
  group_by(Year) %>%
  mutate(id = row_number())%>%
  spread(key = "Year", value = "Jobless rate - Total")%>%
  rename("Month" = "id")

un_year_un_rate_seas_adj <- select(data_un_long, Year, `Jobless rate - Seasonally adjusted`)%>%
  group_by(Year) %>%
  mutate(id = row_number())%>%
  spread(key = "Year", value = "Jobless rate - Seasonally adjusted")%>%
  rename("Month" = "id")

un_year_vacancies <- select(data_un_long, Year, `Notified job vacancies - Total`)%>%
  group_by(Year) %>%
  mutate(id = row_number())%>%
  spread(key = "Year", value = "Notified job vacancies - Total")%>%
  rename("Month" = "id")

un_year_vacancies_seas_adj <- select(data_un_long, Year, `Notified job vacancies - Seasonally adjusted`)%>%
  group_by(Year) %>%
  mutate(id = row_number())%>%
  spread(key = "Year", value = "Notified job vacancies - Seasonally adjusted")%>%
  rename("Month" = "id")

un_year_reg_job_seekers <- select(data_un_long, Year, `Registered job seekers`)%>%
  group_by(Year) %>%
  mutate(id = row_number())%>%
  spread(key = "Year", value = "Registered job seekers")%>%
  rename("Month" = "id")

un_year_labour_force <- select(data_un_long, Year, `Labour force`)%>%
  group_by(Year) %>%
  mutate(id = row_number())%>%
  spread(key = "Year", value = "Labour force")%>%
  rename("Month" = "id")


#WAGES----
#Data Import------
# The information in the SNB on wages is scarce. Thats why we are importing it from the Federal Statistical Office of Switzerland.
## The process is the same as we do with the data of the Central bank, with some modification
download_xml("https://www.bfs.admin.ch/bfsstatic/dam/assets/8046219/master",
             file = "C:/Users/andre/Desktop/Bachelor/2. Semester (FS 2020/Monetary Economics/Paper/Wages2.xlsx",
             quiet = FALSE,
             mode = "wb")

# Again, we want to parse the file. Make sure to input the right adress for the file, as well as file name! Since we have multiple spreadsheets,
## we need to us the funtion "import_list()" of the library "rio". We set the class to tibble and import the multiple sheets into one big spreadsheet
data_wages <- import_list("C:/Users/andre/Desktop/Bachelor/2. Semester (FS 2020/Monetary Economics/Paper/Wages2.xlsx",
                          rbind = TRUE,
                          setclass = "tbl",
                          rbind_fill = FALSE)


#Cleaning------
# lets have a look in the data:
view(data_wages)

# we see it is confusing. First, we give the columns the names we want and drop the columns we dont need:
data_wages_cols <- rename(data_wages, "Nominal Wages (NW): Index  (basis 1939 = 100)" = "...2")%>%
  rename("Growth Rate Nominal Wages" = "...5")%>%
  rename("Variation compared with previous year (CPI)" = "...8")%>%
  rename("Real Wages (RW): Index  (basis 1939 = 100)" = "...9")%>%
  rename("Growth Rate Real Wages" = "...12")%>%
  rename("Year" = "T 39 Evolution of nominal wages, consumer prices and real wages, 2010-2018 (3/3)")%>%
  select(-c(...3,...4,...6,...7,...10,...11,...13,...14, `_file`))

# Now we drop all columns and rows with some kind of NA value:
data_wages_noNA <- drop_na(data_wages_cols)

# we see the order of the years is not chronological. we fix that and also drop the first row that doesnt have values:
data_wages_long <- arrange(data_wages_noNA, Year) %>%
  slice(-c(1))

# the column values are in <chr>. We put it in <dbl>
data_wages_long$Year <- as.double(data_wages_long$Year)
data_wages_long$`Nominal Wages (NW): Index  (basis 1939 = 100)` <- as.double(data_wages_long$`Nominal Wages (NW): Index  (basis 1939 = 100)`)
data_wages_long$`Growth Rate Nominal Wages` <- as.double(data_wages_long$`Growth Rate Nominal Wages`)
data_wages_long$`Variation compared with previous year (CPI)` <- as.double(data_wages_long$`Variation compared with previous year (CPI)`)
data_wages_long$`Real Wages (RW): Index  (basis 1939 = 100)` <- as.double(data_wages_long$`Real Wages (RW): Index  (basis 1939 = 100)`)
data_wages_long$`Growth Rate Real Wages` <- as.double(data_wages_long$`Growth Rate Real Wages`)

# Spreading and separating nominal growth and real growth
wages_year_nom <- select(data_wages_long, Year, `Growth Rate Nominal Wages`)%>%
  spread(key = "Year", value = "Growth Rate Nominal Wages")

wages_year_real <- select(data_wages_long, Year, `Growth Rate Real Wages`)%>%
  spread(key = "Year", value = "Growth Rate Real Wages")

#PLOTTING---------
# GRAPH 1: CPI x Unemployment Rate-------------
## We use here consumer prices as inflation measure and the unemployment rate (people registered as unemplyed)
# Preparing the data to be ploted from the last year avaiable
CPI_years
un_year_un_rate
# lets check the lenghts of both datasets
length(un_year_un_rate)
length(CPI_years)
# select the latest column -1 (we dont have the complete data of 2020, which is the last column),
## lets call it "a" for inflation and "b" for unemplyment (2019)
a_G1 <- length(CPI_years)-1
# lets have a look on the dataset for the CPI and Unemployment:
head(CPI_years)
head(un_year_un_rate)
# we notice that the unemployment is avaiable from 1948 on. Therefore,
## we select from 1948 forward:
CPI_48_to_19 <- select(CPI_years, `1948`:a_G1)

# we repeat it with unemployment:
b_G1 <- length(un_year_un_rate)-1
head(un_year_un_rate)
# we select the data for unemployment from 1948 forward:
unemployment_48_to_19 <- select(un_year_un_rate, `1948`:b_G1)

# compute the means by year, so we plot the data by years, not months. Additionally we add an id column
unemployment_48_to_19 <- summarise_all(unemployment_48_to_19, list(mean = mean))%>%
  rownames_to_column(var = "id")
CPI_48_to_19 <- summarise_all(CPI_48_to_19, list(mean = mean))%>%
  rownames_to_column(var = "id")

# make the data longer to merge. Notice: the pivot_longer function respects the same syntax as the "select" one
unemployment_48_to_19_2_merge <- pivot_longer(unemployment_48_to_19, c( `1948_mean`:`2019_mean`),
                                              names_to = "Year",
                                              values_to = "Unemplyment Rate")%>%
  select(-c(id))

CPI_48_to_19_2_merge <- pivot_longer(CPI_48_to_19, c( `1948_mean`:`2019_mean`),
                                     names_to = "Year",
                                     values_to = "Inflation Rate (CPI)")%>%
  select(-c(id))

# merging the data
in_un_48_to_19 <- merge(CPI_48_to_19_2_merge, unemployment_48_to_19_2_merge,
                        by = "Year",
                        sort = FALSE)

# correct Year Column: we change the names for the years and also put it in a <dbl> format to plot the graph
in_un_48_to_19$Year <- str_replace(in_un_48_to_19$Year, pattern = "_mean", replacement = "")%>%
  as.double(in_un_48_to_19$Year)

# do a regression with the data so we can annotate that later on the regression line in the graph
ols <- lm(in_un_48_to_19$`Inflation Rate (CPI)` ~ in_un_48_to_19$`Unemplyment Rate`, in_un_48_to_19)
# We are mainly interested in the estimators for the intercept ("beta hat zero") and for the unemplyment rate ("beta hat one")
summary(ols)

# plotting data: here you can design the graph as you like. For informations in plotting: "https://ggplot2.tidyverse.org/reference/index.html"
GRAPH1 <- ggplot(in_un_48_to_19, aes(y = in_un_48_to_19$`Inflation Rate (CPI)`, x = in_un_48_to_19$`Unemplyment Rate`))+
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dotted",
             size = 0.1)+
  annotate("text", x= 3.3, y= 2, label= "Phillips Curve", size = 3)+
  annotate("text", x= 1.65, y= 2.55, label= "Regression: y = 3.27 - 0.71x", size = 2, color = "red")+
  geom_path(aes(color = Year, group = NA), size = 0.3)+
  geom_point(size = 1.5, shape = "diamond", show.legend = TRUE)+
  labs(x = "Unemployment Rate (in %)",
       y = "Inflation (CPI) (in %)",
       title = "Phillips Curve in CH:
       CPI x Unemployment
       (1948 to 2019)",
       label = TRUE,
       #       caption = "*Please note that the x axis begins at approximately 1.7;
       #      (the regression line has to go through appr. 1.7 x units
       #         to the left, intercepting thus the y axis at appr. 1.7).         ",
       size = 0.01)+
  geom_label(aes(label = Year),
             hjust = 0.5,
             vjust = 1.3,
             size = 1,
             check_overlap = TRUE,
             label.r = unit(0.5, "mm"),
             label.padding = unit(0.5, "mm"),
             label.size = 0.01)+
  geom_smooth(data = in_un_48_to_19, aes(y = in_un_48_to_19$`Inflation Rate (CPI)`, x = in_un_48_to_19$`Unemplyment Rate`),
              method = "lm",
              formula = y~x,
              color = "red",
              linetype = "11",
              size = 0.5,
              se = FALSE)
GRAPH1
# make sure to save the file as the name and format you like as follows: ggsave("my_graph.format")
ggsave("GRAPH1.pdf")


#GRAPH 2: Nominal Wages x Unemployment Rate-----------
## We use here consumer prices as inflation measure and the unemployment rate (people registered as unemplyed)
# Preparing the data to be ploted from the years 1948 until the latest information avaiable
wages_year_nom
un_year_un_rate
# lets check the lenghts of both datasets
length(un_year_un_rate)
length(wages_year_nom)
# select the latest column -2 for unemployment, since we have data on wages until 2018
b_G2 <- length(un_year_un_rate)-2
head(un_year_un_rate)
# we select the data for unemployment from 1948 forward:
unemployment_48_to_18 <- select(un_year_un_rate, `1948`:b_G2)

# we compute the same selection for nominal wages
wages_nom <- select(wages_year_nom, `1948`:`2018`)

# compute the means by year, so we plot the data by years, not months. Additionally we add an id column
unemployment_48_to_18 <- summarise_all(unemployment_48_to_18, list(mean = mean))%>%
  rownames_to_column(var = "id")
wages_nom <- rownames_to_column(wages_year_nom, var = "id")

# make the data longer to merge. Notice: the pivot_longer function respects the same syntax as the "select" one
unemployment_48_to_18_2_merge <- pivot_longer(unemployment_48_to_18, c( `1948_mean`:`2018_mean`),
                                              names_to = "Year",
                                              values_to = "Unemplyment Rate")%>%
  select(-c(id))


wages_nom_2_merge <- pivot_longer(wages_nom, c( `1943`:`2018`),
                                  names_to = "Year",
                                  values_to = "Nominal Wage Growth Rate") %>%
  select(-c(id)) %>%
  slice(-c(1:5))

# Before merging: correcting the values on the year column from unemployment and correcting data type for years in nominal wages
unemployment_48_to_18_2_merge$Year <- str_replace(unemployment_48_to_18_2_merge$Year, pattern = "_mean", replacement = "")%>%
  as.double(unemployment_10_to_18_2_merge$Year)
wages_nom_2_merge$Year <- as.double(wages_nom_2_merge$Year)

# merging the data
wage_nom_un <- merge(wages_nom_2_merge, unemployment_48_to_18_2_merge,
                     by = "Year",
                     sort = FALSE)


# do a regression with the data so we can annotate that later on the regression line in the graph
ols_G2 <- lm(wage_nom_un$`Nominal Wage Growth Rate` ~ wage_nom_un$`Unemplyment Rate`, wage_nom_un)
# We are mainly interested in the estimators for the intercept ("beta hat zero") and for the unemplyment rate ("beta hat one")
summary(ols_G2)

# plotting data: here you can design the graph as you like. For informations in plotting: "https://ggplot2.tidyverse.org/reference/index.html"
GRAPH2 <- ggplot(wage_nom_un, aes(y = wage_nom_un$`Nominal Wage Growth Rate`, x = wage_nom_un$`Unemplyment Rate`))+
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dotted",
             size = 0.1)+
  annotate("text", x= 3, y= 5, label= "Phillips Curve", size = 3)+
  annotate("text", x= 2.8, y= 3.3, label= "Regression: y = 5.6 - 1.2x", size = 2, color = "red")+
  geom_path(aes(color = Year, group = NA), size = 0.3)+
  geom_point(size = 1.5, shape = "diamond", show.legend = TRUE)+
  labs(x = "Unemployment Rate (in %)",
       y = "Nominal Wage Growth Rate (in %)",
       title = "Phillips Curve in CH:
       Nominal Wages x Unemployment
       (1948 to 2018)",
       label = TRUE,
       #       caption = "*Please note that the x axis begins at approximately 1.7;
       #      (the regression line has to go through appr. 1.7 x units
       #         to the left, intercepting thus the y axis at appr. 1.7).         ",
       size = 0.01)+
  geom_label(aes(label = Year),
             hjust = 0.5,
             vjust = 1.3,
             size = 1,
             check_overlap = TRUE,
             label.r = unit(0.5, "mm"),
             label.padding = unit(0.5, "mm"),
             label.size = 0.01)+
  geom_smooth(data = wage_nom_un, aes(y = wage_nom_un$`Nominal Wage Growth Rate`, x = wage_nom_un$`Unemplyment Rate`), method = "lm",
              formula = y~x,
              color = "red",
              linetype = "11",
              size = 0.5,
              se = FALSE)
GRAPH2
# make sure to save the file as the name and format you like as follows: ggsave("my_graph.format")
ggsave("GRAPH2.pdf")


#GRAPH 3: Real Wages x Unemployment Rate-----------
## We use here consumer prices as inflation measure and the unemployment rate (people registered as unemplyed)
# Preparing the data to be ploted from the years 1948 until the latest information avaiable
wages_year_real
unemployment_48_to_18_2_merge

# make the data longer to merge. Notice: the pivot_longer function respects the same syntax as the "select" one
wages_real_2_merge <- pivot_longer(wages_year_real, c( `1943`:`2018`),
                                   names_to = "Year",
                                   values_to = "Real Wage Growth Rate")%>%
  slice(-c(1:5))

# Before merging: correcting the values on the year column from unemployment and correcting data type for years in nominal wages
wages_real_2_merge$Year <- as.double(wages_real_2_merge$Year)

# merging the data
wage_real_un <- merge(wages_real_2_merge, unemployment_48_to_18_2_merge,
                      by = "Year",
                      sort = FALSE)


# do a regression with the data so we can annotate that later on the regression line in the graph
ols_G3 <- lm(wage_real_un$`Real Wage Growth Rate` ~ wage_real_un$`Unemplyment Rate`, wage_real_un)
# We are mainly interested in the estimators for the intercept ("beta hat zero") and for the unemplyment rate ("beta hat one")
summary(ols_G3)

# plotting data: here you can design the graph as you like. For informations in plotting: "https://ggplot2.tidyverse.org/reference/index.html"
GRAPH3 <- ggplot(wage_real_un, aes(y = wage_real_un$`Real Wage Growth Rate`, x = wage_real_un$`Unemplyment Rate`))+
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dotted",
             size = 0.1)+
  annotate("text", x= 3, y= 2, label= "Phillips Curve", size = 3)+
  annotate("text", x= 2.25, y= 0.7, label= "Regression: y = 2.2 - 0.5x", size = 2, color = "red")+
  geom_path(aes(color = Year, group = NA), size = 0.3)+
  geom_point(size = 1.5, shape = "diamond", show.legend = TRUE)+
  labs(x = "Unemployment Rate (in %)",
       y = "Real Wage Growth Rate (in %)",
       title = "Phillips Curve in CH:
       Real Wages x Unemployment
       (1948 to 2018)",
       label = TRUE,
       #       caption = "*Please note that the x axis begins at approximately 1.7;
       #      (the regression line has to go through appr. 1.7 x units
       #         to the left, intercepting thus the y axis at appr. 1.7).         ",
       size = 0.01)+
  geom_label(aes(label = Year),
             hjust = 0.5,
             vjust = 1.3,
             size = 1,
             check_overlap = TRUE,
             label.r = unit(0.5, "mm"),
             label.padding = unit(0.5, "mm"),
             label.size = 0.01)+
  geom_smooth(data = wage_real_un, aes(y = wage_real_un$`Real Wage Growth Rate`, x = wage_real_un$`Unemplyment Rate`), method = "lm",
              formula = y~x,
              color = "red",
              linetype = "11",
              size = 0.5,
              se = FALSE)
GRAPH3
# make sure to save the file as the name and format you like as follows: ggsave("my_graph.format")
ggsave("GRAPH3.pdf")


# GRAPH 4: Core Inflation Mean x Unemployment Rate / Seasonally adjusted-------------
## We use here the core inflation 2 as inflation measure and the unemployment rate (people registered as unemplyed), seasonally adjusted
# Preparing the data to be ploted from the years 1984 until the latest information avaiable
in_year_CoreMean
un_year_un_rate_seas_adj
# lets check the lenghts of both datasets
length(un_year_un_rate)
length(in_year_CoreMean)
# select the latest column -1 (we dont have the complete data of 2020, which is the last column),
## lets call it "a" for inflation and "b" for unemplyment (2019)
a_G4 <- length(in_year_Core2)-1
# lets have a look on the dataset for the Core inflation 2:
head(in_year_CoreMean)
# we notice that we have data avaiable from 1984 on
## we select from 1984 forward:
core_mean <- select(in_year_CoreMean, `1984`:a_G4)

# we repeat it with unemployment - seas. adj.:
b_G4 <- length(un_year_un_rate_seas_adj)-1
head(un_year_un_rate_seas_adj)
# we select the data for unemployment from 1984 forward:
un_seas_adj_84_to_19 <- select(un_year_un_rate, `1984`:b_G4)

# compute the means by year, so we plot the data by years, not months. Additionally we add an id column
un_seas_adj_84_to_19 <- summarise_all(un_seas_adj_84_to_19, list(mean = mean))%>%
  rownames_to_column(var = "id")

core_mean <- summarise_all(core_mean, list(mean = mean))%>%
  rownames_to_column(var = "id")

# make the data longer to merge. Notice: the pivot_longer function respects the same syntax as the "select" one
un_seas_adj_84_to_19_2_merge <- pivot_longer(un_seas_adj_84_to_19, c( `1984_mean`:`2019_mean`),
                                              names_to = "Year",
                                              values_to = "Unemplyment Rate - seas. adj.")%>%
  select(-c(id))

core_mean_2_merge <- pivot_longer(core_mean, c( `1984_mean`:`2019_mean`),
                                           names_to = "Year",
                                           values_to = "Inflation Rate (Core Mean)")%>%
  select(-c(id))

# merging the data
core_mean_un_seas_adj <- merge(core_mean_2_merge, un_seas_adj_84_to_19_2_merge,
                        by = "Year",
                        sort = FALSE)

# correct Year Column: we change the names for the years and also put it in a <dbl> format to plot the graph
core_mean_un_seas_adj$Year <- str_replace(core_mean_un_seas_adj$Year, pattern = "_mean", replacement = "")%>%
  as.double(core_mean_un_seas_adj$Year)

# do a regression with the data so we can annotate that later on the regression line in the graph
ols_G4 <- lm(core_mean_un_seas_adj$`Inflation Rate (Core Mean)` ~ core_mean_un_seas_adj$`Unemplyment Rate - seas. adj.`, core_mean_un_seas_adj)
# We are mainly interested in the estimators for the intercept ("beta hat zero") and for the unemplyment rate ("beta hat one")
summary(ols_G4)

# plotting data: here you can design the graph as you like. For informations in plotting: "https://ggplot2.tidyverse.org/reference/index.html"
GRAPH4 <- ggplot(core_mean_un_seas_adj, aes(y = core_mean_un_seas_adj$`Inflation Rate (Core Mean)`, x = core_mean_un_seas_adj$`Unemplyment Rate - seas. adj.`))+
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dotted",
             size = 0.1)+
  annotate("text", x= 3.3, y= 1.75, label= "Phillips Curve", size = 3)+
  annotate("text", x= 2, y= 2.49, label= "Regression: y = 3.5 - 0.7x", size = 2, color = "red")+
  geom_path(aes(color = Year, group = NA), size = 0.3)+
  geom_point(size = 1.5, shape = "diamond", show.legend = TRUE)+
  labs(x = "Unemployment Rate - seas. adj. (in %)",
       y = "Inflation (Core Mean) (in %)",
       title = "Phillips Curve in CH:
       Core Mean x Unemployment - seas. adj.
       (1984 to 2019)",
       label = TRUE,
       #       caption = "*Please note that the x axis begins at approximately 1.7;
       #      (the regression line has to go through appr. 1.7 x units
       #         to the left, intercepting thus the y axis at appr. 1.7).         ",
       size = 0.01)+
  geom_label(aes(label = Year),
             hjust = 0.5,
             vjust = 1.3,
             size = 1,
             check_overlap = TRUE,
             label.r = unit(0.5, "mm"),
             label.padding = unit(0.5, "mm"),
             label.size = 0.01)+
  geom_smooth(data = core_mean_un_seas_adj, aes(y = core_mean_un_seas_adj$`Inflation Rate (Core Mean)`, x = core_mean_un_seas_adj$`Unemplyment Rate - seas. adj.`),
              method = "lm",
              formula = y~x,
              color = "red",
              linetype = "11",
              size = 0.5,
              se = FALSE)
GRAPH4
# make sure to save the file as the name and format you like as follows: ggsave("my_graph.format")
ggsave("GRAPH4.pdf")
