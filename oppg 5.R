#oppgave 5
#Download the Excel file "GCIPrawdatatest.xlsx".
#I have taken away data from Norway 1980-1990 as it was faulty
#Save it in an easily accessible location, such as a folder on your Desktop or in your personal folder.
install.packages("tidyverse")
install.packages("readxl")
install.packages("ineq")
library(tidyverse)  
library(readxl) 
library(ineq)
# Set your working directory to the correct folder.
# Insert your file path for 'YOURFILEPATH'.
setwd("~/Desktop/sok-2008/SOK-2008-2022-oppgave1")

decile_data <- read_excel("GCIPrawdatatest.xlsx", skip = 2)  
#The data is now in a 'tibble' (like a spreadsheet for R). Let's use the head function to look at the first few rows:
head(decile_data) 
#Now we use loops to complete our task. We begin by creating a new variable in our dataset, gini, which we initially set to 0 for all country-year combinations.
decile_data$gini <- 0
#Now we use a loop to run through all the rows in our dataset (country-year combinations). For each row we will repeat the Gini coefficient calculation from R walk-through 5.4 and save the resulting value in the gini variable we created.
#The function that calculates Gini coefficients from a vector of numbers is called Gini,and we apply it to the income deciles
# Give us the number of rows in decile_data
noc <- nrow(decile_data)

for (i in seq(1, noc)){
  # Go to Row I to get the decile data
  decs_i <- unlist(decile_data[i, 3:12])
  decile_data$gini[i] <- Gini(decs_i)
}
#With this code, we calculated 4,799 Gini coefficients without having to manually run the same command 4,799 times. We now look at some summary measures for the gini variable.
#First we use the subset function to select Nordic countries and save their data as temp_data. As an example, we have chosen four anglophone countries: the UK, the US, Ireland, and Australia.
temp_data <- subset(
  decile_data, Country %in% c("United States","Sweden","Finland","Norway", 
                              "Denmark"))
#Now we plot the data using ggplot.

ggplot(temp_data, 
       aes(x = Year, y = gini, color = Country)) +
  geom_line(size = 1) +
  theme_bw() +
  ylab("Gini") +
  ggtitle("Gini coefficients for Nordic countries")



#oppgave 6
install.packages("hrbrthemes")
install.packages("cowplot")
library(PxWebApiData)
library(tidyverse)
library(gglorenz)
library(hrbrthemes)
library(cowplot)
library(janitor)


variables <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                     returnMetaFrames = TRUE)
names(variables)


values <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                  returnMetaData = TRUE)

values[[1]]$values
#Inntekt før/etter skatt
values[[2]]$values # 00 = Samlet inntekt, 00S=Inntekt etter skatt
#Desiler
values[[3]]$values
#Statistikkvariabel
values[[4]]$values
#År
values[[5]]$values

dt <- ApiData("https://data.ssb.no/api/v0/en/table/12558/",
              Tid =c("2005","2020"), # Velg årene 2005 og 2020
              Desiler=c("01", "02", "03" ,"04", "05", "06" ,"07", "08" ,"09", "10"), #Vi velger alle desiler
              InntektSkatt="00", #Vi velger samlet inntekt
              ContentsCode="VerdiDesil", #Velger den høyeste verdien i desilen
              Region=c("5401","1902")) #Tromsø endret kommunenummer i 2020


dt <- dt %>% 
  as.tibble(dt)

dt <- dt %>% 
  clean_names()

T2005 <- dt %>% 
  filter(x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$year == "2005")

T2020 <- dt %>%   
  filter(x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$year == "2020")

m1 <- ggplot(T2005, aes(T2005$x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$value)) +
  stat_lorenz(desc = FALSE) +
  geom_abline(linetype = "dotted") +
  theme_bw() +
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  labs(x = "Befolkning prosent",
       y = "Inntekt prosent",
       title = "2005 Lorenzkurve") +
  annotate_ineq(T2005$x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$value)


m2 <- ggplot(T2020, aes(T2020$x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$value)) +
  stat_lorenz(desc = FALSE) +
  geom_abline(linetype = "dotted") +
  theme_bw() +
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  labs(x = "Befolkning prosent",
       y = "Inntekt prosent",
       title = "2020 Lorenzkurve") +
  annotate_ineq(T2020$x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$value)

m1

m2


