
## 1.
# Read in and merge the 2010, 2011, 2012, 2013 & 2014 incidents from laser_incidents_2010-2014.xls file
# (assume all fields marked 'UNKN' as missing values)

#Most did some variation of this:
lasers2010 <- read_xls('data/laser_incidents_2010-2014.xls', sheet = 1, na = c('UNKN', 'na'))
lasers2011 <- read_xls('data/laser_incidents_2010-2014.xls', sheet = 2, na = 'UNKN')
lasers2012 <- read_xls('data/laser_incidents_2010-2014.xls', sheet = 3, na = 'UNKN')
lasers2013 <- read_xls('data/laser_incidents_2010-2014.xls', sheet = 4, na = 'UNKN')
lasers2014 <- read_xls('data/laser_incidents_2010-2014.xls', sheet = 5, na = 'UNKN')

#Making Aircraft ID column names consistent
names(lasers2011)[3] <- 'ACID'
names(lasers2012)[3] <- 'ACID'
names(lasers2013)[3] <- 'ACID'
names(lasers2014)[3] <- 'ACID'

lasers <- bind_rows(lasers2010, lasers2011, lasers2012, lasers2013, lasers2014)


#Simplest/Cleanest:
sheets <- excel_sheets("data/laser_incidents_2010-2014.xls")
df <- map_df(sheets, ~ read_excel(file, na = 'UNKN'))
# but problems with duplicate rows !!

#Another functionized approach:
read_sheets <- function(f) {
    sheets <- excel_sheets(f)
}
sheets <- read_sheets('data/laser_incidents_2010-2014.xls')
dfs <- c()

for (i in 1:length(sheets)) {
    dfs[[i]] <- read_excel('data/laser_incidents_2010-2014.xls', sheet = sheets[[i]])
}
col_names <- colnames(dfs[[1]])

for (i in 1:length(dfs)) {
    colnames(dfs[[i]]) <- col_names
}
main <- do.call('bind_rows', dfs)



#################################################################################################
## 2.
# Excluding missing/unknown values, how many distinct laser color combinations exist for 2010? 
# Order of colors does not matter, and assume 'and', 'or', and '/' separated colors are the same
# e.g. 'blue/green' == 'blue or green' == 'green and blue'

# correct answer is 17 (for 2010) - some did the work for all years - some did not account for green/blue == blue/green
# start with something like this to see what you are dealing with/need to cleanup
unique(lasers2010$COLOR)
#then manually count or clean up the repeats (many ways to do this, here's one)
colors2010 <- colors2010[colors2010 != 'unkn' & colors2010 != 'white/green' & colors2010 != 'blue/green' & colors2010 != 'green/ blue']

#several used stringr functions - not one of the available packages and not noted in the Rmd

##################################################################################################
## 3. 
# How many incidents are identified as blue (and only blue) in the combined dataset?
#correct answer is 296
# Almost everyone got this doing something like
nrow(lasers %>% filter(tolower(COLOR) == 'blue'))
# or
blue_rows <- main[grep('^blue$', main$COLOR),]
nrow(blue_rows)

###################################################################################################
## 4.
# In how many different cities are laser incidents reported? Be sure to look at `CITY` and not the airport (`MAJOR CITY`).

# Intent here was just to count the cities (1239) something like this
unique_city <- all_year_laser %>% 
    select(city) %>% 
    group_by(city) %>% 
    summarise(unique_cities = n_distinct(city))

#but several dug into a deeper analysis of the reported cities like 
#is Chicago/Aurora the same as Chicago/Lake of the Hills and is Dallas/Fort Worth the same as Dallas
#assumed boulder and boulder city are different

#bountiful/farmington == bountiful/framington
#champaign/urbana === champaigne/urbana
#fayetteville/springdale == fayetteville/\nspringdale 
# carlsbad == carlbad 
# greeley == greely 

#WHICH IS NOT WRONG


## 5.
# In what cities have *injuries* from lasers been reported?

# Everyone got this right - 0 cities
# some with more frustration than others

# some ways to see this:
laser_incidents$`Injury Reported` %>% 
    unique()
# [1] "NO"  "No"  NA    "*NO" "NO*"

# or

nrow(lasers %>% filter(tolower(`Injury Reported`) == 'yes'))

## 6. 
# How many helicopters have been targeted (where 'Type A/C' is HELO)? Create a dataframe of these called 'helo'.
# the purpose here was to create a subset of the data to explore; some tried to find unique helicopters 
# by examining the aircraft IDs -more precise language would have been more clear - "How many incidents targeted helicopters?"

# the expected code was something like:
helo <- lasers %>% filter(tolower(`TYPE A/C`) == 'helo')
nrow(helo)    #which found 366

#if you grepped for helo (better!!) you would have found 369

# TYPE                    n
# <chr>                 <int>
# 1               helo   366
# 2 helo (helo - helo)     1
# 3              helos     1
# 4        police helo     1


## 7.
# In what city did laser incidents with helicopters occur most?
# Most got Las Vegas here with 27 incidents
helo_cities <- table(helo$CITY)
print(head(sort(helo_cities, decreasing=TRUE), 3))

# or

helo_cities <- helo %>% 
    group_by(`CITY`) %>% 
    summarise(totals = n()) 
helo_cities %>% arrange(desc(totals))

## 8.
# For the combined 2010-2014 dataset, what are the maximum, minimum, and average altitudes reported?
# intended this to be a straightforward application of max(), min(), and mean() functions
# [1] "The minimum altitude is  0"
# [1] "The maximum altitude is  175000"
# [1] "The mean altitude is  5356.86820359281"

# with code like this
df %>% 
    summarise(mean_alt = mean(as.numeric(ALT), na.rm = TRUE),
              min_alt = min(as.numeric(ALT), na.rm = TRUE),
              max_alt = max(as.numeric(ALT), na.rm = TRUE))

# or

max(as.numeric(df_lasers$altitude), na.rm = T)
min(as.numeric(df_lasers$altitude), na.rm = T)
mean(as.numeric(df_lasers$altitude), na.rm = T)

# but there was some questioning of the data (particularly 175,000 feet)
# and one person dug deeper:

## Found a lot of records with a fl in front of the altitude value and found that
## it stands for "Flight Level", which is the altitude * 100. I have to pull out
## those values and multiply them by 100 before adding them back into the dataframe
## to get the correct average.

# and did this:
## Filtering out the fl values, and also f220, which I'm going to take an educated guess was a type and should be fl220.

fl_df <- all_year_laser %>% 
    filter(grepl(paste("fl"), alt) |
               grepl(paste("f220"), alt))

fl_df$alt <- sub("fl", "", fl_df$alt)
fl_df$alt <- sub("f", "", fl_df$alt)
fl_df$alt <- as.numeric(fl_df$alt)


fl_df <- fl_df %>% 
    mutate(alt = alt*100)

## Now I can filter out the numeric values from the original altitude list

alt_numbers <- all_year_laser %>%
    filter(alt != "unkn" &
               alt != "sfc" &
               !grepl(paste("fl"), alt) &
               !grepl(paste("f"), alt))

alt_numbers$alt <- as.numeric(alt_numbers$alt)

## Merging the newly converted flight level list with the filtered numeric altitude list  

all_alt <- rbind(fl_df, alt_numbers)

alt_summary <- all_alt %>% 
    summarise(max_alt = max(alt),
              min_alt = min(alt),
              avg_alt = mean(alt))

# > alt_summary
# # A tibble: 1 x 3
# max_alt min_alt avg_alt
# <dbl>   <dbl>   <dbl>
#     1  175000       0 5974.32

## 9. 
# Create a histogram of the altitudes using ggplot2. Do the min/max/mean appear in line with  your calculations?
#Most of you got this, and almost everyone followed the instruction to use ggplot. This one was particularly nice:
ggplot(df, aes(x=ALT))+
    geom_histogram() +
    stat_bin(binwidth = 50) +
    geom_vline(aes(xintercept = mean_ALT),col='red',size=2) +
    geom_text(aes(label="mean",y=0,x=mean_ALT), size = 3, col='orange', vjust=1) +
    geom_text(aes(label="min",y=0,x=min_ALT), size = 3, col='blue', vjust=-1) +
    geom_text(aes(label="max",y=0,x=max_ALT), size = 3, col='red', vjust=1)


## 10.
# Add a column (ALT_Category) to the dataframe based on the aircraft's altitude  
# there were several approaches to this

# assigning category after slicing out the relevant rows:
laser_incidents$ALT_Category[laser_incidents$ALT <= 2000] <- 'Low'
laser_incidents$ALT_Category[(laser_incidents$ALT > 2000) & (laser_incidents$ALT <= 8000)] <- 'Medium'
laser_incidents$ALT_Category[laser_incidents$ALT > 8000] <- 'High'

#with case_when():
data <- data %>% 
    mutate(
        ALT_Category = case_when(
            as.numeric(alt_fixed) > 8000 ~ "High",
            as.numeric(alt_fixed) > 2000 ~ "Medium", 
            T ~ "Low"))

#with ifelse statements:
lasers %<>% mutate(ALT_Category = as.factor(ifelse(ALT %in% 0:2000, 'Low',
                                                   ifelse(ALT %in% 2001:8000, 'Medium','High'))))

# using the cut() function to create intervals:
main_alt$ALT_Category<- cut(main_alt$ALT, breaks=c(0, 2000, 7999, Inf), labels=c('Low', 'Medium', 'High'))

## 11.
# Create a plot showing DATE on the x-axis and ALT on the y-axis; color the points on this plot according to the ALT_Category
# most of you had no problem with this
ggplot(df_cat, aes(x=DATE, y=ALT, color=ALT_Category)) +
    geom_point()


