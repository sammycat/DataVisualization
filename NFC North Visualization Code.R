# This is the code for NFC North Data Visualization. This code will do the following:
# 1) Read the applicable data from the NFC North Wikipedia page. 
# 2) Combine the data from the two applicable tables into one tidy table.
# 3) Create the intial graphs that will form the basis for the final visuals I will refine in 
#    Illustrator. 

###########################################################
#This section of the code reads individual tables from the wikipedia page. The "which" argument 
#specifies which order the tables appear on in the page. I want the 3rd and 4th tables. The package
#rvest is required to run this code. 

#install.packages('rvest')

library(rvest)

division_champs <-html("http://en.wikipedia.org/wiki/NFC_North")%>%
  html_table(fill=TRUE)%>%
  .[[3]] #table 3 only

division_wc <-html("http://en.wikipedia.org/wiki/NFC_North")%>%
  html_table(fill=TRUE)%>%
  .[[4]] #table 4 only

##############################################################################

#This is the section of the code that will create the final tidy dataset. 

#The wild card data set read in with some problems. Namely, the years with multiple
#entries are not in separate rows. To fix this problem, I am going to manipulate the
#Wild card dataset. 

# I also know that I eventually want to combine the data from the two separate dataframes 
# into one for the purposes of this visualization. I will need to Rbind the data so I want to 
# check the column names for each dataset first. Knowing column names will also be important 
# for manipulating the datasets. 

# colnames(division_champs)
# [1] "Season"          "Team"            "Record"          "Playoff Results"

# colnames(division_wc)
# [1] "Season"          "Team"            "Record"          "Playoff Results"

# In both datasets, 1982 appears as 1982+ because the season had a strike and
# the author of the dataset made a comment using a +. The first thing I want to do is change 
# 1982+ to 1982 so it isn't confusing.

division_champs[division_champs=="1982+"] <- 1982
division_wc[division_wc=="1982+"] <- 1982

# Next, I want to identify the years in which there were multiple wildcard recipients. I am 
# going to create a vector of these values and use it to create a subset of data that does not
# include these years.

division_wc_multi <- c(1982, 1993, 1994, 1997, 1999, 2001)

division_wc_new <- subset(division_wc, ! division_wc$Season %in% division_wc_multi)

# I am creating a dataset just for these unusual years. If I were working with a larger dataset, 
# I would search for a better solution in R to handle this. For this project, and considering the 
# size of the dataset, manually entering the data for these years seems like the best solution. 
# First, I am replicating each of the columns that appear in the current Wild Card dataset. 

season <- c(1982, 1982, 1982, 
        1993, 1993,
        1994, 1994, 1994,
        1997, 1997, 1997,
        1999, 1999,
        2001, 2001)

team <- c("Minnesota Vikings","Tampa Bay Buccaneers", "Detroit Lions",
        "Minnesota Vikings", "Green Bay Packers",
        "Detroit Lions", "Chicago Bears", "Green Bay Packers",
        "Detroit Lions", "Minnesota Vikings", "Tampa Bay Buccaneers",
        "Detroit Lions","Minnesota Vikings",
        "Tampa Bay Buccaneers", "Green Bay Packers")

record <- c("5-4-0", "5-4-0", "4-5-0",
          "9-7-0", "9-7-0",
          "9-7-0", "9-7-0","9-7-0",
          "9-7-0","9-7-0","10-6-0",
          "8-8-0","10-6-0",
          "9-7-0","12-4-0")

playoffs <- c("Lost NFC Second Round","Lost NFC First Round","Lost NFC First Round",
              "Lost Wild Card Playoffs","Lost Divisional Playoffs",
              "Lost Wild Card Playoffs", "Lost Divisional Playoffs", "Lost Divisional Playoffs",
              "Lost Wild Card Playoffs", "Lost Divisional Playoffs", "Lost Divisional Playoffs",
              "Lost Wild Card Playoffs", "Lost Divisional Playoffs",
              "Lost Wild Card Playoffs","Lost Divisional Playoffs")

# I am creating a dataframe that combines these 4 vectors and renaming the columns so they match. 
# This is so that I can rbind them to form one complete Wild Card dataset. 

division_wc_repetition <- data.frame(season, team, record, playoffs)

colnames(division_wc_repetition) <- c("Season", "Team", "Record", "Playoff Results")

division_wc_final <- rbind(division_wc_new, division_wc_repetition)

# I am adding a new column onto each dataset before I rbind called "Type". This will let R and I know
# which dataset each row originally came from after I rbind.  

division_wc_final$Type <- "Wild Card"

division_champs$Type <- "Division Champ"

nfc_north <- rbind(division_champs, division_wc_final)

# There are some additional labeling rows that were included in the datasets. They aren't hurting
# anything but I want to remove them so my final dataset looks cleaner. 

rm_labels <- c("NFL Central (pre-merger)", "NFC Central (post merger)", "NFC North", 
               "NFC Central")

nfc_north_final <- subset(nfc_north, ! nfc_north$Season %in% rm_labels)

#I tried creating a plot with "Teams" along the y-axis and got an error. Need to create a 
#new column that contains a numeric value to code for each "Team".

nfc_north_final$Code <- ifelse(nfc_north_final$Team=="Chicago Bears", 1,
                        ifelse(nfc_north_final$Team=="Detroit Lions", 2,
                        ifelse(nfc_north_final$Team=="Green Bay Packers", 3,
                        ifelse(nfc_north_final$Team=="Minnesota Vikings", 4,
                                      5)))) 

# Space in the column name "Playoff Results" is causing problems. Renaming the column "Results".
colnames(nfc_north_final) <- c("Season", "Team", "Record", "Results", "Type", "Code")

# Also want to encode whether a team won the NFC conference championship or super bowl in a given year.

# First, I need to create a unique key on the table.

nfc_north_final$Key <- paste(nfc_north_final$Season, nfc_north_final$Team, sep=" ")

# I will use the grep function to find keywords in the strings and create new dataframes based on
# them. Note: If a team lost the superbowl, they won the conference championship.

won_conference <- nfc_north_final[grep("Lost Super Bowl", nfc_north_final$Results), ]
won_superbowl <- nfc_north_final[grep("Won Super Bowl", nfc_north_final$Results), ]


#Need to add a new column onto the final dataset that captures this.

nfc_north_final$Win <- ifelse(nfc_north_final$Key %in% won_conference$Key, "Conference",
                       ifelse(nfc_north_final$Key %in% won_superbowl$Key, "Super Bowl",
                                                    "None")) 

# I am writing a csv of the final dataset for my records. 
# write.csv(nfc_north_final, "nfc_north_final.csv")

# Subsetting data set so it only contains games from 1970 and later. Years prior are still showing up 
# on graph - even when I use xlim command. 

nfc_north_70to14 <- subset(nfc_north_final, nfc_north_final$Season >= 1970)

##########################################################

#Making Images - Goal is to create a set of stacked time series graphs, for decades. I am going
#to begin with 1970. 

#install.packages(Hmisc)

library(Hmisc) # This library will allow me to use "minor.tick" in my plot. 

#I wanted to easily distinguish the wild card from the division champs so I used the following code
#(based on example from Visualize This) to accomplish this. 

fill_colors <- c()
for (i in 1:length(nfc_north_70to14$Type)) {
  if (nfc_north_70to14$Type[i] == "Wild Card") {
    fill_colors <- c(fill_colors, "#821122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}


#I will use the same logic to encode the shapes in the plots based on win type.  

shape_type <- c()
for (i in 1:length(nfc_north_70to14$Win)) {
  if (nfc_north_70to14$Win[i] == "Conference") {
    shape_type <- c(shape_type, 16)
  } else if (nfc_north_70to14$Win[i] == "Super Bowl"){
    shape_type <- c(shape_type, 18)
    }else {
    shape_type <- c(shape_type, 15)
  }
}

# Plot to draw decade charts. Have decided not to use this method. 
# plot(nfc_north_70to14$Season, nfc_north_70to14$Code, type="p", xlim= c(1970, 1979), pch=shape_type, 
#      cex=3, bty="n", col=fill_colors)
# minor.tick(nx=2, ny=0, tick.ratio=0.5)

#Final visualization output

plot(nfc_north_70to14$Season, nfc_north_70to14$Code, type="p", xlim= c(1970, 2014), 
     pch=shape_type, cex=3, bty="n", col=fill_colors)
minor.tick(nx=10, ny=0, tick.ratio=0.5)
 

