#This is the RCode to create the Buffy the Vampire Slayer treemap 

#read data into r

buffy <- read.csv(".../buffy_data_final.csv")

#portfolio is the package necessary to actually make the treemap

#install.packages("portfolio")

library(portfolio)

#adding a numeric association for each broad class

buffy$Class_num <- ifelse(buffy$Class_broad=="Slayer", 1,
                       ifelse(buffy$Class_broad=="Witch", 2,
                       ifelse(buffy$Class_broad=="Human", 3,
                       ifelse(buffy$Class_broad=="Watcher", 4,
                       ifelse(buffy$Class_broad=="Vampire", 5,
                       ifelse(buffy$Class_broad=="Demon", 6,
                       ifelse(buffy$Class_broad=="Werewolf", 7,
                       ifelse(buffy$Class_broad=="Deity/Force", 8,
                       ifelse(buffy$Class_broad=="Supernatural Human", 9,
                       ifelse(buffy$Class_broad=="Human Immortal", 10,
                       ifelse(buffy$Class_broad=="Robot", 11,
                       ifelse(buffy$Class_broad=="Bio-Mechanical Demonoid", 12,
                       ifelse(buffy$Class_broad=="Human/Zombie", 13,
                                                      14))))))))))))) 


#selecting the top 30 characters, data is already ordered

buffy_sub <- buffy[1:30,]

#creating the treepmap

map.market(id=buffy_sub$Character,area=buffy_sub$Lines, group=buffy_sub$Class_num,
           color=buffy_sub$Seasons, lab=c(TRUE,TRUE),main="Buffy Map")

  
