library(readxl)
library(dplyr)
library(xlsx)

state <- c("Kansas","Nebraska","Wyoming ","North Dakota")

df <- data.frame()

#Read in district poverty info
dist_info <- read_excel("C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/school_districts_broadband_v2.xlsx")

#Percent of students w/o broadband
dist_info$unserved_percentage <- as.numeric(dist_info$unserved_pop_5_19)/as.numeric(dist_info$population_5_19_2018)*100

#Read in raw coding data
for (states in state){
  
  df <- rbind(df,read_excel("C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/CE6.6.1 Data Entry_FINAL.xlsx", 
                            sheet = states)[,1:29])
}

#Filter out uncoded plans
df_coded <- df

#Match column names for dist info
names(dist_info)[4] <- "District"
names(dist_info)[46] <- "State"
names(dist_info)[51] <- names(df_coded)[27]
names(dist_info)[50] <- names(df_coded)[29]
names(dist_info)[43] <- names(df_coded)[28]

#Filter out irrelevant info columns
relevent_dist_info <- dist_info[,c(4,43,46,50,51,52)]

#Merge with coded dataset
df_merged <- merge(df_coded[,1:26],relevent_dist_info)

#Remove duplicate plans for reliability
df_dupes_removed <- df_merged[-which((duplicated(df_merged[1]) | duplicated(df_merged[1],fromLast = TRUE)) & df_merged[4] != "Team"),]
df_dupes_removed <- df_dupes_removed[-which((duplicated(df_dupes_removed[1]))),]

df_dupes_removed <- merge(df_dupes_removed[,1:26],relevent_dist_info)

df_dupes_removed$quartile <- gtools::quantcut(as.numeric(df_dupes_removed$`District Poverty`), q = 4)

#Create dataframe for each state
df_KS <- df_dupes_removed[df_dupes_removed[2] == "Kansas",]
df_ND <- df_dupes_removed[df_dupes_removed[2] == "North Dakota",]
df_WY <- df_dupes_removed[df_dupes_removed[2] == "Wyoming",]
df_NE <- df_dupes_removed[df_dupes_removed[2] == "Nebraska",]

df_KS$`Poverty Quantile` <- gtools::quantcut(as.numeric(df_KS$`District Poverty`), q = 4)
df_ND$`Poverty Quantile` <- gtools::quantcut(as.numeric(df_ND$`District Poverty`), q = 4)
df_WY$`Poverty Quantile` <- gtools::quantcut(as.numeric(df_WY$`District Poverty`), q = 4)
df_NE$`Poverty Quantile` <- gtools::quantcut(as.numeric(df_NE$`District Poverty`), q = 4)

gmodels::CrossTable(df_dupes_removed$`Internet Access`,df_dupes_removed$State)
xtabs(formula = ~`Student access to device` + State,data = df_dupes_removed)

#Create output files
xlsx::write.xlsx(x = df_KS, file = "C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/output/Kansas_data.xlsx",sheetName = "Data",append = TRUE)
xlsx::write.xlsx(x = df_ND, file = "C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/output/North_Dakota_data.xlsx",sheetName = "Data",append = TRUE)
xlsx::write.xlsx(x = df_WY, file = "C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/output/Wyoming_data.xlsx",sheetName = "Data",append = TRUE)
xlsx::write.xlsx(x = df_NE, file = "C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/output/Nebraska_data.xlsx",sheetName = "Data",append = TRUE)

#By State

#Reformatting dataframe

df_Long <- reshape2::melt(data = df_dupes_removed[,-c(3,4,27)],id.vars = names(df_dupes_removed)[1:2])
df_Long <- df_Long[df_Long$variable != "unserved_percentage",]
df_Long <- df_Long[!is.na(df_Long$value),]
df_Long[df_Long$value == "NO",]$value <- "No"


df_tally <- df_Long[,-1] %>%
  group_by(State,variable,value) %>%
  tally() 

df_tally_perc <- df_Long[,-1] %>%
  group_by(variable,State,value) %>%
  tally() %>%
  mutate(percent = round((n/sum(n))*100,digits = 1))

#Reshape raw counts
df_tally <- df_tally[!is.na(df_tally$value),] 

df_wide <- tidyr::spread(df_tally, key = State, value = n)

df_wide[is.na(df_wide)] <- 0

#Reshape Percentage
df_tally_perc <- df_tally_perc[,-4]

df_tally_perc <- df_tally_perc[!is.na(df_tally_perc$value),]

df_wide_perc <- tidyr::spread(df_tally_perc, key = State, value = percent)

df_wide_perc[is.na(df_wide_perc)] <- 0

#Create output files
xlsx::write.xlsx(x = data.frame(df_wide), file = "C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/output/Aggregated_State.xlsx")
xlsx::write.xlsx(x = data.frame(df_wide_perc), file = "C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/output/Aggregated_State.xlsx",sheetName = "Percent",append = TRUE)

#By Connectivity

#Reformatting dataframe

df_Long <- reshape2::melt(data = df_dupes_removed[,-c(3,4,27)],id.vars = names(df_dupes_removed)[c(1,28)])
df_Long <- df_Long[df_Long$variable != "unserved_percentage",]
df_Long <- df_Long[!is.na(df_Long$value),]
df_Long <- df_Long[!df_Long$`Internet Connectivity` == "Unknown",]
df_Long[df_Long$value == "NO",]$value <- "No"


df_tally <- df_Long[,-1] %>%
  group_by(`Internet Connectivity`,variable,value) %>%
  tally()

df_tally_perc <- df_Long[,-1] %>%
  group_by(variable,`Internet Connectivity`,value) %>%
  tally() %>%
  mutate(percent = round((n/sum(n))*100,digits = 1))

#Reshape raw counts
df_tally <- df_tally[!is.na(df_tally$value),]

df_wide <- tidyr::spread(df_tally, key = `Internet Connectivity`, value = n)

df_wide[is.na(df_wide)] <- 0

#Reshape Percentage
df_tally_perc <- df_tally_perc[,-4]

df_tally_perc <- df_tally_perc[!is.na(df_tally_perc$value),]

df_wide_perc <- tidyr::spread(df_tally_perc, key = `Internet Connectivity`, value = percent)

df_wide_perc[is.na(df_wide_perc)] <- 0

#Create output files
xlsx::write.xlsx(x = data.frame(df_wide), file = "C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/output/Aggregated_Connectivity.xlsx")
xlsx::write.xlsx(x = data.frame(df_wide_perc), file = "C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/output/Aggregated_Connectivity.xlsx",sheetName = "Percent",append = TRUE)


#By Locale

#Reformatting dataframe

df_Long <- reshape2::melt(data = df_dupes_removed[,-c(3,4,27)],id.vars = names(df_dupes_removed)[c(1,29)])
df_Long <- df_Long[df_Long$variable != "unserved_percentage",]
df_Long <- df_Long[!is.na(df_Long$value),]
df_Long[df_Long$value == "NO",]$value <- "No"
df_Long[df_Long$`Urban-Centric Locale` == "Other",]$`Urban-Centric Locale` <- "Rural"
df_Long <- df_Long[!is.na(df_Long$`Urban-Centric Locale`),]

sum(!is.na(df_dupes_removed$`Internet Access`))


df_tally <- df_Long[,-1] %>%
  group_by(`Urban-Centric Locale`,variable,value) %>%
  tally()

df_tally_perc <- df_Long[,-1] %>%
  group_by(variable,`Urban-Centric Locale`,value) %>%
  tally() %>%
  mutate(percent = round((n/sum(n))*100,digits = 1))

#Reshape raw counts
df_tally <- df_tally[!is.na(df_tally$value),]

df_wide <- tidyr::spread(df_tally, key = `Urban-Centric Locale`, value = n)

df_wide[is.na(df_wide)] <- 0

#Reshape Percentage
df_tally_perc <- df_tally_perc[,-4]

df_tally_perc <- df_tally_perc[!is.na(df_tally_perc$value),]

df_wide_perc <- tidyr::spread(df_tally_perc, key = `Urban-Centric Locale`, value = percent)

df_wide_perc[is.na(df_wide_perc)] <- 0

#Create output files
xlsx::write.xlsx(x = data.frame(df_wide), file = "C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/output/Aggregated_locale.xlsx")
xlsx::write.xlsx(x = data.frame(df_wide_perc), file = "C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/output/Aggregated_locale.xlsx",sheetName = "Percent",append = TRUE)

#By quartile

df_Long <- reshape2::melt(data = df_dupes_removed[,-c(2,3,4,27)],id.vars = names(df_dupes_removed)[c(1,31)])
df_Long <- df_Long[df_Long$variable != "unserved_percentage",]
df_Long <- df_Long[!is.na(df_Long$value),]
df_Long[df_Long$value == "NO",]$value <- "No"
df_Long <- df_Long[!is.na(df_Long$quartile),]

df_tally <- df_Long[,-1] %>%
  group_by(quartile,variable,value) %>%
  tally()

df_tally_perc <- df_Long[,-1] %>%
  group_by(variable,quartile,value) %>%
  tally() %>%
  mutate(percent = round((n/sum(n))*100,digits = 1))


#Reshape raw counts
df_tally <- df_tally[!is.na(df_tally$value),]
df_tally <- df_tally[!is.na(df_tally$quartile),]

df_wide <- tidyr::spread(df_tally, key = quartile, value = n)

df_wide[is.na(df_wide)] <- 0

#Reshape Percentage
df_tally_perc <- df_tally_perc[,-4]

df_tally_perc <- df_tally_perc[!is.na(df_tally_perc$value),]


df_wide_perc <- tidyr::spread(df_tally_perc, key = quartile, value = percent)

df_wide_perc[is.na(df_wide_perc)] <- 0

#Create output files
xlsx::write.xlsx(x = data.frame(df_wide), file = "C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/output/Aggregated_quartile.xlsx")
xlsx::write.xlsx(x = data.frame(df_wide_perc), file = "C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/output/Aggregated_quartile.xlsx",sheetName = "Percent",append = TRUE)





dupess <- df_merged[which((duplicated(df_merged[1]) | duplicated(df_merged[1],fromLast = TRUE))),]
xlsx::write.xlsx(x = data.frame(dupess), file = "C:/Users/DavidMcCullough/Marzano Research/MZR - Share/10_REL/Task 6-Research/CE6.6_JiT/Data Entry/Analysis/output/missing_reliability22.xlsx",append = TRUE)
