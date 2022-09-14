df <- read.csv("~/Downloads/NeuroMAP+Adult+Demographics_January+29,+2020_14.17.csv")

df<-Demographics

str(df)


df <- df %>% 
  select(
    Q1,
    # Q7,
    Q8_1,
    Q8_2,
    Q8_3,
    Q8_4,
    Q8_5
  ) %>%
  rename(
    `ID` = Q1,
    # `Hispanic/Latino` = Q7,
    `American Indian/Alaskan Native` = Q8_1,
    `Asian` = Q8_2,
    `Black/African American` = Q8_3,
    `Native Hawaiian/Other Pacific Islander` = Q8_4,
    `White` = Q8_5
  ) 


#df <- df[-c(1,2),]
df[,-1] <- data.frame(lapply(df[,-1], function(x) as.numeric(as.character(x))))
df[,-1] <- data.frame(ifelse(is.na(df[,-1]), 0,1))





df_sum <- df %>% group_by(ID) %>%mutate(master_race = ifelse(
  sum(`American Indian/Alaskan Native`, `Asian`, `Black/African American`, `Native Hawaiian/Other Pacific Islander`, `White`) >= 2,
  "bi/multiracial",
  "uniracial")) %>% ungroup() %>% data.frame()#, `Black.African.American`, `Native.Hawaiian.Other.Pacific.Islander`, `White`))

races <- colnames(df_sum)[2:6]
for(row in 1:nrow(df_sum)){
  if(df_sum$master_race[row] != "bi/multiracial"){
    
    if(length(races[which(df_sum[row,races] == 1)]) == 1){
      df_sum$master_race[row] <- races[which(df_sum[row,races] == 1)]
    } else{
      df_sum$master_race[row] <- "no_race_given"
    }
    
    
    
  }
}



# 
# row <- 2
# 
# 
# # df_sum <- df %>% mutate(master_race = sum(colnames(.)))
# # 
# # 
# # str(df)
# # 
