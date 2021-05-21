dnalist<-list("A", "T", "G", "C") # A list to extract the 4 letters
 
# Initialize with lists and dataframe
df4=list()
df3=data.frame(t(c("A", "T", "G", "C", "T", "A")))
df=list()
df1=list() 

# The main loop to extract 96 words different minimun in 3 positions.
while (nrow(df3) < 96){
  

  df=list(sample(dnalist, 6, replace = TRUE)) #Randomly extract 6 letters=1word
  df1=as.data.frame(unlist(df, use.names = TRUE))
  
  
  # Trasformations row names, column names to match the dataframes.
  df2=as.data.frame(df1)
  df2=t(df2)
  rownames(df2) <- 1:nrow(df2)
  colnames(df2) <- 1:ncol(df2)
  as.data.frame(df2)
  rownames(df3) <- 1:nrow(df3)
  colnames(df3) <- 1:ncol(df3)
  #Adding to the previous accepted sequences of letters a new one
  df3<-rbind(df3, df2)
  rownames(df3)<-1:nrow(df3)
  
  df3=as.matrix(df3)
  
  
  # Checking the similarity of the last added row with all the previous
  # and checking by position Yes if match and NO if does not match.
  dflist=list()
  cc=list()
  for (k in 1:nrow(df3)){
    
    dflist[[k]]=ifelse(df3[k, 1:6] == df3[nrow(df3), 1:6 ], print <- "YES", print <- "NO")
    
    cc[[k]]=as.data.frame(unlist(dflist[[k]], use.names = FALSE))
  }
  
  z=as.data.frame(cc)
  z_t=t(z[ , 1:(length(cc)-1)])# Extracting the last added sequence because we now the result. TRUE
  
  # Counting YES or NO and creating a table. If in the row of NO exist min 3 is accepted
  # and printing yes
  count_no=list()
  z_table=list()
  for (m in 1:nrow(z_t)){
    z_table[[m]]=as.data.frame(table(z_t[m, ]))
    count_no[[m]]=ifelse(((z_table[[m]][1, 1] == "NO")&&(z_table[[m]][1,2] >= 3)), print <- "YES", print <- "NO")
    
  }
  # If the new added sequence of 6 letters satisfy previous criteria is accepted and we proceed until 96 words.
  # If not is going to be extracted from the dataframe and we are looping with next sequence until 96.
  ifelse(count_no == "YES", df4 <-df3, df3<- df3[1:(nrow(df3)-1), ])

}

# Extracting list of 96 in a dataframe.
finaldf=as.data.frame(df3)

  
  
  
# The main algorithm finishing here with out the use of external libraries

##################################################################################################################

#Next follows the distance matrix but since it was optional I used external libraries.
#Also the follow function does not giving 100% correct results in the distance matrix.
#For some reason does not calculating correct the deviations of the positions.
  
  

#install.packages("writexl")
#library(writexl)
#write_xlsx(finaldf, "C:/Users/Nutzer/Desktop/test_project/test_project_monheim/finaldf.xlsx")


library(stringr)


#Merging  columns in one as a string
aaa=str_c(finaldf[, 1],finaldf[,2], finaldf[,3], finaldf[,4], finaldf[,5], finaldf[,6])
data<-as.data.frame(aaa)




library(stringdist)
library(janitor)
# Calculating dist matrix

dist_mat <- stringdist::stringdistmatrix(data$aaa, data$aaa, method = "lv")

rownames(dist_mat) <- data$aaa
colnames(dist_mat) <- data$aaa


d=as.data.frame(dist_mat)




