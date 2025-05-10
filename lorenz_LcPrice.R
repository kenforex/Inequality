

rm(list = ls())
############################################################################################################

#install.packages("ggplot2") 
#install.packages("gglorenz")
library(readxl)
library(ggplot2) 
library(gglorenz) 
library(dplyr)
library(ineq)
library(viridisLite)
library(viridis)
library(DescTools)
library(readr)
library(cowplot)
library(ggpubr)
library(xlsx)


################################################Function#####################################################
LCplot <- function(score,data){
  #Delete Missing Value
  Del = which(is.na(data$price))
  if (identical(Del,integer(0))){
  } else {
    data = data[-Del,]
    score = score[-Del]
  }
  
  Score = as.matrix(rep(0,nrow(data)))
  data = cbind(data,Score)
  data$Score = score
  data_sort = data %>% arrange(price)
  Score = data_sort$Score
  Price = data_sort$price
  PS = Price*Score
  #PS = Score
  
  Price_lc = Lc(PS, n = rep(1,length(PS)), plot = FALSE)
  p = Price_lc[1]
  L = Price_lc[2]
  D = data.frame(p,L)
  D = D[-1,]
  p = ggplot(data = D, mapping = aes(x = p)) +
    geom_abline(color="#1b98e0") +
    #geom_line(mapping = aes(y=L, color=Score, group = 1), size = 2) +
    geom_line(mapping = aes(y=L, group = 1), linewidth = 2) +
    scale_color_viridis(option = "C") +
    scale_x_continuous(name="Cumulative Proportion of Cars", limits=c(0,1)) + 
    scale_y_continuous(name="Cumulative Proportion of Prices, %,", limits=c(0,1)) +
    annotate_ineq(PS, x = 0.15, y = 0.95, decimals = 4, measure_ineq = "Gini", size = 5) +
    theme(axis.line = element_line(colour = "blue", size = 2, linetype = "solid")) #+
    #ggtitle(NAME)
  
  #annotate_figure(p, top = text_grob("New Energy Cars", color = "black", face = "bold", size = 16))
  return(p)
}

LC<-function(D){
  data = D

  num = regmatches(data$num, gregexpr("\\d+", data$num))
  score = rep(0,length(num))
  
  ############################################################################################
###########################################Loop Start###########################################
  ############################################################################################
  for (i in 1:length(num)){
    
    #Count the total number of digits in one plate number
    if (length(num[[i]])>1) { 
      #Multiple lists
      N = 0
      for (l in 1:length(num[[i]])){
        N = N + nchar(num[[i]][l])
      } 
    } else { N = nchar(num[[i]]) }
    
    #########################################New energy#########################################
    ############################################################################################
    if (data$ifnew[i] == 1){
      
      
      #####################The number of digits is 5(New)#####################
      if (N == 5){
        
        #The case of "66666","88888","99999"
        No0 = c(grepl("66666",num[[i]]),
                grepl("88888",num[[i]]),
                grepl("99999",num[[i]]))
        if (identical(which(No0 == TRUE),integer(0))==FALSE){
          
          score[i] = 100
          
        } else {
          
          #The case of "6666","8888","9999"
          No1 = c(grepl("6666",num[[i]]),
                  grepl("8888",num[[i]]),
                  grepl("9999",num[[i]]))
          if (identical(which(No1 == TRUE),integer(0))==FALSE){
            
            score[i] = 79
            
          } else {
            
            #The case of "666","888","999"
            No2 = c(grepl("666",num[[i]]),
                    grepl("888",num[[i]]),
                    grepl("999",num[[i]]))
            if (identical(which(No2 == TRUE),integer(0))==FALSE){
              
              score[i] = 61
            
            } else {
              
              #The case of "66","88","99"
              No3 = c(grepl("66",num[[i]]),
                      grepl("88",num[[i]]),
                      grepl("99",num[[i]]))
              if (identical(which(No3 == TRUE),integer(0))==FALSE){
                
                score[i] = 45
                
              } else {
                
                #The case of "0000","1111"
                No4 = c(grepl("0000",num[[i]]),
                        grepl("1111",num[[i]]))
                if (identical(which(No4 == TRUE),integer(0))==FALSE){
                  
                  score[i] = 86
                  
                } else {
                  
                  #The case of "4"
                  No5 = grepl("4",num[[i]])
                  if (identical(which(No5 == TRUE),integer(0))==FALSE){
                    
                    score[i] = 12
                    
                  } else {
                    
                    #The normal case
                    score[i] = 23
                    
                  }
                }
              }
            }
          }
        }
        
       }
      #####################The end of digits 5(New)#####################
      
      
      #####################The number of digits is 4(New)#####################
      if (N == 4){
        
          #The case of "6666","8888","9999"
          No1 = c(grepl("6666",num[[i]]),
                  grepl("8888",num[[i]]),
                  grepl("9999",num[[i]]))
          if (identical(which(No1 == TRUE),integer(0))==FALSE){
            
            score[i] = 100
            
          } else {
            
            #The case of "666","888","999"
            No2 = c(grepl("666",num[[i]]),
                    grepl("888",num[[i]]),
                    grepl("999",num[[i]]))
            if (identical(which(No2 == TRUE),integer(0))==FALSE){
              
              score[i] = 79
              
            } else {
              
              #The case of "66","88","99"
              No3 = c(grepl("66",num[[i]]),
                      grepl("88",num[[i]]),
                      grepl("99",num[[i]]))
              if (identical(which(No3 == TRUE),integer(0))==FALSE){
                
                score[i] = 61
                
              } else {
                
                #The case of "0000","1111"
                No4 = c(grepl("0000",num[[i]]),
                        grepl("1111",num[[i]]))
                if (identical(which(No4 == TRUE),integer(0))==FALSE){
                  
                  score[i] = 88
                  
                } else {
                  
                  #The case of "4"
                  No5 = grepl("4",num[[i]])
                  if (identical(which(No5 == TRUE),integer(0))==FALSE){
                    
                    score[i] = 11
                    
                  } else {
                    
                    #The normal case
                    score[i] = 30
                    
                  }
                }
              }
            }
          }
        
      }
      #####################The end of digits 4(New)#####################
      
      }
    ###################################The End of New energy####################################
    ############################################################################################
    
    
    
    #########################################Fuel energy########################################
    ############################################################################################
    if (data$ifnew[i] == 0){
      
      
      #####################The number of digits is 5(Fuel)#####################
      if (N == 5){
        
        #The case of "66666","88888","99999"
        No0 = c(grepl("66666",num[[i]]),
                grepl("88888",num[[i]]),
                grepl("99999",num[[i]]))
        if (identical(which(No0 == TRUE),integer(0))==FALSE){
          
          score[i] = 100
          
        } else {
          
          #The case of "6666","8888","9999"
          No1 = c(grepl("6666",num[[i]]),
                  grepl("8888",num[[i]]),
                  grepl("9999",num[[i]]))
          if (identical(which(No1 == TRUE),integer(0))==FALSE){
            
            score[i] = 79
            
          } else {
            
            #The case of "666","888","999"
            No2 = c(grepl("666",num[[i]]),
                    grepl("888",num[[i]]),
                    grepl("999",num[[i]]))
            if (identical(which(No2 == TRUE),integer(0))==FALSE){
              
              score[i] = 61
              
            } else {
              
              #The case of "66","88","99"
              No3 = c(grepl("66",num[[i]]),
                      grepl("88",num[[i]]),
                      grepl("99",num[[i]]))
              if (identical(which(No3 == TRUE),integer(0))==FALSE){
                
                score[i] = 45
                
              } else {
                
                #The case of "0000","1111"
                No4 = c(grepl("0000",num[[i]]),
                        grepl("1111",num[[i]]))
                if (identical(which(No4 == TRUE),integer(0))==FALSE){
                  
                  score[i] = 86
                  
                } else {
                  
                  #The case of "4"
                  No5 = grepl("4",num[[i]])
                  if (identical(which(No5 == TRUE),integer(0))==FALSE){
                    
                    score[i] = 12
                    
                  } else {
                    
                    #The normal case
                    score[i] = 23
                    
                  }
                }
              }
            }
          }
        }
        
      }
      #####################The end of digits 5(Fuel)#####################
      
      
      #####################The number of digits is 4(Fuel)#####################
      if (N == 4){
        
        #The case of "6666","8888","9999"
        No1 = c(grepl("6666",num[[i]]),
                grepl("8888",num[[i]]),
                grepl("9999",num[[i]]))
        if (identical(which(No1 == TRUE),integer(0))==FALSE){
          
          score[i] = 100
          
        } else {
          
          #The case of "666","888","999"
          No2 = c(grepl("666",num[[i]]),
                  grepl("888",num[[i]]),
                  grepl("999",num[[i]]))
          if (identical(which(No2 == TRUE),integer(0))==FALSE){
            
            score[i] = 79
            
          } else {
            
            #The case of "66","88","99"
            No3 = c(grepl("66",num[[i]]),
                    grepl("88",num[[i]]),
                    grepl("99",num[[i]]))
            if (identical(which(No3 == TRUE),integer(0))==FALSE){
              
              score[i] = 61
              
            } else {
              
              #The case of "0000","1111"
              No4 = c(grepl("0000",num[[i]]),
                      grepl("1111",num[[i]]))
              if (identical(which(No4 == TRUE),integer(0))==FALSE){
                
                score[i] = 88
                
              } else {
                
                #The case of "4"
                No5 = grepl("4",num[[i]])
                if (identical(which(No5 == TRUE),integer(0))==FALSE){
                  
                  score[i] = 11
                  
                } else {
                  
                  #The normal case
                  score[i] = 30
                  
                }
              }
            }
          }
        }
        
      }
      #####################The end of digits 4(Fuel)#####################
      
      
      #####################The number of digits is 3(Fuel)#####################
      if (N == 3){
          
          #The case of "666","888","999"
          No1 = c(grepl("666",num[[i]]),
                  grepl("888",num[[i]]),
                  grepl("999",num[[i]]))
          if (identical(which(No1 == TRUE),integer(0))==FALSE){
            
            score[i] = 100
            
          } else {
            
            #The case of "66","88","99"
            No2 = c(grepl("66",num[[i]]),
                    grepl("88",num[[i]]),
                    grepl("99",num[[i]]))
            if (identical(which(No2 == TRUE),integer(0))==FALSE){
              
              score[i] = 78
              
            } else {
              
              #The case of "4"
              No3 = grepl("4",num[[i]])
              if (identical(which(No3 == TRUE),integer(0))==FALSE){
                
                score[i] = 13
                
              } else {
                
                #The normal case
                score[i] = 27
                
              }
            }
          }
        
      }
      #####################The end of digits 3(Fuel)#####################
      
    }
    ##################################The End of Fuel energy####################################
    ############################################################################################
    
    
    }
  ############################################################################################
############################################Loop End############################################
  ############################################################################################    

  #Delete Error Score
  Del = which(score==0)
  data = data[-Del,]
  score = score[-Del]
  Delnum = length(Del)
  
  #Total plot and score
  p1 = LCplot(score,data)
  score = as.matrix(score)
  SD_Total = cbind(data,score)
  
  #New energy plot and score
  Ind_new = which(data$ifnew == 1)
  data_new = data[Ind_new,]
  score_new = score[Ind_new]
  p1_2 = LCplot(score_new,data_new)
  score_new = as.matrix(score_new)
  SD_New = cbind(data_new,score_new)

  #Fuel energy plot and score
  Ind_fuel = which(data$ifnew == 0)
  data_fuel = data[Ind_fuel,]
  score_fuel = score[Ind_fuel]
  p1_3 = LCplot(score_fuel,data_fuel)
  score_fuel = as.matrix(score_fuel)
  SD_Fuel = cbind(data_fuel,score_fuel)
  
  L = list("Error" = Del, "ErrorNum" = Delnum, "SD_Total" = SD_Total, "SD_New" = SD_New, "SD_Fuel" = SD_Fuel,
           "Total_plot" = p1, "New_plot" = p1_2, "Fuel_plot" = p1_3)
  return(L)
}



###################################################Read Data##################################################

#Write File Name and name
FileName = "~/Desktop/Prof_Sun project/inequality/Latest Data/data/shanghai3040.xlsx"
name = "Shanghai"

data <- read_excel(FileName)

#Total Score with Data
Total_Score = LC(data)$SD_Total
filename1 = "~/Desktop/Total_Score.xlsx"
#write.xlsx(Total_Score, filename1,row.names = FALSE)
#New Energy Score with Data
New_Score = LC(data)$SD_New
filename2 = "~/Desktop/New_Score.xlsx"
#write.xlsx(New_Score, filename2,row.names = FALSE)
#Fuel Energy Score with Data
Fuel_Score = LC(data)$SD_Fuel
filename3 = "~/Desktop/Fuel_Score.xlsx"
#write.xlsx(Fuel_Score, filename3,row.names = FALSE)

#Total Figure
Total_Figure = LC(data)$Total_plot
Title1 = paste(name,"Total Cars")
dev.new()
annotate_figure(Total_Figure, top = text_grob(Title1, color = "black", face = "bold", size = 14))
#New Energy Figure
New_Figure = LC(data)$New_plot
Title2 = paste(name,"New Cars")
dev.new()
annotate_figure(New_Figure, top = text_grob(Title2, color = "black", face = "bold", size = 14))
#Fuel Energy Figure
Fuel_Figure = LC(data)$Fuel_plot
Title3 = paste(name,"Fuel Cars")
dev.new()
annotate_figure(Fuel_Figure, top = text_grob(Title3, color = "black", face = "bold", size = 14))

