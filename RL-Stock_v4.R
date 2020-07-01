getwd()

rm(list=ls())
setwd("F:/R")
library(ReinforcementLearning)
library(tidyverse)
apple = read_csv("apple_v1_csv.csv")

#n=nrow(apple)
n=100

reward=1
penalty=-1
starttime=""
endtime=""
win=0
loss=0
############################################ create table

t1=data.frame(EMA = ".",
              SMA = ".",
              StateX="..",
              ActionX="1",
              NextStateX="..",
              RewardX=0,
              close=".",
              row=1:n,
              stringsAsFactors = FALSE,
              status="")

######################################################################################

########################################################################## attributes used in RL
apple1=apple %>%                                                                 # reducr dimension to 2/3  
  mutate(
    s1=ifelse(EMA>open,(s1="O"),(ifelse(EMA<open,(s1="X"),(s1=".")))),           # reduct dimension to 3 for EMA, compare with open, could possibly be wrong, maybe should compair with SMA
    s2=ifelse(SMA>open,(s2="O"),(ifelse(SMA<open,(s2="X"),(s2=".")))),           # reduct dimension to 3 for SMA, compare with open, same as above
    s9=ifelse(close>open,(s9="O"),(ifelse(close<open,(s9="X"),(s9="."))))        # reduce dimension to 3 for close, compare with open, same as above
    
  ) 
t1$EMA[4:n]=apple1$s1[4:n]
t1$SMA[4:n]=apple1$s2[4:n]
t1$close[4:n]=apple1$s9[4:n]



############################################ pretrain, let RL now all the choices first

RandomSet=c("buy","hold","sell")   #generate random dataset used for random number selection

for (i in (1:3)) {
  t1[i,"ActionX"]=RandomSet[i]
}



# reset some parameters
learningCount=0
data_unseen=0
model=0                          # reset the model 
       
starttime=Sys.time()
for (i in 4:n) {
  
  
  ######################################################## continue from bottom, copy current progress into column StateX  
  t1[i,"StateX"]=paste(t1[i,"EMA"],t1[i,"SMA"],sep="")
  
  ######################################################## Start RL learning 
  
 
  control     <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.1)
  model       <- ReinforcementLearning(t1[1:i-1,], s = "StateX", a = "ActionX", r = "RewardX", s_new = "NextStateX",iter = 5, control = control)                              
  data_unseen <- data.frame(State = t1[i,"StateX"], stringsAsFactors = FALSE)                 # get the data you want to import to RL

  options(show.error.messages = FALSE)                                                  # surpress error message. If this specific type not showing in training, it will send an error
  mtry=try(predict(model, data_unseen$State))                                           # try to predict use the model just trained
  options(show.error.messages = TRUE)                                                   # stop surpress error message. So if other error happens then ill know

  if(inherits(mtry, "try-error")){                                                      # another loop to make sure if there is error in predicting, it will pick a random number to continue the code.
    action=RandomSet[1]                                                                 # if met a unseen state, just buy it
  }else{action=mtry}                                                                    # if the number generated is correct, use that number keep going

  t1[i,"ActionX"]=action
  ######################################################## learning ends, we have the predict action now
  ######################################################## if it's a buying action, count for reward
  
  if(t1[i,"ActionX"]=="buy"){
    if(t1[i,"close"]=="O"){t1[i,"RewardX"]=reward;win=win+1;status="win"}                         # if stock price increase after by in, reward
    if(t1[i,"close"]=="X"){t1[i,"RewardX"]=penalty;loss=loss+1;status="loss"}                        # if stock price decrease after by in, penalty
    t1[i,"status"]=status
    }
  
  ######################################################## if it's a holding action, count for reward. Basically doing nothing
  
  if(t1[i,"ActionX"]=="hold"){
    t1[i,"RewardX"]=penalty 
    status="loss"
    t1[i,"status"]=status
  }
  
  ######################################################## if it's a selling action, count for reward
  
  if(t1[i,"ActionX"]=="sell"){
    if(t1[i,"close"]=="X"){t1[i,"RewardX"]=reward;win=win+1;status="win"}                         # if stock price decrease after sell, reward
    if(t1[i,"close"]=="O"){t1[i,"RewardX"]=penalty;loss=loss+1;status="loss"}                        # if stock price increase after sell, penalty
    t1[i,"status"]=status
    
  }
        
  print(paste0(" @ step # ",i,", Winning % -> ",round((win/i)*100,digit=2)," % , ", status))  
  endtime=Sys.time()
  
}
totaltime=endtime-starttime
totaltime
for (i in (1:10)) {
  a=t1[(i*200-199):(i*200),] %>%
    count(.,.$RewardX==1)
  b=a$n[[2]]/200
  
  print(b)
}

model
t1
sum(t1$RewardX==1)
sum(t1$RewardX==-0.1)
sum(t1$RewardX==-1)

t1 %>%
  count(.,.$RewardX)

summary(t1)
summary(t1$RewardX)
summary(model)
filename=paste0("Stock_RL_data_0501_1130pm_",n,"rows.Rda")
filename
save(t1,file=filename)
