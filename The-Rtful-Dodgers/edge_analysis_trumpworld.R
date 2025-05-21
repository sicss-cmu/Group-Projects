# installing packages
install.packages(c("igraph", "tidyverse", "ggraph", "tidygraph", "gender", "remotes"))

# loading packages
library(igraph)
library(tidyverse)
library(ggraph)
library(tidygraph)
library(gender)
library(dplyr)
library(stringr)
library(ggrepel)
library(scales)
library(visNetwork)
library(readr)
library(sjmisc)
library(rlist)

#read CSV(s)
trumpworld <- read.csv("GitHub/sicss-2025-project/trumpworld.csv")
connection_classification <- read.csv("GitHub/sicss-2025-project/connection_classifications.csv")

#Identify unique types of connections and save as a variable
Unique_conn_types <- unique(trumpworld$Connection) #833 unique terms

#Group connections by types, then feed types into mutate setup
Ownership_conn <-str_find(connection_classification$Classifications,"Ownership")
Political_conn <-str_find(connection_classification$Classifications,"Political")
Executive_conn <-str_find(connection_classification$Classifications,"Executive")
Personal_conn <-str_find(connection_classification$Classifications,"Personal")

#cluster unique conn types by index?
Ownership_conn_types <-Unique_conn_types[Ownership_conn]
Political_conn_types <-Unique_conn_types[Political_conn]
Executive_conn_types <-Unique_conn_types[Executive_conn]
Personal_conn_types <-Unique_conn_types[Personal_conn]



#mutate trumpworld to add classifications to 
trumpworld_classified <-trumpworld %>%
  mutate(classification = case_when(
    Connection %in% Ownership_conn_types ~"Ownership",
    Connection %in% Political_conn_types ~"Political",
    Connection %in% Executive_conn_types ~"Executive",
    Connection %in% Personal_conn_types ~"Personal",
  ))



#concatenate vectors (Unique agents from both A and B)

concat_agents<-c(unique(trumpworld$Entity.A), unique(trumpworld$Entity.B))

#unique agents

unique_agents_final<-unique(concat_agents)

#create networks for each unique connection type? (Can use this to feed in later stuff once clustered)

#Need to turn things into adjacency matrices?-> more likely 

uniqueClasses <-unique(trumpworld_classified$classification)

#start by grouping by connection type?
networkData = list()#null preallocate?
currentData = NULL
for (i in 1:length(uniqueClasses)){
  #networkName = paste(Unique_conn_types[i],"_network",sep = '')
   currentData<- trumpworld_classified %>%
    filter(classification == uniqueClasses[i]) #why does it not keep the rows?
  networkData[[i]] <- currentData
}

#Iterate through network data to create network connectivity matrices (across both diags)

networkMatrices = list() #no idea if this works
currentMatrix = NULL

for (i in 1:length(uniqueClasses)){
  currentData <-networkData[[i]] #indext into list to extract relevant dataframe
  currentMatrix <- matrix(0,length(unique_agents_final),length(unique_agents_final)) #preallocate zeros to everything
  for (j in 1:nrow(currentData)){ #go through each row of data
    #find indexes of agents
    agent1 = currentData$Entity.A[j] #get jth val of entity A of current data
    agent1Index = match(agent1,unique_agents_final) #retrieves index of agent
    
    agent2 = currentData$Entity.B[j]
    agent2Index = match(agent2,unique_agents_final)
    #Assign "1" to connectivity matrices in relevant places(2 for reciprocity)
    currentMatrix[agent1Index,agent2Index] <-1
    currentMatrix[agent2Index,agent1Index] <-1
  }
  networkMatrices[[i]]<-currentMatrix #assign matrix to place in the list
}

#Organize nodes -> (i,...) (j,...) -> save as 2 separate lists (will be ordered as well since a to b)

networkEdges = list()#preallocate null list
currentEdgesA = list()
currentEdgesB = list()
for (i in 1:length(uniqueClasses)){
  currentData <-networkData[[i]]
  currentEdgesA = list()#null list
  currentEdgesB = list()
  for (j in 1:nrow(currentData)){
    agent1 = currentData$Entity.A[j] #get jth val of entity A of current data
    agent1Index = match(agent1,unique_agents_final) #retrieves index of agent
    agent2 = currentData$Entity.B[j]
    agent2Index = match(agent2,unique_agents_final)
    #append index values to edges
    
    list.append(currentEdgesA,agent1Index) #List append not working
    list.append(currentEdgesB,agent2Index)
    
  }
  #Save the 2 lists to the network edges
  networkEdges[[i]] <- do.call(rbind,Map(data.frame,A = currentEdgesA,B =currentEdgesB))
}


#Make networks with matrix?

nodes = data.frame(id = 1:length(unique_agents_final),
                   label = unique_agents_final #can add other stuff here -> ex: square for org, circle for person?
                   )
#Nodes are shared between graphs




#write file, separate by new lines (Ended up just copying and pasting from terminal output. DO NOT RUN THIS)

#write(Unique_conn_types,file = "txtConnections.txt",ncolumns = 1,sep = "\n")

#try to write to file

#fileConn <-file("txtconnections.txt")
#writeLines(Unique_conn_types,con = fileConn,sep = "\n")
#close(fileConn)

#write.table(Unique_conn_types, file = "~/GitHub/sicss-2025-project/txtconnections.txt",sep = "\n")

