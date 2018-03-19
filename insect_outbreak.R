# insect_outbreak.R
# 
# R script for insect outbreak connectivity
#
# Version: 1.0
# Authors: Greg Huang, Gabriel Yiu
# Last update: 11:20 pm, March 16, 2018, by Greg
#
# Versions:
#           1.0 Initial setup
#
# =============================================================================

#==== load in dependent .csv files ====
connections <- read.csv("connecting_outbreaks.csv")
num_outbreaks <- read.csv("number_of_connecting_outbreaks.csv")
dict <- read.csv("dict_connections.csv")
master <- read.csv("edit_master.csv")
#in the master file, 0 indicates no outbreak, 1 indicates outbreak

#==== categorize the interactions ====
# this step is done to merge the two IDs and demonstrate a categorical ID for 
# each interaction

id1 <- c(connections$id_1)
id2 <- c(connections$id_2)
id_interaction <-  vector("list", 1235)
i <- 1
while (i < 1235){
  id_interaction[[i]] <- paste(as.character(id1[i]),as.character(id2[i]))
  id_interaction[[i]] <- gsub(" ", "->", id_interaction[i])
  i <- i+1
}

# bind the merged IDs into the dataframe
connections$id_interaction <- cbind(id_interaction)

#==== clean up the dictionary dataframe ====
# n stands for the other nodes the current node connects to (neighbors)
newcols <- c("node", "n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9", "n10")
colnames(dict) <- newcols
rownames(dict) <- dict$node
#dict dataframe is now cleaned up (easier to access and understand)

#==== begin checking each of the interactions on the yearly basis==== 
#Comparing each node j at time t to node j at time t + 1.
#There are 4 options of what can happen at node j between t, t + 1.
#0 -> 0 will be 0, 0 -> 1 will be 1, 1 -> 0 will be -1, and 1 -> 1 will be 2.
master[is.na(master)] <- 0
self_comparison <- data.frame(id = numeric(0), y2000_y2001 = numeric(0),
                              y2001_y2002 = numeric(0), y2002_y2003 = numeric(0),
                              y2003_y2004 = numeric(0), y2004_y2005 = numeric(0),
                              y2005_y2006 = numeric(0), y2006_y2007 = numeric(0),
                              y2007_y2008 = numeric(0), y2008_y2009 = numeric(0),
                              y2009_y2010 = numeric(0), y2010_y2011 = numeric(0),
                              y2011_y2012 = numeric(0), y2012_y2013 = numeric(0))

for (elem in 1:nrow(master)){
  node <- master[elem,]
  inp_vec <- c(elem)
  for (i in 5:17){
    if (node[i] == 0 & node[i + 1] == 0){
      inp_vec <- c(inp_vec, 0)
    }else if (node[i] == 0 & node[i + 1] == 1){
      inp_vec <- c(inp_vec, 1)
    }else if (node[i] == 1 & node[i + 1] == 0){
      inp_vec <- c(inp_vec, -1)
    }else{
      inp_vec <- c(inp_vec, 2)
    }
  }
  self_comparison[nrow(self_comparison) + 1,] <- inp_vec
}
# self_comparison is the data frame described above

#==== Counts year to year ====
# The total column is the number of neighbours and
# each entry under each year refers to the number of neighbours
# that had an insect outbreak.
neigh_counts <- data.frame(id = numeric(0), total = numeric(0),
                           y2000 = numeric(0), y2001 = numeric(0),
                           y2002 = numeric(0), y2003 = numeric(0),
                           y2004 = numeric(0), y2005 = numeric(0), 
                           y2006 = numeric(0), y2007 = numeric(0), 
                           y2008 = numeric(0), y2009 = numeric(0), 
                           y2010 = numeric(0), y2011 = numeric(0), 
                           y2012 = numeric(0), y2013 = numeric(0))
for (elem in 1:nrow(master)){
  neigh <- dict[elem,]
  neigh <- neigh[!is.na(neigh)]
  inp_vec <- c(neigh[1], length(neigh) - 1)
  for (yr in 5:18){
    count <- 0
    for (n in neigh[2:length(neigh)]){
      count <- master[n + 1, yr] + count
    }
    inp_vec <- c(inp_vec, count)
  }
  neigh_counts[nrow(neigh_counts) + 1,] <- inp_vec
}
