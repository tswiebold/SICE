#takes the average/median or mode of each cell with missing data across all imputed datasets


if (ncol(Continuous) == ncol(NoMissingness)){
  Continuous <- Continuous
} else {
  
  #Continuous SICE using median
  Cont_imp <- imputed_Continuous$imp[-c(1:ncol(NoMissingness))]
  Continuous <- as.data.frame(Continuous[,-c(1:ncol(NoMissingness))])
  if(ncol(Continuous) == 1){
    colnames(Continuous) <- Continous_names_2 
  }
  
  
  for (i in 1:length(Cont_imp)) {
    Var_imp <- as.data.frame(Cont_imp[i]) #creates the data.frame
    Var_imp$median <- rep(0,nrow(Var_imp)) #creates a vector for the median
    Var_name <- colnames(Var_imp)[1] #gets the variable name from the column names of Var_imp that needs to be cleaned
    Var_name <- stringr::str_replace(Var_name, '\\.1', '')
    # Var_name <- stringr::str_replace(Var_name, '1', '')
    Row.num <- as.numeric(row.names(Var_imp)) #gets the row numbers that need to be filled with the median of the imputed values
    for (j in 1:nrow(Var_imp)) {
      Med <- median(as.numeric(Var_imp[j,]))
      Var_imp[j,which(colnames(Var_imp) == "median")] <- Med
      Continuous[Row.num[j],which(colnames(Continuous) == Var_name)] <- Var_imp$median[j]
    }
  }
}
##############################################################################################
#Binary SICE using mode

Mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
} #source: https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode

Binary_imp <- imputed_Binary$imp[-c(1:ncol(NoMissingness))]
Binary <- as.data.frame(Binary[,-c(1:ncol(NoMissingness))])
if(ncol(Binary) == 1){
  colnames(Binary) <- Binary_names_2 
}

for (i in 1:length(Binary_imp)) {
  Var_imp <- as.data.frame(Binary_imp[i]) #creates the data.frame
  Var_imp$mode <- rep(NA,nrow(Var_imp)) #creates a vector for the mode
  Var_name <- colnames(Var_imp)[1] #gets the variable name from the column names of Var_imp that needs to be cleaned
  Var_name <- stringr::str_replace(Var_name, '\\.1', '')
  # Var_name <- stringr::str_replace(Var_name, '1', '')
  Row.num <- as.numeric(row.names(Var_imp)) #gets the row numbers that need to be filled with the mode of the imputed values
  for (j in 1:nrow(Var_imp)) {
    Modde <- Mode(Var_imp[j,])
    Var_imp[j,which(colnames(Var_imp) == "mode")] <- levels(Binary[Row.num[j],which(colnames(Binary) == Var_name)])[which(levels(Binary[Row.num[j],which(colnames(Binary) == Var_name)]) == Modde)]
    Binary[Row.num[j],which(colnames(Binary) == Var_name)] <- Var_imp$mode[j]
  }
}

#################################################################################################
#Categorical SICE using mode
MultiCHOIS_imp <- imputed_MultiCategoryCHOIS$imp[-c(1:ncol(NoMissingness))]

for (i in 1:length(MultiCHOIS_imp)) {
  Var_imp <- as.data.frame(MultiCHOIS_imp[i]) #creates the data.frame
  Var_imp$mode <- rep(NA,nrow(Var_imp)) #creates a vector for the median
  Var_name <- colnames(Var_imp)[1] #gets the variable name from the column names of Var_imp that needs to be cleaned
  Var_name <- stringr::str_replace(Var_name, '\\.1', '')
  # Var_name <- stringr::str_replace(Var_name, '1', '')
  Row.num <- as.numeric(row.names(Var_imp)) #gets the row numbers that need to be filled with the median of the imputed values
  for (j in 1:nrow(Var_imp)) {
    Modde <- Mode(Var_imp[j,])
    Var_imp[j,which(colnames(Var_imp) == "mode")] <- levels(MultiCategory[Row.num[j],which(colnames(MultiCategory) == Var_name)])[which(levels(MultiCategory[Row.num[j],which(colnames(MultiCategory) == Var_name)]) == Modde)]
    MultiCategory[Row.num[j],which(colnames(MultiCategory) == Var_name)] <- Var_imp$mode[j]
  }
}

MultiPHQ9_imp <- imputed_MultiCategoryPHQ9$imp[-c(1:ncol(NoMissingness))]

for (i in 1:length(MultiPHQ9_imp)) {
  Var_imp <- as.data.frame(MultiPHQ9_imp[i]) #creates the data.frame
  Var_imp$mode <- rep(NA,nrow(Var_imp)) #creates a vector for the median
  Var_name <- colnames(Var_imp)[1] #gets the variable name from the column names of Var_imp that needs to be cleaned
  Var_name <- stringr::str_replace(Var_name, '\\.1', '')
  # Var_name <- stringr::str_replace(Var_name, '1', '')
  Row.num <- as.numeric(row.names(Var_imp)) #gets the row numbers that need to be filled with the median of the imputed values
  for (j in 1:nrow(Var_imp)) {
    Modde <- Mode(Var_imp[j,])
    Var_imp[j,which(colnames(Var_imp) == "mode")] <- levels(MultiCategory[Row.num[j],which(colnames(MultiCategory) == Var_name)])[which(levels(MultiCategory[Row.num[j],which(colnames(MultiCategory) == Var_name)]) == Modde)]
    MultiCategory[Row.num[j],which(colnames(MultiCategory) == Var_name)] <- Var_imp$mode[j]
  }
}

MultiIA_imp <- imputed_MultiCategoryIA$imp[-c(1:ncol(NoMissingness))]

for (i in 1:length(MultiIA_imp)) {
  Var_imp <- as.data.frame(MultiIA_imp[i]) #creates the data.frame
  Var_imp$mode <- rep(NA,nrow(Var_imp)) #creates a vector for the median
  Var_name <- colnames(Var_imp)[1] #gets the variable name from the column names of Var_imp that needs to be cleaned
  Var_name <- stringr::str_replace(Var_name, '\\.1', '')
  # Var_name <- stringr::str_replace(Var_name, '1', '')
  Row.num <- as.numeric(row.names(Var_imp)) #gets the row numbers that need to be filled with the median of the imputed values
  for (j in 1:nrow(Var_imp)) {
    Modde <- Mode(Var_imp[j,])
    Var_imp[j,which(colnames(Var_imp) == "mode")] <- levels(MultiCategory[Row.num[j],which(colnames(MultiCategory) == Var_name)])[which(levels(MultiCategory[Row.num[j],which(colnames(MultiCategory) == Var_name)]) == Modde)]
    MultiCategory[Row.num[j],which(colnames(MultiCategory) == Var_name)] <- Var_imp$mode[j]
  }
}

MultiIntake_imp <- imputed_MultiCategoryIntake$imp[-c(1:ncol(NoMissingness))]

for (i in 1:length(MultiIntake_imp)) {
  Var_imp <- as.data.frame(MultiIntake_imp[i]) #creates the data.frame
  Var_imp$mode <- rep(NA,nrow(Var_imp)) #creates a vector for the median
  Var_name <- colnames(Var_imp)[1] #gets the variable name from the column names of Var_imp that needs to be cleaned
  Var_name <- stringr::str_replace(Var_name, '\\.1', '')
  # Var_name <- stringr::str_replace(Var_name, '1', '')
  Row.num <- as.numeric(row.names(Var_imp)) #gets the row numbers that need to be filled with the median of the imputed values
  for (j in 1:nrow(Var_imp)) {
    Modde <- Mode(Var_imp[j,])
    Var_imp[j,which(colnames(Var_imp) == "mode")] <- levels(MultiCategory[Row.num[j],which(colnames(MultiCategory) == Var_name)])[which(levels(MultiCategory[Row.num[j],which(colnames(MultiCategory) == Var_name)]) == Modde)]
    MultiCategory[Row.num[j],which(colnames(MultiCategory) == Var_name)] <- Var_imp$mode[j]
    
  }
}

if (ncol(Continuous) == ncol(NoMissingness)){
  Model_Dataset <- cbind.data.frame(Continuous, Binary, MultiCategory)
} else {
  Model_Dataset <- cbind.data.frame(NoMissingness, Continuous, Binary, MultiCategory)
}