#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(rhandsontable)
library(Rfast)
source("dataCleansing.R")

cleanser <- new("dataCleansing")
cleanser$initialize()
cleanser$dataCleanser(dataName2)
cleanser$cleansingForm
options_data <-  cleanser$cleansingForm$numeric[[1]]$table

inputedCol_numeric <- NULL
inputedCol_factora <- NULL
inputedCol_Date <-NULL
  
missList <- NULL
initData <- cleanser$cleansingForm$numeric[[1]]$table
if (nrow(initData) > 3) {
  missList <- initData[, 2][3 : (nrow(initData) - 1)]
}

searchColname <- function(cleansingForm, colName) {
    for (i in cleansingForm) {
        for (j in i) {
            if (j$colname == colName) {
              if (is.null(dim(j$table))) {
                table <- t(j$table)
                dimnames(table) <- list("table", NULL)
                return(table)
              }
              else {
                return(j$table)
              }
            }
        }
    }
}

getMissValues <- function(cleansingForm, colName) {
  row_startTable <- 3
  col_startTable <- 2
  table <- searchColname(cleansingForm, colName)
  if (nrow(table) > row_startTable) {
    return(table[row_startTable : (nrow(table) - 1), col_startTable])
  }
  else {
    return(character(0))
  }
}


getLevels <- function(cleansingForm, colName) {
  row_startTable <- 3
  col_startTable <- 3 
  col_replace <- 5
  table <- searchColname(cleansingForm, colName)
  return(table[row_startTable : (nrow(table) - 1), col_startTable])
}

getReplacedLevels <- function(cleansingForm, colName) {
  row_startTable <- 3
  col_startTable <- 3 
  col_replace <- 5
  table <- searchColname(cleansingForm, colName)
  isReplaced <- replace(table[, col_replace] != "", c(1 : (row_startTable - 1), nrow(table)), FALSE)
  for (i in seq_len(length(table[isReplaced, col_replace]))) {
    if (table[isReplaced, col_replace][i] == "N/A") {
      table[isReplaced, col_startTable][i] <- NA
    }
    else {
      table[isReplaced, col_startTable][i] <- table[isReplaced, col_replace][i]
    }
  }
  return(table[row_startTable : (nrow(table) - 1), col_startTable])
}

getPooledLevels <- function(cleansingForm, colName) {
  levels <- getReplacedLevels(cleansingForm, colName)
  pooledLevels <- NULL
  for (i in 2 : length(levels)) {
    temp <- NULL
    combinations <- RcppAlgos::comboGeneral(levels, i)
    for (j in seq_len(ncol(combinations))) {
      temp <- paste0(temp, "+", combinations[, j])
    }
    temp <- substr(temp, 2, nchar(temp))
    pooledLevels <- append(pooledLevels, temp)
    if (length(pooledLevels) > 100) {
      break
    }
  }
  return(pooledLevels)
}

delChoicesPool <- function(cleansingForm, colName, pool) {
  levels <- getLevels(cleansingForm, colName)
  pooledLevels <- strsplit(pool, "[+]")
  for (i in pooledLevels) {
    for (j in i) {
      levels <- levels[levels != j]
    }
  }
  if (length(levels) < 2) {
    pooledLevels <- NULL
  }
  else {
    pooledLevels <- NULL
    for (i in 2 : length(levels)) {
      temp <- NULL
      combinations <- RcppAlgos::comboGeneral(levels, i)
      for (j in seq_len(ncol(combinations))) {
        temp <- paste0(temp, "+", combinations[, j])
      }
      temp <- substr(temp, 2, nchar(temp))
      pooledLevels <- append(pooledLevels, temp)
      if (length(pooledLevels) > 100) {
        break
      }
    }
  }
  delPooledLevels <- append(pool, pooledLevels)
  return(delPooledLevels)
} 

mkRepPoolLevels <- function(cleansingForm, colName) {
  row_startTable <- 3
  col_startTable <- 7
  col_levelsOrder <- 2
  levels <- getReplacedLevels(cleansingForm, colName)
  table <- searchColname(cleansingForm, colName)
  
  isPooled <- replace(table[, col_startTable] != "", c(1 : (row_startTable - 1), nrow(table)), FALSE)
  poolList <- strsplit(table[isPooled, col_startTable], "[+]")
  repPoolLevels <- levels
  for (i in poolList) {
    levelsOrder <- gsub("[^0-9]", "", table[row_startTable : (nrow(table) - 1), col_levelsOrder])
    orderNchar <- order(nchar(dataLevels), decreasing = TRUE)
    temp <- NULL
    for (j in i) {
      temp <- paste0(temp, "+", levels[levelsOrder == j])
    }
    temp <- substr(temp, 2, nchar(temp))
    for(j in i) {
      repPoolLevels <- replace(repPoolLevels, levelsOrder == j, temp)
    }
  }
  return(repPoolLevels)
}

getRepPoolLevels <- function(cleansingForm, colName) {
  return(unique(mkRepPoolLevels(cleansingForm, colName)))
}

getRepPool_forOrder <- function(cleansingForm, colName) {
  levels <- mkRepPoolLevels(cleansingForm, colName)
  for (i in seq_len(length(levels))) {
    if (!is.na(levels[i])) {
      if (length(levels[levels == levels[i]]) > 0) {
        levels[levels == levels[i] & !is.na(levels)] <- replace(levels[levels == levels[i] & !is.na(levels)], c(FALSE, rep(TRUE, length(levels[levels == levels[i] & !is.na(levels)]) - 1)), NA)
      }
    }
  }
  return(levels)
}

getOrder <- function(cleansingForm, colName) {
  row_startTable <- 3
  col_startTable <- 9
  repPoolLevels <- getRepPool_forOrder(cleansingForm, colName)
  table <- searchColname(cleansingForm, colName)
  levelsOrder <- as.numeric(table[row_startTable : (nrow(table) - 1), col_startTable])
  selectedOrder <- NULL
  for (i in order(levelsOrder)) {
    if(!is.na(levelsOrder[i])) {
      selectedOrder <- append(selectedOrder, repPoolLevels[i])
    }
  }
  return(na.omit(selectedOrder))
}

isRepByMean <- function(cleansingForm, colName) {
  selected <- NULL
  row_startTable <- 3
  table <- searchColname(cleansingForm, colName)
  if (nrow(table) > 3) {
    for (i in (row_startTable : (nrow(table) - 1))) {
      print(table[i, 4])
      if (table[i, 4] == mean(na.omit(as.numeric(cleanser$dataset[, colName])))) {
        selected <- append(selected, table[i, 2]) 
      }
    }
  }
  if (is.null(selected)) {
    selected <- character(0)
  }
  return(selected)
}

isRepByNA <- function(cleansingForm, colName) {
  selected <- NULL
  row_startTable <- 3
  table <- searchColname(cleansingForm, colName)
  for (i in (row_startTable : (nrow(table) - 1))) {
    if (table[i, 5] == "N/A") {
      selected <- append(selected, table[i, 3]) 
    }
  }
  return(selected)
}

isCategorised <- function(cleansingForm, colName) {
  options(warn = -1)
  breaksLabels <- paste0(-Inf, ",", mean(na.omit(as.numeric(cleanser$dataset[, colName]))), ",", Inf)
  labels <- paste0("<", mean(na.omit(as.numeric(cleanser$dataset[, colName]))), ",", "≧", mean(na.omit(as.numeric(cleanser$dataset[, colName]))))
  if (searchColname(cleansingForm, colName)[3, 6] == breaksLabels & searchColname(cleansingForm, colName)[3, 8] == labels) {
    return(TRUE)    
  }
  else {
    return(FALSE)
  }
  options(warn = 0)
}

getColNames <- function(cleansingForm, colNames_type) {
  if (colNames_type == "numeric") {
    forms <- cleansingForm$numeric
  }
  else if (colNames_type == "factor") {
    forms <- cleansingForm$factor
  }
  else {
    forms <- cleansingForm$Date
  }
  names <- NULL
  for (i in forms) {
    names <- append(names, i$colname)
  } 
  return(names)
}

output_dataNumeric <- function(dataset, colName, newColName, selected, options, categorise, dataValues, session) {
  bottom_table <- as.data.frame(dataset[, colName])
  if (newColName != "") {
    colnames(bottom_table) <- searchColname(cleanser$cleansingForm, colName)[1, 2]
  }
  else {
    colnames(bottom_table) <- colName
  }
  
  if (!is.null(options$changes$changes)) {
    changedRow <- options$changes$changes[[1]][[1]]
    changedCol <- options$changes$changes[[1]][[2]]
    isColName <- changedRow == 0 & changedCol == 1
    isRepMissVal <- (changedRow > 1 & changedRow < (nrow(dataValues) - 1)) & changedCol == 3
    isCategorised <- changedRow == 2 & changedCol == 5 | changedRow == 2 & changedCol == 7 
    if (isColName | isRepMissVal | isCategorised) { 
      if (isColName) {
        colnames(bottom_table) <- options$changes$changes[[1]][[4]]
      }
      for (i in seq_len(length(cleanser$cleansingForm))) {
        for (j in seq_len(length(cleanser$cleansingForm[[i]]))) {
          if (cleanser$cleansingForm[[i]][[j]]$colname == colName) {
            if (isRepMissVal) {
              if (!isColChanged_repMissVal) {
                cleanser$cleansingForm[[i]][[j]]$table[changedRow + 1, changedCol + 1] <<- options$changes$changes[[1]][[4]]
              }
            }
            else {
              cleanser$cleansingForm[[i]][[j]]$table[changedRow + 1, changedCol + 1] <<- options$changes$changes[[1]][[4]]
            }
          }
        }
      }
    }
  }
  
  table <- searchColname(cleanser$cleansingForm, colName)
  
  row_startTable <- 3
  col_missVal <- 2
  col_repMissVal <- 4
  sel_inForm <- replace(table[, col_repMissVal] != "", c(1 : (row_startTable - 1), nrow(table)), FALSE)
  
  if (any(table[row_startTable : (nrow(table) - 1), col_repMissVal] != "")) {
    bottom_table[, 1] <- cleanser$replaceMissVal(bottom_table[, 1], replace(table, table == "", NA),  1)
  }
   
  col_categorise <- 6
  col_labels <- 8
  if (any(table[row_startTable : (nrow(table) - 1), col_categorise] != "")) {
    breaksLabels <- strsplit(table[row_startTable, col_categorise], ",")
    labels <- strsplit(table[row_startTable, col_labels], ",")
    if (length(unlist(breaksLabels)) == length(unlist(labels)) + 1 & length(unlist(labels)) > 0) {
      bottom_table[, 1] <- cleanser$cutting(as.numeric(bottom_table[, 1]), replace(table, table == "", NA), 1)
    }
  }
  #replaceMiss for form
 # for (i in seq_len(length(cleanser$cleansingForm))) {
#    for (j in seq_len(length(cleanser$cleansingForm[[i]]))) {
#      if (cleanser$cleansingForm[[i]][[j]]$colname == colName) {
      #  row_startTable <- 3
      #  col_startTable <- 2
      #  sel_inForm <- replace(cleanser$cleansingForm[[i]][[j]]$table[, col_startTable + 2] != "", c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
      #  for (k in seq_len(length(cleanser$cleansingForm[[i]][[j]]$table[sel_inForm, col_startTable]))) { # todo rewrite with replaceMiss from class cleanser
      #    bottom_table[, 1] <- replace(bottom_table[, 1], bottom_table[, 1] == cleanser$cleansingForm[[i]][[j]]$table[sel_inForm, col_startTable][k], cleanser$cleansingForm[[i]][[j]]$table[sel_inForm, col_startTable + 2][k])
      #  }
      #  if (searchColname(cleanser$cleansingForm, colName)[3, 6] != "") {
      #    breaksLabels <- strsplit(searchColname(cleanser$cleansingForm, colName)[3, 6], ",")
      #    labels <- strsplit(searchColname(cleanser$cleansingForm, colName)[3, 8], ",")
      #    if (length(unlist(breaksLabels)) == length(unlist(labels)) + 1 & length(unlist(labels)) > 0) {
      #      bottom_table[, 1] <- cleanser$cutting(as.numeric(bottom_table[, 1]), searchColname(cleanser$cleansingForm, colName), 1)
      #    }
      #  }
      #}
  #  }
  #}
  return(bottom_table)
}

output_dataFactor <- function(dataset, colName, newColName, selected, options, session) {
  table <- searchColname(cleanser$cleansingForm, colName)
  bottom_table <- as.data.frame(dataset[, colName])
  if (newColName != "") {
    colnames(bottom_table) <- table[1, 2]
  }
  else {
    colnames(bottom_table) <- colName
  }
  
  if (!is.null(options$changes$changes)) {
    changedRow <- options$changes$changes[[1]][[1]]
    changedCol <- options$changes$changes[[1]][[2]]
    isColName <- changedRow == 0 & changedCol == 1
    isRepByNA <- (changedRow > 1 & changedRow < (nrow(table) - 1)) & changedCol == 4
    isPooled <- (changedRow > 1 & changedRow < (nrow(table) - 1)) & changedCol == 6
    isOrdered <- (changedRow > 1 & changedRow < (nrow(table) - 1)) & changedCol == 8
    if (isColName | isRepByNA | isPooled | isOrdered) { 
      if (isColName) {
        colnames(bottom_table) <- options$changes$changes[[1]][[4]]
      }
      for (i in seq_len(length(cleanser$cleansingForm))) {
        for (j in seq_len(length(cleanser$cleansingForm[[i]]))) {
          if (cleanser$cleansingForm[[i]][[j]]$colname == colName) {
            cleanser$cleansingForm[[i]][[j]]$table[changedRow + 1, changedCol + 1] <<- options$changes$changes[[1]][[4]]
            updateSelectInput(session, "pool", "Select any combinations of pooled levels", choices = getPooledLevels(cleanser$cleansingForm, colName))
          }
        }
      }
    }
  }
  table <- searchColname(cleanser$cleansingForm, colName)
  repPoolLevels <- getRepPoolLevels(cleanser$cleansingForm, colName)
  
  row_startTable <- 3
  col_repTable <- 5
  if (any(table[row_startTable : (nrow(table) - 1), col_repTable] != "")) {
    bottom_table[, 1] <- cleanser$replacer(bottom_table[, 1], replace(table, table == "", NA),  1)
  }

  col_poolTable <- 7
  canPool <- TRUE
  for(i in seq_len(length(repPoolLevels))) {
    ncharPool <- nchar(table[row_startTable : (nrow(table) - 1), col_poolTable])
    dupCheck_ncharPool <- nchar(gsub(i, "", table[row_startTable : (nrow(table) - 1), col_poolTable]))
    if (length(repPoolLevels[(ncharPool == dupCheck_ncharPool) == FALSE]) != 1) {
      canPool <- FALSE
      break
    }
  }
  if (any(table[row_startTable : (nrow(table) - 1), col_poolTable] != "") & canPool) {
    bottom_table[, 1] <- cleanser$pooler(bottom_table[, 1], replace(table, table == "", NA),  1)
  }
  
  col_orderTable <- 9
  canOrder <- all(na.omit(as.numeric(table[row_startTable : (nrow(table) - 1), col_orderTable],is.na(table[row_startTable : (nrow(table) - 1), col_orderTable]), 0) <= length(repPoolLevels)))
  if (any(table[row_startTable : (nrow(table) - 1), col_orderTable] != "") & canOrder) {
    bottom_table[, 1] <- cleanser$orderer(bottom_table[, 1], replace(table, table == "", NA),  1)
  }
  
  return(bottom_table)
}

output_dataDate <- function(dataset, colName, newColName) {
  table <- searchColname(cleanser$cleansingForm, colName)
  bottom_table <- as.data.frame(dataset[, colName])
  if (newColName != "") {
    colnames(bottom_table) <- table[1, 2]
  }
  else {
    colnames(bottom_table) <- colName
  }
  return(bottom_table)
}

init_inputCol <- function(inputCol) {
  inputCol_dataFrame <- data.frame(colname = rep("", length(inputCol)), inputed = rep("", length(inputCol)))
  for (i in seq_len(length(inputCol))) {
    inputCol_dataFrame$colname[i] <- inputCol[i]
    inputCol_dataFrame$inputed[i] <- ""
  }
  return(inputCol_dataFrame)
}

replaceColname <- function(colName, newColName) {
  for (i in cleanser$cleansingForm) {
    for (j in i) {
      if (j$colname == colName) {
        j$table[1, 2] <<- newColName
      }
    }
  }
}

replaceWithMean <- function(colName, selected) {
  row_startTable <- 3
  col_startTable <- 2
  col_replacedWith <- 4
  for (i in seq_len(length(cleanser$cleansingForm))) {
    for (j in seq_len(length(cleanser$cleansingForm[[i]]))) {
      if (cleanser$cleansingForm[[i]][[j]]$colname == colName) {
        all_selected <- rep(FALSE, nrow(cleanser$cleansingForm[[i]][[j]]$table))
        for (k in selected) {
          is_selected <- replace(cleanser$cleansingForm[[i]][[j]]$table[, col_startTable] == k, c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
          cleanser$cleansingForm[[i]][[j]]$table[is_selected,  col_replacedWith] <<- mean(na.omit(as.numeric(cleanser$dataset[, colName])))
          all_selected <- all_selected | is_selected
        }
        not_inForm <- replace(cleanser$cleansingForm[[i]][[j]]$table[, col_startTable + 2] == mean(na.omit(as.numeric(cleanser$dataset[, colName]))), c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
        not_selected <- replace(!all_selected & not_inForm, c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
        print(paste0("na:", cleanser$cleansingForm[[i]][[j]]$table[not_selected,  col_replacedWith], ": " ,isColChanged_repMissVal))
        if (!isColChanged_repMissVal) {
        cleanser$cleansingForm[[i]][[j]]$table[not_selected,  col_replacedWith] <<- ""
        }
      }
    }
  }
  isColChanged_repMissVal <<- FALSE
}

replaceWithNA <- function(colName, selected) {
  row_startTable <- 3
  col_startTable <- 3
  col_replacedWith <- 5
  for (i in seq_len(length(cleanser$cleansingForm))) {
    for (j in seq_len(length(cleanser$cleansingForm[[i]]))) {
      if (cleanser$cleansingForm[[i]][[j]]$colname == colName) {
        all_selected <- rep(FALSE, nrow(cleanser$cleansingForm[[i]][[j]]$table))
        for (k in selected) {
          is_selected <- replace(cleanser$cleansingForm[[i]][[j]]$table[, col_startTable] == k, c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
          cleanser$cleansingForm[[i]][[j]]$table[is_selected,  col_replacedWith] <<- "N/A"
          all_selected <- all_selected | is_selected
        }
        not_inForm <- replace(cleanser$cleansingForm[[i]][[j]]$table[, col_replacedWith] == "N/A", c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
        not_selected <- replace(!all_selected & not_inForm, c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
        cleanser$cleansingForm[[i]][[j]]$table[not_selected,  col_replacedWith] <<- ""
      }
    }
  }
}

categorise <- function(colName, categorise) {
  row_startTable <- 3
  col_startTable <- 6
  options(warn = -1)
  for (i in seq_len(length(cleanser$cleansingForm))) {
    for (j in seq_len(length(cleanser$cleansingForm[[i]]))) {
      if (cleanser$cleansingForm[[i]][[j]]$colname == colName) {
        breaksLabels <- paste0(-Inf, ",", mean(na.omit(as.numeric(cleanser$dataset[, colName]))), ",", Inf)
        labels <- paste0("<", mean(na.omit(as.numeric(cleanser$dataset[, colName]))), ",", "≧", mean(na.omit(as.numeric(cleanser$dataset[, colName]))))
        if (categorise) {
          cleanser$cleansingForm[[i]][[j]]$table[row_startTable, col_startTable] <<- breaksLabels
          cleanser$cleansingForm[[i]][[j]]$table[row_startTable, col_startTable + 2] <<- labels 
        }
        else {
          cleanser$cleansingForm[[i]][[j]]$table[row_startTable, col_startTable] <<- ""
          cleanser$cleansingForm[[i]][[j]]$table[row_startTable, col_startTable + 2] <<- ""
        }
      }
    }
  }
  options(warn = 0)
}

poolLevels <- function(colName, pool, order, session) {
  row_startTable <- 3
  col_replace <- 5
  col_startTable <- 7
  col_levelsOrder <- 2
  selectedPools <- unique(mkRepPoolLevels(cleanser$cleansingForm, colName)[grep("[+]", mkRepPoolLevels(cleanser$cleansingForm, colName))])
  for (i in seq_len(length(cleanser$cleansingForm))) {
    for (j in seq_len(length(cleanser$cleansingForm[[i]]))) {
      if (cleanser$cleansingForm[[i]][[j]]$colname == colName) {
        dataLevels <- getReplacedLevels(cleanser$cleansingForm, colName)
        levelsOrder <- gsub("[^0-9]", "", cleanser$cleansingForm[[i]][[j]]$table[row_startTable : (nrow(cleanser$cleansingForm[[i]][[j]]$table) - 1), col_levelsOrder])
        orderNchar <- order(nchar(dataLevels), decreasing = TRUE)
        if (length(pool) == 0 & isColChanged_pool) {
          pool <- selectedPools
        }
        for (k in seq_len(length(dataLevels))) {
          pool <- gsub(dataLevels[orderNchar][k], levelsOrder[orderNchar][k], pool)
        }
        isPooled <- replace(cleanser$cleansingForm[[i]][[j]]$table[, col_startTable] == "", c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
        notPooled <- setdiff(cleanser$cleansingForm[[i]][[j]]$table[row_startTable : (nrow(cleanser$cleansingForm[[i]][[j]]$table) - 1), col_startTable], pool)
        if (!is.null(pool)) {
          for (k in pool) {
            if (all(k != cleanser$cleansingForm[[i]][[j]]$table[, col_startTable])) {
              cleanser$cleansingForm[[i]][[j]]$table[min(grep(TRUE, isPooled)), col_startTable] <<- k
            }
          }
        }
        for (k in notPooled) {
            row_notPooled <- cleanser$cleansingForm[[i]][[j]]$table[, col_startTable] == k
            cleanser$cleansingForm[[i]][[j]]$table[row_notPooled, col_startTable] <<- ""
        }
      }
    }
  }
  if (length(pool) == 0 & !isColChanged_pool) {
    updateSelectInput(session, "pool", "Select any combinations of pooled levels", choices = getPooledLevels(cleanser$cleansingForm, colName), selected = pool)
    numOrder <- mkOrdinal(order) 
    print("c")
    updateSelectInput(session, "order", paste0("Select as the ", numOrder, " order"), choices = getRepPoolLevels(cleanser$cleansingForm, colName), selected = order)
  }
  isColChanged_pool <<- FALSE
}

orderLevels <- function(colName, order, session) {
  row_startTable <- 3
  col_replace <- 5
  col_pool <- 7
  col_startTable <- 9
  col_levels <- 3
  selectedOrder <- getOrder(cleanser$cleansingForm, colName)
  for (i in seq_len(length(cleanser$cleansingForm))) {
    for (j in seq_len(length(cleanser$cleansingForm[[i]]))) {
      if (cleanser$cleansingForm[[i]][[j]]$colname == colName) {
        isReplaced <- any(cleanser$cleansingForm[[i]][[j]]$table[row_startTable : (nrow(cleanser$cleansingForm[[i]][[j]]$table) - 1), col_replace] == "")
        isPooled <- any(cleanser$cleansingForm[[i]][[j]]$table[row_startTable : (nrow(cleanser$cleansingForm[[i]][[j]]$table) - 1), col_pool] == "")
        
        repPoolLevels <- getRepPool_forOrder(cleanser$cleansingForm, colName)
        all_ordered <- rep(FALSE, nrow(cleanser$cleansingForm[[i]][[j]]$table))
        if (isColChanged_order) {
          order <- selectedOrder
        }
        for (k in seq_len(length(order))) {
          isOrdered <- c(rep(FALSE, row_startTable - 1), repPoolLevels == order[k], FALSE)
          isOrdered <- replace(isOrdered, is.na(isOrdered), FALSE)
          all_ordered <- all_ordered | isOrdered
          cleanser$cleansingForm[[i]][[j]]$table[isOrdered, col_startTable] <<- k 
        }
        notOrdered <- replace(!all_ordered, c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
        cleanser$cleansingForm[[i]][[j]]$table[notOrdered, col_startTable] <<- ""
      }
    }
  }
  if (length(order) == 0) {
    numOrder <- mkOrdinal(order) 
    updateSelectInput(session, "order", paste0("Select as the ", numOrder, " order"), choices = getRepPoolLevels(cleanser$cleansingForm, colName), selected = order)
  }
  isColChanged_order <<- FALSE
}

changeColName <- function(colName, newColName) {
  for (i in seq_len(length(cleanser$cleansingForm))) {
    for (j in seq_len(length(cleanser$cleansingForm[[i]]))) {
      if (cleanser$cleansingForm[[i]][[j]]$colname == colName) {
        if (newColName == "") {
          cleanser$cleansingForm[[i]][[j]]$table[1, 2] <<- ""
          inputedCols[[i]]$inputed[inputedCols[[i]]$colname == colName] <<- ""
        }
        else {
          cleanser$cleansingForm[[i]][[j]]$table[1, 2] <<- newColName 
          inputedCols[[i]]$inputed[inputedCols[[i]]$colname == colName] <<- newColName
        }
        return()
      }
    }
  }
}

right_tableManager <- function(dataValues, colName, newColName) {
  if (newColName != "") {
    dataFrame <- searchColname(cleanser$cleansingForm, colName)
    dataFrame[1, 2] <- newColName
    dataValues$data <- dataFrame
  }
  else {
    dataValues$data <- searchColname(cleanser$cleansingForm, colName)
  }
  return(dataValues$data)
}

mkOrdinal <- function(order) {
  if (length(order) + 1 == 1) {
    ordinal <- "st"
  }
  else if (length(order) + 1 == 2) {
    ordinal <- "nd"
  }
  else if (length(order) + 1 == 3) {
    ordinal <- "rd"
  }
  else {
    ordinal <- "th"
  }
  return(paste0(length(order) + 1, ordinal))
}

output_cleansingForm <- function(dataName, file, cleansingForm) {
  formNumeric <-  as.data.frame(matrix(NA, ncol = 8))[numeric(0), ]
  formFactor <- as.data.frame(matrix(NA, ncol = 9))[numeric(0), ]
  formDate <- as.data.frame(matrix(NA, ncol = 2))[numeric(0), ]
  for (i in seq_len(length(cleansingForm))) {
    for (j in seq_len(length(cleansingForm[[i]]))) {
      if (i == 1) {
        formNumeric <- rbind(formNumeric, cleansingForm[[i]][[j]]$table)
      }
      else if(i == 2) {
        formFactor <- rbind(formFactor, cleansingForm[[i]][[j]]$table)
      } 
      else {
        formDate <- rbind(formDate, cleansingForm[[i]][[j]]$table)
      }
    }
  }
  cleanser$writeTablesOnExcel(formNumeric, formFactor, formDate, "data")
  cleansedData <- cleanser$dataCleanser(dataName2)
  save(cleansedData, file = "data.rda")
  zip(zipfile = file, files = c(paste0("dataCleansingForm_", dataName, "_.xlsx"), "data.rda"))
}

inputedCol_numeric <- init_inputCol(getColNames(cleanser$cleansingForm, "numeric"))
inputedCol_factor <- init_inputCol(getColNames(cleanser$cleansingForm, "factor"))
inputedCol_Date <- init_inputCol(getColNames(cleanser$cleansingForm, "Date"))

inputedCols <- list(inputedCol_numeric, inputedCol_factor, inputedCol_Date) 

isColChanged_pool <- FALSE
isColChanged_orde <- FALSE
isColChanged_repMissVal <- FALSE

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Visual Data-Cleansing Form"),
  div(style = "position:absolute;right:1em;",
  downloadButton("downloadSave", "Execute and download")
  ),
  tabsetPanel(
    tabPanel("numeric", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          selectInput("colName", "Choose a colname:", 
                      choices = getColNames(cleanser$cleansingForm, "numeric"),
          ),
          textInput("newColName", "Change the colname to", ""),
          checkboxGroupInput("numeric_NA", "Missing-values replace with the mean", choices = getMissValues(cleanser$cleansingForm, cleanser$cleansingForm[[1]][[1]]$colname), selected = isRepByMean(cleanser$cleansingForm, colnames(cleanser$dataset)[1]), width = "200px"),
          strong("Whether to categorise numerical values into ≧ the mean and < the mean or not"),
          checkboxInput("categorise", "Categorise", value = isCategorised(cleanser$cleansingForm, colnames(cleanser$dataset)[1]))
        ),
          # Show a plot of the generated distribution
        mainPanel(
          h4("form"),
          rHandsontableOutput("options"),
          h4("data"),
          dataTableOutput("view"),
        )
      )
    ),
    tabPanel("factor", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          selectInput("colNameFactor", "Choose a colname:",
                      choices = getColNames(cleanser$cleansingForm, "factor"),
          ),
          textInput("newColNameFactor", "Change the colname to", ""),
          checkboxGroupInput("factor_NA", "Choose any levels you want to replace with NA", choices = getLevels(cleanser$cleansingForm, cleanser$cleansingForm[[2]][[1]]$colname), width = "200px"),
          selectInput("pool", "Select any combinations of pooled levels", choices = getPooledLevels(cleanser$cleansingForm, cleanser$cleansingForm[[2]][[1]]$colname), multiple = TRUE),
          selectInput("order", "Select as the 1st level", choices = getRepPoolLevels(cleanser$cleansingForm, cleanser$cleansingForm[[2]][[1]]$colname),  multiple = TRUE)
        ),
        mainPanel(
          h4("form"),
          rHandsontableOutput("optionsFactor"),
          h4("data_factor"),
          dataTableOutput("viewFactor")
        )
      )
    ),
    tabPanel("Date", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          selectInput("colNameDate", "Choose a colname:",
                      choices = getColNames(cleanser$cleansingForm, "Date"),
          ),
          textInput("newColNameDate", "Change the colname to", ""),
        ),
        mainPanel(
          h4("form"),
          rHandsontableOutput("optionsDate"),
          h4("data_factor"),
          dataTableOutput("viewDate")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  dataset <- data.frame()
  datasetFactor <- data.frame()
  datasetDate <- data.frame()
  options_data <- data.frame()
  options_dataFactor <- data.frame()
  options_dataDate <- data.frame()
  dataValues <- reactiveValues(data = options_data)
  dataValuesFactor <- reactiveValues(data = options_dataFactor)
  dataValuesDate <- reactiveValues(data = options_dataDate)
  
  
  observe(
    {
      changeColName(input$colName, input$newColName)
      changeColName(input$colNameFactor, input$newColNameFactor)
      changeColName(input$colNameDate, input$newColNameDate)
      replaceWithMean(input$colName, input$numeric_NA)
      replaceWithNA(input$colNameFactor, input$factor_NA) 
      categorise(input$colName, input$categorise)
      poolLevels(input$colNameFactor, input$pool, input$order, session)
      orderLevels(input$colNameFactor, input$order, session)
      
      dataValues$data <- right_tableManager(dataValues, input$colName, input$newColName)
      dataValuesFactor$data <- right_tableManager(dataValuesFactor, input$colNameFactor, input$newColNameFactor)
      dataValuesDate$data <- right_tableManager(dataValuesDate, input$colNameDate, input$newColNameDate)
    }
  )
  observeEvent(
    input$colName,{
      updateTextInput(session, "newColName", "Change the colname to", searchColname(cleanser$cleansingForm, input$colName)[1, 2])
      isColChanged_repMissVal <<- TRUE
      updateCheckboxGroupInput(session, "numeric_NA", "Choose any missing-values you want to replace with the mean", choices = getMissValues(cleanser$cleansingForm, input$colName), selected = isRepByMean(cleanser$cleansingForm, input$colName))
      updateCheckboxInput(session, "categorise", "Categorise", value = isCategorised(cleanser$cleansingForm, input$colName))
      print(isRepByMean(cleanser$cleansingForm, colName))
  })
  observeEvent(
    input$colNameFactor,{
      updateTextInput(session, "newColNameFactor", "Change the colname to", searchColname(cleanser$cleansingForm, input$colNameFactor)[1, 2])
      updateCheckboxGroupInput(session, "factor_NA", "Choose any levels you want to replace with NA", choices = getLevels(cleanser$cleansingForm, input$colNameFactor), selected = isRepByNA(cleanser$cleansingForm, input$colNameFactor))
      selectedPools <-  unique(mkRepPoolLevels(cleanser$cleansingForm, input$colNameFactor)[grep("[+]", mkRepPoolLevels(cleanser$cleansingForm, input$colNameFactor))])
      updateSelectInput(session, "pool", "Select any combinations of pooled levels", choices = delChoicesPool(cleanser$cleansingForm, input$colNameFactor, selectedPools), selected = selectedPools)
      selectedOrder <- getOrder(cleanser$cleansingForm, input$colNameFactor)
      numOrder <- mkOrdinal(selectedOrder) 
      updateSelectInput(session, "order", paste0("Select as the ", numOrder, " order"), choices = getRepPoolLevels(cleanser$cleansingForm, input$colNameFactor), selected = selectedOrder)
      isColChanged_pool <<- TRUE
      isColChanged_order <<- TRUE
    }
  )
  observeEvent(
    input$pool,{
      updateSelectInput(session, "pool", "Select any combinations of pooled levels", choices = delChoicesPool(cleanser$cleansingForm, input$colNameFactor, input$pool), selected = input$pool)
      print("a")
      updateSelectInput(session, "order", paste0("Select as the 1st order"), choices = getRepPoolLevels(cleanser$cleansingForm, input$colNameFactor), selected = character(0))
    }
  )
  observeEvent(
    input$order,{
      numOrder <- mkOrdinal(input$order) 
      print("b")
      updateSelectInput(session, "order", paste0("Select as the ", numOrder, " order"), choices = getRepPoolLevels(cleanser$cleansingForm, input$colNameFactor), selected = input$order)
    }
  )

  output$options <- renderRHandsontable({ 
    rhandsontable(dataValues$data)
  })
  output$optionsFactor <- renderRHandsontable({ 
    rhandsontable(dataValuesFactor$data)
  })
  output$optionsDate <- renderRHandsontable({ 
    rhandsontable(dataValuesDate$data)
  })
  
  output$view <- renderDataTable({
    dataset <- output_dataNumeric(cleanser$dataset, input$colName, input$newColName, input$numeric_NA, input$options, input$categorise, dataValues$data, session)
  })
  output$viewFactor <- renderDataTable({
    datasetFactor <- output_dataFactor(cleanser$dataset, input$colNameFactor, input$newColNameFactor, input$factor_NA, input$optionsFactor, session)
  })
  output$viewDate <- renderDataTable({
    datasetDate <- output_dataDate(cleanser$dataset, input$colNameDate, input$newColNameDate)
  })
  
  output$download <- downloadHandler(
    filename = paste0("data", ".zip"),
    content = function(file) {
      output_cleansingForm(file, cleansingForm)
    },
    contentType = "application/zip"
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

