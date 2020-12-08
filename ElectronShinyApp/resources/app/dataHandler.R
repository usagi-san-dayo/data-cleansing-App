source("UsagiSan.R")
cleanser <- new("dataCleansing")
cleanser$initialize()
isColChanged_repMissVal <- TRUE
isColChanged_repNA <- TRUE
isColChanged_pool <- TRUE
isColChanged_order <- TRUE
out_formnumeric <- FALSE
out_formfactor <- FALSE

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
  if (is.null(table)) {
    return(character(0))
  }
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
  if (is.null(table)) {
    return(NULL)
  }
  return(table[row_startTable : (nrow(table) - 1), col_startTable])
}

getReplacedLevels <- function(cleansingForm, colName) {
  row_startTable <- 3
  col_startTable <- 3 
  col_replace <- 5
  table <- searchColname(cleansingForm, colName)
  if (is.null(table)) {
    return(NULL)
  }
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
  levels <- na.omit(getReplacedLevels(cleansingForm, colName))
  if (is.null(levels) | length(levels) < 2) {
    return(character(0))
  }
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
  if (is.null(pool)) {
    pool <- character(0)
  }
  levels <- na.omit(getReplacedLevels(cleansingForm, colName))
  pooledLevels <- strsplit(pool, "[+]")
  if (is.null(levels)) {
    return(NULL)
  }
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
  if (is.null(table)) {
    return(NULL)
  }
  isPooled <- replace(table[, col_startTable] != "", c(1 : (row_startTable - 1), nrow(table)), FALSE)
  poolList <- strsplit(table[isPooled, col_startTable], "[+]")
  repPoolLevels <- levels
  for (i in poolList) {
    levelsOrder <- gsub("[^0-9]", "", table[row_startTable : (nrow(table) - 1), col_levelsOrder])
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
  if (is.null(levels)) {
    return(NULL)
  }
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
  if (is.null(table)) {
    return(NULL)
  }
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
  if (is.null(table) | any(dim(cleanser$dataset) == c(0, 0))) {
    return(NULL)
  }
  if (nrow(table) > 3) {
    for (i in (row_startTable : (nrow(table) - 1))) {
      options(warn = -1)
      if (table[i, 4] == mean(na.omit(as.numeric(cleanser$dataset[, colName])))) {
        selected <- append(selected, table[i, 2]) 
      }
      options(warn = 0)
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
  if (is.null(table)) {
    return(NULL)
  }
  for (i in (row_startTable : (nrow(table) - 1))) {
    if (table[i, 5] == "N/A") {
      selected <- append(selected, table[i, 3]) 
    }
  }
  return(selected)
}

isCategorised <- function(cleansingForm, colName) {
  if (any(dim(cleanser$dataset) == c(0, 0))) {
    return(NULL)
  }
  options(warn = -1)
  breaksLabels <- paste0(-Inf, ",", mean(na.omit(as.numeric(cleanser$dataset[, colName]))), ",", Inf)
  labels <- paste0("<", mean(na.omit(as.numeric(cleanser$dataset[, colName]))), ",", "\u2265", mean(na.omit(as.numeric(cleanser$dataset[, colName]))))
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

output_dataNumeric <- function(dataset, colName, newColName, selected, options, session) {
  table <- searchColname(cleanser$cleansingForm, colName)
  bottom_table <- as.data.frame(dataset[, colName])
  if (newColName != "") {
    colnames(bottom_table) <- newColName
  }
  else {
    colnames(bottom_table) <- colName
  }
  tempColName <- newColName
  tempSelected <- selected
  tempOptions <- options
  
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
  return(bottom_table)
}

output_dataFactor <- function(dataset, colName, newColName, selected, pool, order, options, session) {
  table <- searchColname(cleanser$cleansingForm, colName)
  bottom_table <- as.data.frame(dataset[, colName])
  if (newColName != "") {
    colnames(bottom_table) <- table[1, 2]
  }
  else {
    colnames(bottom_table) <- colName
  }
  tempColName <- colName
  tempSelected <- selected
  tempPool <- pool
  tempOrder <- order
  tempOptions <- options
  
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
    if (length(repPoolLevels[!(ncharPool == dupCheck_ncharPool)]) > 1) {
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

output_dataDate <- function(dataset, colName, newColName, options) {
  table <- searchColname(cleanser$cleansingForm, colName)
  bottom_table <- as.data.frame(dataset[, colName])
  if (newColName != "") {
    colnames(bottom_table) <- table[1, 2]
  }
  else {
    colnames(bottom_table) <- colName
  }
  tempColName <- colName
  tempOptions <- options
  return(bottom_table)
}

formHandler_numeric <- function(options, colName) {
  if (!is.null(cleanser$cleansingForm$numeric) & any(dim(cleanser$dataset) != c(0, 0)) & colName != "") {
    table <- searchColname(cleanser$cleansingForm, colName)
    if (!is.null(options$changes$changes)) {
      changedRow <- options$changes$changes[[1]][[1]]
      changedCol <- options$changes$changes[[1]][[2]]
      isColName <- changedRow == 0 & changedCol == 1
      isRepMissVal <- (changedRow > 1 & changedRow < (nrow(table) - 1)) & changedCol == 3
      isCategorised <- changedRow == 2 & changedCol == 5 | changedRow == 2 & changedCol == 7 
      if (isColName | isRepMissVal | isCategorised) { 
        for (i in seq_len(length(cleanser$cleansingForm))) {
          for (j in seq_len(length(cleanser$cleansingForm[[i]]))) {
            if (cleanser$cleansingForm[[i]][[j]]$colname == colName) {
              if (!isColChanged_repMissVal) {
                cleanser$cleansingForm[[i]][[j]]$table[changedRow + 1, changedCol + 1] <<- options$changes$changes[[1]][[4]]
              }
            }
          }
        }
      }
    }
  }
}

formHandler_factor <- function(options, colName) {
  if (!is.null(cleanser$cleansingForm$factor) & any(dim(cleanser$dataset) != c(0, 0)) & colName != "") {
    table <- searchColname(cleanser$cleansingForm, colName)
    if (!is.null(options$changes$changes)) {
      changedRow <- options$changes$changes[[1]][[1]]
      changedCol <- options$changes$changes[[1]][[2]]
      isColName <- changedRow == 0 & changedCol == 1
      isRepByNA <- (changedRow > 1 & changedRow < (nrow(table) - 1)) & changedCol == 4
      isPooled <- (changedRow > 1 & changedRow < (nrow(table) - 1)) & changedCol == 6
      isOrdered <- (changedRow > 1 & changedRow < (nrow(table) - 1)) & changedCol == 8
      if (isColName | isRepByNA | isPooled | isOrdered) { 
        for (i in seq_len(length(cleanser$cleansingForm))) {
          for (j in seq_len(length(cleanser$cleansingForm[[i]]))) {
            if (cleanser$cleansingForm[[i]][[j]]$colname == colName) {
              if (!isColChanged_repNA & !isColChanged_pool & !isColChanged_order) {
                cleanser$cleansingForm[[i]][[j]]$table[changedRow + 1, changedCol + 1] <<- options$changes$changes[[1]][[4]]
              }
            }
          }
        }
      }
    }
  }
}

formHandler_Date <- function(options, colName) {
  if (!is.null(cleanser$cleansingForm$factor) & any(dim(cleanser$dataset) != c(0, 0)) & colName != "") {
    table <- searchColname(cleanser$cleansingForm, colName)
    if (!is.null(options$changes$changes)) {
      changedRow <- options$changes$changes[[1]][[1]]
      changedCol <- options$changes$changes[[1]][[2]]
      isColName <- changedRow == 0 & changedCol == 1
      if (isColName) { 
        for (i in seq_len(length(cleanser$cleansingForm))) {
          for (j in seq_len(length(cleanser$cleansingForm[[i]]))) {
            if (cleanser$cleansingForm[[i]][[j]]$colname == colName) {
                cleanser$cleansingForm[[i]][[j]]$table[changedRow + 1, changedCol + 1] <<- options$changes$changes[[1]][[4]]
            }
          }
        }
      }
    }
  }
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
        sel <- selected
        if (isColChanged_repMissVal) {  
          sel <- isRepByMean(cleanser$cleansingForm, colName)
        }
        options(warn = -1)
        for (k in sel) {
          is_selected <- replace(cleanser$cleansingForm[[i]][[j]]$table[, col_startTable] == k, c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
          cleanser$cleansingForm[[i]][[j]]$table[is_selected,  col_replacedWith] <<- mean(na.omit(as.numeric(cleanser$dataset[, colName])))
          all_selected <- all_selected | is_selected
        }
        not_inForm <- replace(cleanser$cleansingForm[[i]][[j]]$table[, col_startTable + 2] == mean(na.omit(as.numeric(cleanser$dataset[, colName]))), c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
        options(warn = 0)
        not_selected <- replace(!all_selected & not_inForm, c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
        if (!isColChanged_repMissVal) {  
          cleanser$cleansingForm[[i]][[j]]$table[not_selected,  col_replacedWith] <<- ""
        }
      }
    }
  }
}

replaceWithNA <- function(colName, selected, pool, session) {
  row_startTable <- 3
  col_startTable <- 3
  col_replacedWith <- 5
  col_poolTable <- 7
  table <- searchColname(cleanser$cleansingForm, colName)
  if (is.null(table)) {
    return()
  }
  if (any(table[row_startTable : (nrow(table) - 1), col_poolTable] != "" ) & length(pool) > 0 & length(selected) != length(isRepByNA(cleanser$cleansingForm, colName))) {
    showNotification("Remove all selected pools before replacing")
  }
  else {
    for (i in seq_len(length(cleanser$cleansingForm))) {
      for (j in seq_len(length(cleanser$cleansingForm[[i]]))) {
        if (cleanser$cleansingForm[[i]][[j]]$colname == colName) {
          sel <- selected
          if (isColChanged_repNA) {
            sel <- isRepByNA(cleanser$cleansingForm, colName)
          }
          all_selected <- rep(FALSE, nrow(cleanser$cleansingForm[[i]][[j]]$table))
          for (k in sel) {
            is_selected <- replace(cleanser$cleansingForm[[i]][[j]]$table[, col_startTable] == k, c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
            cleanser$cleansingForm[[i]][[j]]$table[is_selected,  col_replacedWith] <<- "N/A"
            all_selected <- all_selected | is_selected
          }
          not_inForm <- replace(cleanser$cleansingForm[[i]][[j]]$table[, col_replacedWith] == "N/A", c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
          not_selected <- replace(!all_selected & not_inForm, c(1 : (row_startTable - 1), nrow(cleanser$cleansingForm[[i]][[j]]$table)), FALSE)
          if (!isColChanged_repNA) {  
            cleanser$cleansingForm[[i]][[j]]$table[not_selected,  col_replacedWith] <<- ""
          }
        }
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
        labels <- paste0("<", mean(na.omit(as.numeric(cleanser$dataset[, colName]))), ",", "\u2265", mean(na.omit(as.numeric(cleanser$dataset[, colName]))))
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
          if (!is.na(dataLevels[orderNchar][k])) {
            pool <- gsub(dataLevels[orderNchar][k], levelsOrder[orderNchar][k], pool)
          }
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
    updateSelectInput(session, "order", paste0("Select as the ", numOrder, " order"), choices = na.omit(getRepPoolLevels(cleanser$cleansingForm, colName)), selected = order)
  }
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
    updateSelectInput(session, "order", paste0("Select as the ", numOrder, " order"), choices = na.omit(getRepPoolLevels(cleanser$cleansingForm, colName)), selected = order)
  }
}

changeColName_numeric <- function(colName, newColName) {
  if(!is.null(searchColname(cleanser$cleansingForm, colName))) {
    for (i in seq_len(length(cleanser$cleansingForm$numeric))) {
      if (cleanser$cleansingForm$numeric[[i]]$colname == colName) {
        if (newColName == "") {
          cleanser$cleansingForm$numeric[[i]]$table[1, 2] <<- ""
        }
        else {
          cleanser$cleansingForm$numeric[[i]]$table[1, 2] <<- newColName 
        }
        return()
      }
    }
  }
}

changeColName_factor <- function(colName, newColName) {
  if(!is.null(searchColname(cleanser$cleansingForm, colName))) {
    for (i in seq_len(length(cleanser$cleansingForm$factor))) {
      if (cleanser$cleansingForm$factor[[i]]$colname == colName) {
        if (newColName == "") {
          cleanser$cleansingForm$factor[[i]]$table[1, 2] <<- ""
        }
        else {
          cleanser$cleansingForm$factor[[i]]$table[1, 2] <<- newColName 
        }
        return()
      }
    }
  }
}

changeColName_Date <- function(colName, newColName) {
  if(!is.null(searchColname(cleanser$cleansingForm, colName))) {
    for (i in seq_len(length(cleanser$cleansingForm$Date))) {
      if (cleanser$cleansingForm$Date[[i]]$colname == colName) {
        if (newColName == "") {
          cleanser$cleansingForm$Date[[i]]$table[1, 2] <<- ""
        }
        else {
          cleanser$cleansingForm$Date[[i]]$table[1, 2] <<- newColName 
        }
        return()
      }
    }
  }
}

right_tableManager <- function(dataValues, colName) {
  dataValues$data <- searchColname(cleanser$cleansingForm, colName)
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

mergeCleansingForm <- function(cleansingForm) {
  formDate <- c("ColName", "Change the colName")
  formNumeric <- c("ColName", "Change the colName", rep("", 6))
  formFactor <- c("ColName", "Change the colName", rep("", 7))
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
  return(list(numeric = formNumeric, factor = formFactor, Date = formDate))
}

scanData <- function(file, volumes, dir, numFac, numDate, encode, session) {
  cleanser <<- new("dataCleansing")
  cleanser$initialize()
  saveLocation <- ""
  if (length(parseDirPath(volumes, dir)) > 0) {
    saveLocation <- paste0(parseDirPath(volumes, dir), "/")
  }
  else {
    saveLocation <- "../saves/"
  }
  cleanser$fileInfo$name <<- file$name
  cleanser$mkCleansingForm(gsub("\\.csv", "", file$name), gsub("\\.csv", "", file$datapath), numFac, numDate, encode, saveLocation)
  tabPanelInit(session)
}

output_cleansingForm <- function(file, fileInfo, volumes, dir, cleansingForm, fileEncoding) {
  saveLocation <- ""
  if (length(parseDirPath(volumes, dir)) > 0) {
    saveLocation <- paste0(parseDirPath(volumes, dir), "/")
  }
  else {
    saveLocation <-  "../saves/"
  }
  forms <- mergeCleansingForm(cleanser$cleansingForm)
  formNumeric <- forms$numeric
  formFactor <- forms$factor
  formDate <- forms$Date
  cleanser$writeTablesOnExcel(formNumeric, formFactor, formDate, gsub("\\.csv", "", fileInfo$name), saveLocation)
  cleansedData <- cleanser$dataCleanser(gsub("\\.csv", "", fileInfo$name), fileEncoding = fileEncoding, path = saveLocation)
  if (fileEncoding == "Others") {
    write.csv(cleansedData, paste0(saveLocation, "Cleansed_DataSet_", gsub("\\.csv", "", fileInfo$name), ".csv"), row.names = FALSE, fileEncoding = "CP932")
  }
  else {
    write.csv(cleansedData, paste0(saveLocation, "Cleansed_DataSet_", gsub("\\.csv", "", fileInfo$name), ".csv"), row.names = FALSE, fileEncoding = fileEncoding)
    
  }
  save(cleansedData, file = paste0(saveLocation, "Cleansed_DataFrame_", gsub("\\.csv", "", fileInfo$name), ".rda"))
  files <- c(paste0("dataCleansingForm_", gsub("\\.csv", "", fileInfo$name), "_.xlsx"), paste0("Cleansed_DataSet_", fileInfo$name), fileInfo$name, paste0("Cleansed_DataFrame_", gsub("\\.csv", "", fileInfo$name), ".rda"))
  originPath <- getwd()
  setwd(saveLocation)
  zipFile <- zip(zipfile = file, files)
  setwd(originPath)
  zipFile
}

mkPreviewData <- function(data, cleansingForm, append) {
  dataList <- mergeCleansingForm(cleansingForm)
  for (i in seq_len(length(dataList))) {
    dataList[[i]] <- replace(dataList[[i]], dataList[[i]] == "", NA)
    rownames(dataList[[i]]) <- seq_len(nrow(dataList[[i]]))
  }
  for (i in colnames(data)) {
    if (!is.na(any(dataList[[1]][, 1] == i))) {
      data <- cleanser$cleansNumeric(data, i, replace(dataList[[1]], dataList[[1]] == "", NA), append)
    }
    if (!is.na(any(dataList[[2]][, 1] == i))) {
      data <- cleanser$cleansFactor(data, i, replace(dataList[[2]], dataList[[2]] == "", NA), append)
    }
    if (!is.na(any(dataList[[3]][, 1] == i))) {
      data <- cleanser$cleansDate(data, i, replace(dataList[[3]], dataList[[3]] == "", NA))
    }
  }
  return(data)
}

tabPanelInit <- function(session) {
  updateSelectInput(session, "colName", "Choose a colname:", choices = getColNames(cleanser$cleansingForm, "numeric"))
  updateTextInput(session, "newColName", "Change the colname to", "")
  updateCheckboxGroupInput(session, "numeric_NA", "Missing-values replace with the mean", choices = getMissValues(cleanser$cleansingForm, cleanser$cleansingForm$numeric[[1]]$colname))
  updateCheckboxInput(session, "categorise", "Categorise", value = isCategorised(cleanser$cleansingForm, colnames(cleanser$dataset)[1]))
  updateSelectInput(session, "colNameFactor", "Choose a colname:", choices = getColNames(cleanser$cleansingForm, "factor"))
  updateTextInput(session, "newColNameFactor", "Change the colname to", "")
  updateCheckboxGroupInput(session, "factor_NA", "Choose any levels you want to replace with NA", choices = getLevels(cleanser$cleansingForm, cleanser$cleansingForm$factor[[1]]$colname))
  updateSelectInput(session, "pool", "Select any combinations of pooled levels", choices = getPooledLevels(cleanser$cleansingForm, cleanser$cleansingForm$factor[[1]]$colname))
  updateSelectInput(session, "order", "Select as the 1st level", choices = getRepPoolLevels(cleanser$cleansingForm, cleanser$cleansingForm$factor[[1]]$colname))
  updateSelectInput(session, "colNameDate", "Choose a colname:", choices = getColNames(cleanser$cleansingForm, "Date"))
  updateTextInput(session, "newColNameDate", "Change the colname to", "")
}