library(openxlsx)
library(data.table)

setRefClass(
  Class = "dataCleansing",
  
  fields = list(
    cleansingForm = "list",
    dataset = "data.frame"
  ),
  
  methods = list(
    initialize = function() {
      cleansingForm <<- list(numeric = NULL, factor = NULL, Date = NULL)
    },

    mkNumericTable = function(data, index) {
      table <- c(index, rep("", 7))
      table <- rbind(table, c("", "Missing values", "", "Replace the column B with the spesific numbers", "", "Breaks", "", "Labels"))
      options(warn = -1)
      if (length(unique(data[is.na(as.numeric(data[, index])), index])) > 0) {
        numericData <- cbind(rep("", length(unique(data[is.na(as.numeric(data[, index])), index]))),
                             unique(data[is.na(as.numeric(data[, index])), index]),
                             rep("", length(unique(data[is.na(as.numeric(data[, index])), index]))),
                             rep("", length(unique(data[is.na(as.numeric(data[, index])), index]))),
                             rep("", length(unique(data[is.na(as.numeric(data[, index])), index]))),
                             rep("", length(unique(data[is.na(as.numeric(data[, index])), index]))),
                             rep("", length(unique(data[is.na(as.numeric(data[, index])), index]))),
                             rep("", length(unique(data[is.na(as.numeric(data[, index])), index]))))
      }
      else{
        numericData <- NULL
      }
      options(warn = 0)
      table <- rbind(table, numericData)
      table <- rbind(table, rep("", 8))
      return(table)
    },

    mkFactorTable = function(data, index) {
      table <- c(index, rep("", 8))
      table <- rbind(table, c("", "", "Levels", "", "Replace the column B with", "", "Pool the column B", "", "The Order of levels"))
      factorData <- cbind(rep("", nlevels(as.factor(data[, index]))),
                          paste0("No.", 1:nlevels(as.factor(data[, index]))), levels(as.factor(data[, index])),
                          rep("", nlevels(as.factor(data[, index]))),
                          rep("", nlevels(as.factor(data[, index]))),
                          rep("", nlevels(as.factor(data[, index]))),
                          rep("", nlevels(as.factor(data[, index]))),
                          rep("", nlevels(as.factor(data[, index]))),
                          rep("", nlevels(as.factor(data[, index]))))

      table <- rbind(table, factorData)
      table <- rbind(table, t(c(rep("", 9))))
      return(table)
    },

    formatOrder = function(formatStr) {
      order <- NULL
      for (i in formatStr) {
        if (i == "Y") {
          order <- append(order, 1)
        }
        else if(i == "m") {
          order <- append(order, 2)
        }
        else {
          order <- append(order, 3)
        }
      }
      return(order)
    },
    
    dateClassifier = function(data, index) {
      DateFormats <- list(c("Y", "m", "d"), c("Y", "d", "m"), c("m", "Y", "d"), c("m", "d", "Y"), c("d","Y", "m"), c("d", "m", "Y"))
      formatAndDate <- NULL
      delimiter <- lapply(strsplit(data[, index], ""), function(x) {
        return(x[grep("[^0-9]", x)])
      })
      
      indexDelim <- lapply(strsplit(data[, index], ""), function(x) {
        dateList <- NULL
        delStr <- c(0, grep("[^0-9]", x))
        for(i in seq_len(length(delStr))) {
          if (i == length(delStr)) {
            dateList[[i]] <- (delStr[i] + 1) : length(x)
          }
          else {
            dateList[[i]] <- (delStr[i] + 1) : (delStr[i + 1] - 1)
          }
        }
        return(list(index = dateList, delim = x[grep("[^0-9]", x)], indexDelim = grep("[^0-9]", x), data = x))
      })
      
      arrangedIndex <- lapply(indexDelim, function(x) {
        index <- NULL
        for (i in seq_len(2)) { 
          index <- append(index, c(x$index[[order(x$delim)[i]]], x$indexDelim[order(x$delim)[i]]))
        }
        if (length(x$delim) == 2) {
        index <- append(index, x$index[[3]])
        }
        else {
          index <- append(index, x$index[[order(x$delim[3])]])
        }
        return(list(index = index, data = x$data))
      })
      
      data[, index] <- unlist(lapply(arrangedIndex, function(x) {
        return(paste(x$data[x$index], collapse = ""))
      }))
      
      for (formatStr in DateFormats) {
        format <- paste0("%", formatStr[1], "-%", formatStr[2], "-%", formatStr[3])
        options(warn = -1)
  
        dateData <- replace(data[, index], data[, index] == "", NA)
        dateData <- gsub("[^0-9]", "-", as.character(dateData))
        dateData <- replace(dateData, nchar(gsub("[^-]", "", dateData)) != 2 | lapply(strsplit(dateData, "-"), length) != 3, NA)
        date_dataFrame <- as.data.frame(strsplit(dateData, "-"))
        YmdOrder <- c(match("Y",formatStr), match("m",formatStr), match("d",formatStr)) 
        charData <- paste0(formatC(as.numeric(date_dataFrame[YmdOrder[1], ]), width = 4, flag = "0"), "-",
                           formatC(as.numeric(date_dataFrame[YmdOrder[2], ]), width = 2, flag = "0"), "-",
                           formatC(as.numeric(date_dataFrame[YmdOrder[3], ]), width = 2, flag = "0"))
        options(warn = 0)
        
        formatAndDate[[length(formatAndDate) + 1]] <- list(format = format, date = charData)
      }
      return(formatAndDate)
    },

    changeColName = function(data, colname, refData, rowNumber) {
      changedName <- NULL
      if (!is.na(refData[rowNumber, 2])) {
        changedName <- refData[rowNumber, 2]
      }
      else {
        changedName <- colname
      }
      return(changedName)
    },

    replaceMissVal = function(data, refData, rowNumber) {
      missVal <- unique(data[is.na(as.numeric(data))])
      for (j in seq_len(length(missVal))) {
        if (!is.na(refData[rowNumber + 1 + j, 4])) {
          data <- replace(data, data == missVal[j], refData[rowNumber + 1 + j, 4])
        }
      }
      return(data)
    },

    cutting = function(data, refData, rowNumber) {
      if (!is.na(refData[rowNumber + 2,  8])) {
        labels <- strsplit(refData[rowNumber + 2, 8], ",")
        breaks <- strsplit(refData[rowNumber + 2, 6], ",")
        data <- cut(data, breaks = as.numeric(breaks[[1]]), labels = labels[[1]], right = FALSE)
      }
      else {
        breaks <- strsplit(refData[rowNumber + 2, 6], ",")
        data <- cut(data, breaks = as.numeric(breaks[[1]]), right = FALSE)
      }
     return(data)
    },

    orderer = function(data, refData, rowNumber) {
      orderVector <- rep(NA, nlevels(as.factor(data)))
      factorLength <- 0
      rowIndex <- 1

      while (!is.na(refData[rowNumber + 1 + rowIndex, 2])) {
        factorLength <- factorLength + 1
        rowIndex <- rowIndex + 1
      }

      pooledFactor <- strsplit(refData[(rowNumber + 2):(rowNumber + 1 + factorLength), 7], "[+]")
      poolLevel <- NULL
      for (i in seq_len(length(pooledFactor))) {
        refDataPool <- NULL
        refDataLevels <- NULL
        if (length(pooledFactor[[i]]) > 1) {
          for (j in seq_len(length(pooledFactor[[i]]))) {
            if (!is.na(refData[rowNumber + 1 + as.numeric(pooledFactor[[i]][j]), 5])) {
              refDataPool <- paste0(refDataPool, "+",  refData[rowNumber + 1 + as.numeric(pooledFactor[[i]][j]), 5])
            }
            else {
              refDataPool <- paste0(refDataPool, "+",  refData[rowNumber + 1 + as.numeric(pooledFactor[[i]][j]), 3])
            }
            refDataLevels <- append(refDataLevels, as.numeric(pooledFactor[[i]][j]))
          }
          refDataPool <- substr(refDataPool, 2, nchar(refDataPool))
          poolLevel[[i]] <- list(pool = refDataPool, levels = refDataLevels)
        }
      }

      for (i in seq_len(length(poolLevel))) {
        refData[rowNumber + 1 + poolLevel[[i]]$levels, 5] <- poolLevel[[i]]$pool
      }

      for (j in seq_len(factorLength)) {
        if (!is.na(refData[rowNumber + 1 + j, 9])) {
          orderNum <- as.numeric(refData[rowNumber + 1 + j, 9])
          if (!is.na(refData[rowNumber + 1 + j, 5])) {
            if (refData[rowNumber + 1 + j, 5] != "N/A") {
              orderVector[orderNum] <- refData[rowNumber + 1 + j, 5]
            }
          }
          else {
            orderVector[orderNum] <- refData[rowNumber + 1 + j, 3]
          }
        }
      }
      remainLevels <- setdiff(levels(as.factor(data)), orderVector)
      count <- 1
      for (i in seq_len(length(orderVector))) {
        if (is.na(orderVector[i])) {
          orderVector[i] <- remainLevels[count]
          count <- count + 1
        }
      }
      return(factor(data, levels = orderVector))
    },

    pooledName = function(data, refData, rowNumber) {
      pooledData <- pooler(data, refData, rowNumber)
      pooledFactor <- ""
      for (j in grep("[+]", levels(pooledData))) {
        pooledFactor <- paste0(pooledFactor, ".", levels(pooledData)[j])
      }
      return(substr(pooledFactor, 2, nchar(pooledFactor)))
    },

    pooler = function(data, refData, rowNumber) {
      poolList <- strsplit(refData[(rowNumber + 2) : (rowNumber + 1 + nlevels(as.factor(data))), 7], "[+]")

      data <- as.character(data)
      for (j in seq_len(length(poolList))) {
        if (length(poolList[[j]]) > 1) {
          poolLevel <- ""
          for (k in seq_len(length(poolList[[j]]))) {
            if (!is.na(refData[rowNumber + 1 + as.numeric(poolList[[j]][k]), 5])) {
              poolLevel <- paste0(poolLevel, "+",  refData[rowNumber + 1 + as.numeric(poolList[[j]][k]), 5])
            }
            else {
              poolLevel <- paste0(poolLevel, "+",  refData[rowNumber + 1 + as.numeric(poolList[[j]][k]), 3])
            }
          }
          poolLevel <- substr(poolLevel, 2, nchar(poolLevel))
          for (k in seq_len(length(poolList[[j]]))) {
            if (!is.na(refData[rowNumber + 1 + as.numeric(poolList[[j]][k]), 5])) {
              data <- replace(data, data == refData[rowNumber + 1 + as.numeric(poolList[[j]][k]), 5], poolLevel)
            }
            else {
              data <- replace(data, data == refData[rowNumber + 1 + as.numeric(poolList[[j]][k]), 3], poolLevel)
            }
          }
        }
      }
      levels <- levels(as.factor(data))
      return(factor(data, levels = levels))
    },

    replacer = function(data, refData, rowNumber) {
      levelsRow <- refData[(rowNumber + 2) : (rowNumber + 1 + nlevels(as.factor(data))), 5] != ""
      factorLength <- 0
      rowIndex <- 1

      while (!is.na(refData[rowNumber + 1 + rowIndex, 2])) {
        factorLength <- factorLength + 1
        rowIndex <- rowIndex + 1
      }

      data <- as.character(data)

      refData[(rowNumber + 2) : (rowNumber + 1 + factorLength), 3] <- replace(refData[(rowNumber + 2) : (rowNumber + 1 + factorLength), 3], is.na(refData[(rowNumber + 2) : (rowNumber + 1 + factorLength), 3]), "")
      for (j in seq_len(nlevels(as.factor(data)))) {
        if (!is.na(levelsRow[j])) {
          if (refData[rowNumber + 1 + j, 5] == "N/A") {
            data <- replace(data, data == refData[rowNumber + 1 + j, 3], NA)
          }
          else {
            data <- replace(data, data == refData[rowNumber + 1 + j, 3], refData[rowNumber + 1 + j, 5])
          }
        }
      }
      levels <- levels(as.factor(data))
      return(factor(data, levels = levels))
    },

    readData = function(dataName, fileEncoding) {
      if (fileEncoding == "UTF-8" | fileEncoding == "Latin-1") {
        data <- data.table::fread(paste0(dataName, ".csv"), encoding = fileEncoding)
      }
      else {
        data <- data.table::fread(paste0(dataName, ".csv"), encoding = "unknown")
      }
      return(data)
    },

    mkTimeTable = function(data, index, tableTime, leastNumOfDate) {
      asDatedVector <- rep(FALSE, nrow(data))
      haveNumber <- replace(rep(FALSE, nrow(data)), grep("[0-9]", data[, index]), TRUE) 
      haveLeastNumStr <- nchar(data[, index]) > 4
      delimiter <- nchar(gsub("[0-9]", "", data[, index])) == 2 
      if (length(data[haveNumber & haveLeastNumStr & delimiter, index]) <= leastNumOfDate) {
        return(NULL)
      }
      formCharData <- dateClassifier(data, index)
      for (i in formCharData) {
        asDatedVector[!is.na(isCorrectFormat(i))] <- TRUE
      }
      if (length(asDatedVector[asDatedVector == TRUE]) > leastNumOfDate) {
        tableTime <- rbind(tableTime, c(index, ""))
        cleansingForm$Date[[length(cleansingForm$Date) + 1]] <<- list(table = as.data.frame(t(c(index, ""))), colname = index)
        return(tableTime)
      }
      return(NULL)
    },

    writeTablesOnExcel = function(tableNumeric, tableFactor, tableTime, dataName) {
      sheetNumeric <- list(table = tableNumeric, sheetName = "numeric")
      sheetFactor <- list(table = tableFactor, sheetName = "factor")
      sheetTime <- list(table = tableTime, sheetName = "Date")
      wb <- openxlsx::createWorkbook()
      for (i in list(sheetNumeric, sheetFactor, sheetTime)) {
        openxlsx::addWorksheet(wb, i$sheetName)
      }
      st <- openxlsx::createStyle(fontName = "Yu Gothic", fontSize = 11)
      for (i in list(sheetNumeric, sheetFactor, sheetTime)) {
        openxlsx::addStyle(wb, i$sheetName, style = st, cols = 1:2, rows = 1:2)
        openxlsx::writeData(wb, sheet = i$sheetName, x = i$table, colNames = F, withFilter = F)
      }
      openxlsx::modifyBaseFont(wb, fontSize = 11, fontColour = "#000000", fontName = "Yu Gothic")
      openxlsx::saveWorkbook(wb, paste0("dataCleansingForm_", dataName, "_.xlsx"), overwrite = TRUE)
    },

    mkTableNum_Fac = function(data, index, numOrFac, tableNumeric, tableFactor) {
      options(warn = -1)
      charEqualNum <-  as.character(as.numeric(data[, index])) ==as.numeric(data[, index]) #todo check
      options(warn = 0)
      if (length(na.omit(data[charEqualNum == FALSE, index])) == 0 &  length(na.omit(data[charEqualNum == TRUE, index])) > 0 & nlevels(as.factor(data[, index])) > nrow(data) / numOrFac) {
        numTab <- mkNumericTable(data, index)
        tableNumeric <- rbind(tableNumeric, numTab)
        if (!is.null(numTab)) {
          cleansingForm$numeric[[length(cleansingForm$numeric) + 1]] <<- list(table = as.data.frame(numTab), colname = index)
        }
      }
      else {
        facTab <- mkFactorTable(data, index)
        tableFactor <- rbind(tableFactor, facTab)
        if (!is.null(facTab)) {
          cleansingForm$factor[[length(cleansingForm$factor) + 1]] <<- list(table = as.data.frame(facTab), colname = index)
        }
      }
      return(list(num = tableNumeric, fac = tableFactor))
    },

    cleansNumeric = function(data, index, refData, append) {
      if (any(refData[, 1] == index)) {
        options(warn = -1)
        rowNumber <- as.numeric(rownames(refData[refData[, 1] == index & !is.na(refData[, 1]), ]))
        colnames(data)[colnames(data) == index] <- changeColName(data, index, refData, rowNumber)
        index <- changeColName(data, index, refData, rowNumber)
        if (any(!is.na(refData[(rowNumber + 2):(rowNumber + 1 + length(unique(data[is.na(as.numeric(data[, index])), index]))), 4])) & length(unique(data[is.na(as.numeric(data[, index])), index])) > 0) {
          if (append == TRUE) {
            data <- cbind(data, replaceMissVal(data[, index], refData, rowNumber))
            colnames(data)[ncol(data)] <- paste0(index, "_missing Values replaced")
          }
          else {
            data[, index] <- replaceMissVal(data[, index], refData, rowNumber)
          }
        }
        data[, index] <- as.numeric(data[, index])
        options(warn = 0)
        if (any(!is.na(refData[rowNumber + 2, 6]))) {
          if (append == TRUE) {
            data <- cbind(data, cutting(data[, index], refData, rowNumber))
            colnames(data)[ncol(data)] <- paste0(index, "_categorized")
          }
          else {
            data[, index] <- cutting(data[, index], refData, rowNumber)
          }
        }
      }
      return(data)
    },
    cleansFactor = function(data, index, refData, append) {
      if (any(refData[, 1] == index)) {
        pooling <- FALSE
        ordering <- FALSE
        options(warn = -1)
        rowNumber <- as.numeric(rownames(refData[refData[, 1] == index & !is.na(refData[, 1]), ]))
        options(warn = 0)
        colnames(data)[colnames(data) == index] <- changeColName(data, index, refData, rowNumber)
        index <- changeColName(data, index, refData, rowNumber)
        nrowLevel <- nlevels(as.factor(data[, index]))
        if (any(!is.na(refData[(rowNumber + 2):(rowNumber + 1 +  nrowLevel), 5]))) { #5 for replace
          data[, index] <- replacer(data[, index], refData, rowNumber)
          if (any(!is.na(refData[(rowNumber + 2):(rowNumber + 1 + nlevels(as.factor(data[, index]))), 7]))) { #7 for pool
            if (append == TRUE) {
              data <- cbind(data, pooler(data[, index], refData, rowNumber))
              colnames(data)[ncol(data)] <- paste0(index, "_", pooledName(data[, index], refData, rowNumber))
              index <- colnames(data)[ncol(data)]
            }
            else {
              data[, index] <- pooler(data[, index], refData, rowNumber)
            }
            pooling <- TRUE
            if (any(!is.na(refData[(rowNumber + 2):(rowNumber + 1 +  nrowLevel), 9]))) { #9 for order
              data[, index] <- orderer(data[, index], refData, rowNumber)
              ordering <- TRUE
            }
          }
        }
        if (any(!is.na(refData[(rowNumber + 2):(rowNumber + 1 + nlevels(as.factor(data[, index]))), 7])) & pooling == FALSE) {
          if (append == TRUE) {
            data <- cbind(data, pooler(data[, index], refData, rowNumber))
            colnames(data)[ncol(data)] <- paste0(index, "_", pooledName(data[, index], refData, rowNumber))
          }
          else {
            data[, index] <- pooler(data[, index], refData, rowNumber)
          }
          pooling <- TRUE
          if (any(!is.na(refData[(rowNumber + 2):(rowNumber + 1 + nlevels(as.factor(data[, index]))), 9])) & ordering == FALSE) {
            data[, index] <- orderer(data[, index], refData, rowNumber)
            ordering <- TRUE
          }
        }
        if (any(!is.na(refData[(rowNumber + 2):(rowNumber + 1 + nlevels(as.factor(data[, index]))), 9])) & ordering == FALSE) {
          data[, index] <- orderer(data[, index], refData, rowNumber)
          ordering <- TRUE
        }
        data[, index] <- as.factor(data[, index])
      }
      return(data)
    },

    cleansDate = function(data, index, refData) {
      if (any(refData[, 1] == index)) {
        asDatedVector <- rep(FALSE, nrow(data))
        options(warn = -1)
        rowNumber <- as.numeric(rownames(refData[refData[, 1] == index & !is.na(refData[, 1]), ]))
        colnames(data)[colnames(data) == index] <- changeColName(data, index, refData, rowNumber)
        index <- changeColName(data, index, refData, rowNumber)
        formCharData <- dateClassifier(data, index)
        for (i in formCharData) {
          data[!is.na(isCorrectFormat(i)) & !asDatedVector, index] <- i$date[!is.na(isCorrectFormat(i)) & !asDatedVector]
          asDatedVector <- asDatedVector | !is.na(isCorrectFormat(i)) 
        }
        options(warn = 0)
      }
      return(data)
    },

    isCorrectFormat <- function(dateAsFormat) {
      splitedFCD <- strsplit(dateAsFormat$date, "-")
      correctDate <- unlist(lapply(splitedFCD, function(x) {
        conditionFormat <- nchar(x[1]) == 4 & nchar(x[2]) == 2 & nchar(x[3]) == 2
        conditionUpper <- as.numeric(x[2]) < 13 & as.numeric(x[3] < 32)
        if (conditionFormat & conditionUpper) {
           return(TRUE)
        }
        else {
          return(FALSE)
        }
        }))
      return(correctDate)
    },

#' Cleansing the dataset on a csv-file to change its form to more arranged one to handle.
#' @encoding UTF-8
#'
#' @param dataName The file-name of a csv file that will be cleansed.
#' @param append Allows you to append the new datas generated from dataCleansingForm__.xlsx.
#' @param numOrFac The criteria for classifying whether the column data is numeric or factor. If the number of levels are greater than the ratio (nrow(data)/numOrFac), then it will be assiged to numeric group.
#' @param leastNumOfDate The criteria for classifying whether the column data is Date of numeric. if the data contains the dateFormat is greater than this value, leastNumOfDate, then the data will be assigned to Date group.
#' @param fileEncoding File-encoding
#'
#' @importFrom data.table fread
#' @export
    dataCleanser = function(dataName, append = FALSE, numOrFac = 10, leastNumOfDate = 10, fileEncoding = "CP932") {
      files <- list.files()
      if (any(files == paste0("dataCleansingForm_", dataName, "_.xlsx")) == FALSE) {
        data <- as.data.frame(readData(dataName, fileEncoding))
        dataset <<- data
        tableTime <- c("ColName", "Change the colName")
        tableNumeric <- c("ColName", "Change the colName", rep("", 6))
        tableFactor <- c("ColName", "Change the colName", rep("", 7))

        for (i in colnames(data)) {
          table_Time <- mkTimeTable(data, i, tableTime, leastNumOfDate)
          if (!is.null(table_Time)) {
            tableTime <- table_Time
            next ()
          }
          tableNum_Fac <- mkTableNum_Fac(data, i, numOrFac, tableNumeric, tableFactor)
          tableNumeric <- tableNum_Fac$num
          tableFactor <- tableNum_Fac$fac
        }
        writeTablesOnExcel(tableNumeric, tableFactor, tableTime, dataName)
      }
      else {
        data <- as.data.frame(readData(dataName, fileEncoding))
        dataList <- NULL
        sheetList <- c("numeric", "factor", "Date")
        for (i in seq_len(length(sheetList))) {
          dataList[[i]] <- openxlsx::read.xlsx(paste0("dataCleansingForm_", dataName, "_.xlsx"), sheet = sheetList[i], colNames = F, skipEmptyRows = FALSE, skipEmptyCols = FALSE, na.strings = c("NA", ""))
        }
        for (i in colnames(data)) {
          if (!is.na(any(dataList[[1]][, 1] == i))) {
            data <- cleansNumeric(data, i, dataList[[1]], append)
          }
          if (!is.na(any(dataList[[2]][, 1] == i))) {
            data <- cleansFactor(data, i, dataList[[2]], append)
          }
          if (!is.na(any(dataList[[3]][, 1] == i))) {
            data <- cleansDate(data, i, dataList[[3]])
          }
        }
        return(data)
      }
    }
  )
)
