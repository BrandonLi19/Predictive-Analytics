# Concatenate store data and add an additional column StoreID ranging from 2 to 9
AddIDColumn <- function(storeID) {
  return(cbind(dataList[[storeID]], StoreID = substr(storeID, 6, 6)))
}

DataPrep <- function(directory) {
  # Read all xlsx workbooks
  require(readxl)
  require(dplyr)
  require(sqldf)
  
  fileList <- list.files(path = directory, pattern = '*.xlsx')
  dataList <- sapply(fileList, read_xlsx, simplify = FALSE)
  names(dataList) <- tolower(substr(names(dataList), 1, 6))  # Rename list items to 'store2'-like
  
  # GetStoreIDMapping <- function(dataList) {
  #   # May not be necessary
  #   lapply(dataList, function(dataFrame) {unique(dataFrame$Store)})
  # }
  
  dataFrameWithStoreID <- lapply(names(dataList), AddIDColumn)
  allData <- do.call(rbind, dataFrameWithStoreID)
  
  SQLstring <- 'select curr.*, 
                prevOne.Y1 as prevOneY1 , log(prevOne.Y1) as prevOneY1Log ,
prevOne.Y2 as prevOneY2 , log(prevOne.Y2) as prevOneY2Log ,
  prevOne.Y3 as prevOneY3 , log(prevOne.Y3) as prevOneY3Log ,
  prevOne.Y4 as prevOneY4 , log(prevOne.Y4) as prevOneY4Log ,
  prevOne.Y5 as prevOneY5 , log(prevOne.Y5) as prevOneY5Log ,
  prevOne.Y6 as prevOneY6 , log(prevOne.Y6) as prevOneY6Log ,
  prevOne.Y7 as prevOneY7 , log(prevOne.Y7) as prevOneY7Log ,
  prevOne.Y8 as prevOneY8 , log(prevOne.Y8) as prevOneY8Log ,
  prevOne.Y9 as prevOneY9 , log(prevOne.Y9) as prevOneY9Log ,
  prevOne.Y10 as prevOneY10 , log(prevOne.Y10) as prevOneY10Log ,
  prevOne.Y11 as prevOneY11 , log(prevOne.Y11) as prevOneY11Log ,
  prevOne.Y12 as prevOneY12 , log(prevOne.Y12) as prevOneY12Log ,
  prevOne.Y13 as prevOneY13 , log(prevOne.Y13) as prevOneY13Log ,
  prevOne.Y14 as prevOneY14 , log(prevOne.Y14) as prevOneY14Log ,
  prevOne.Y15 as prevOneY15 , log(prevOne.Y15) as prevOneY15Log ,
  prevOne.Y16 as prevOneY16 , log(prevOne.Y16) as prevOneY16Log ,
  prevOne.Y17 as prevOneY17 , log(prevOne.Y17) as prevOneY17Log ,
  prevOne.Y18 as prevOneY18 , log(prevOne.Y18) as prevOneY18Log ,
  prevOne.Y19 as prevOneY19 , log(prevOne.Y19) as prevOneY19Log ,
  prevOne.Y20 as prevOneY20 , log(prevOne.Y20) as prevOneY20Log ,
  prevOne.Y21 as prevOneY21 , log(prevOne.Y21) as prevOneY21Log ,
  prevOne.Y22 as prevOneY22 , log(prevOne.Y22) as prevOneY22Log ,
  prevOne.Y23 as prevOneY23 , log(prevOne.Y23) as prevOneY23Log ,
  prevOne.Y24 as prevOneY24 , log(prevOne.Y24) as prevOneY24Log ,
  
  prevTwo.Y1 as prevTwoY1 , log(prevTwo.Y1) as prevTwoY1Log ,
  prevTwo.Y2 as prevTwoY2 , log(prevTwo.Y2) as prevTwoY2Log ,
  prevTwo.Y3 as prevTwoY3 , log(prevTwo.Y3) as prevTwoY3Log ,
  prevTwo.Y4 as prevTwoY4 , log(prevTwo.Y4) as prevTwoY4Log ,
  prevTwo.Y5 as prevTwoY5 , log(prevTwo.Y5) as prevTwoY5Log ,
  prevTwo.Y6 as prevTwoY6 , log(prevTwo.Y6) as prevTwoY6Log ,
  prevTwo.Y7 as prevTwoY7 , log(prevTwo.Y7) as prevTwoY7Log ,
  prevTwo.Y8 as prevTwoY8 , log(prevTwo.Y8) as prevTwoY8Log ,
  prevTwo.Y9 as prevTwoY9 , log(prevTwo.Y9) as prevTwoY9Log ,
  prevTwo.Y10 as prevTwoY10 , log(prevTwo.Y10) as prevTwoY10Log ,
  prevTwo.Y11 as prevTwoY11 , log(prevTwo.Y11) as prevTwoY11Log ,
  prevTwo.Y12 as prevTwoY12 , log(prevTwo.Y12) as prevTwoY12Log ,
  prevTwo.Y13 as prevTwoY13 , log(prevTwo.Y13) as prevTwoY13Log ,
  prevTwo.Y14 as prevTwoY14 , log(prevTwo.Y14) as prevTwoY14Log ,
  prevTwo.Y15 as prevTwoY15 , log(prevTwo.Y15) as prevTwoY15Log ,
  prevTwo.Y16 as prevTwoY16 , log(prevTwo.Y16) as prevTwoY16Log ,
  prevTwo.Y17 as prevTwoY17 , log(prevTwo.Y17) as prevTwoY17Log ,
  prevTwo.Y18 as prevTwoY18 , log(prevTwo.Y18) as prevTwoY18Log ,
  prevTwo.Y19 as prevTwoY19 , log(prevTwo.Y19) as prevTwoY19Log ,
  prevTwo.Y20 as prevTwoY20 , log(prevTwo.Y20) as prevTwoY20Log ,
  prevTwo.Y21 as prevTwoY21 , log(prevTwo.Y21) as prevTwoY21Log ,
  prevTwo.Y22 as prevTwoY22 , log(prevTwo.Y22) as prevTwoY22Log ,
  prevTwo.Y23 as prevTwoY23 , log(prevTwo.Y23) as prevTwoY23Log ,
  prevTwo.Y24 as prevTwoY24, log(prevTwo.Y24) as prevTwoY24Log ,
  
  log(curr.Y1) as Y1Log,log(curr.P1) as P1Log ,
  log(curr.Y2) as Y2Log,log(curr.P2) as P2Log ,
  log(curr.Y3) as Y3Log,log(curr.P3) as P3Log ,
  log(curr.Y4) as Y4Log,log(curr.P4) as P4Log ,
  log(curr.Y5) as Y5Log,log(curr.P5) as P5Log ,
  log(curr.Y6) as Y6Log,log(curr.P6) as P6Log ,
  log(curr.Y7) as Y7Log,log(curr.P7) as P7Log ,
  log(curr.Y8) as Y8Log,log(curr.P8) as P8Log ,
  log(curr.Y9) as Y9Log,log(curr.P9) as P9Log ,
  log(curr.Y10) as Y10Log,log(curr.P10) as P10Log ,
  log(curr.Y11) as Y11Log,log(curr.P11) as P11Log ,
  log(curr.Y12) as Y12Log,log(curr.P12) as P12Log ,
  log(curr.Y13) as Y13Log,log(curr.P13) as P13Log ,
  log(curr.Y14) as Y14Log,log(curr.P14) as P14Log ,
  log(curr.Y15) as Y15Log,log(curr.P15) as P15Log ,
  log(curr.Y16) as Y16Log,log(curr.P16) as P16Log ,
  log(curr.Y17) as Y17Log,log(curr.P17) as P17Log ,
  log(curr.Y18) as Y18Log,log(curr.P18) as P18Log ,
  log(curr.Y19) as Y19Log,log(curr.P19) as P19Log ,
  log(curr.Y20) as Y20Log,log(curr.P20) as P20Log ,
  log(curr.Y21) as Y21Log,log(curr.P21) as P21Log ,
  log(curr.Y22) as Y22Log,log(curr.P22) as P22Log ,
  log(curr.Y23) as Y23Log,log(curr.P23) as P23Log ,
  log(curr.Y24) as Y24Log,log(curr.P24) as P24Log 
  
                  from allData as curr join allData as prevOne 
                  on curr.Week = prevOne.Week + 1 and curr.Store = prevOne.Store
                  join allData as prevTwo on curr.Week = prevTwo.Week + 2 and curr.Store = prevTwo.Store'
  
  allDataWithPrevLog <- sqldf(SQLstring)
  
  return(allDataWithPrevLog)
}