library(readxl)
dir = getwd()

getXlsxFiles = function(path, type){
    allFiles = list.files(path)
    if(type == "junior")
        xlsx = allFiles[which(grepl("國中", allFiles))]
    else if(type == "elementary")
        xlsx = allFiles[which(grepl("國小", allFiles))]
    else{
        print("Not supported type")
        return(NULL)
    }
    return(substr(xlsx,1,nchar(xlsx)-5))
}

getSchoolNames = function(path, type){
    xlsx = getXlsxFiles(path, type)
    if(is.null(xlsx))
        return(NULL)
    if(type == "junior")
        school = substr(xlsx, regexpr("國中", xlsx)-2, regexpr("國中", xlsx)-1)
    else if(type == "elementary")
        school = substr(xlsx, regexpr("國小", xlsx)-2, regexpr("國小", xlsx)-1)
    school
}

getControlSchool = function(path, type){
    xlsx = getXlsxFiles(path, type)
    if(is.null(xlsx))
        return(NULL)
    school.control = xlsx[which(grepl("實驗", xlsx))]
    if(type == "junior")
        school.control = substr(school.control, regexpr("國中", school.control)-2, regexpr("國中", school.control)-1)
    else if(type == "elementary")
        school.control = substr(school.control, regexpr("國小", school.control)-2, regexpr("國小", school.control)-1)
    school.control
}

getAllSheet = function(school, path){
    if(!grepl(".xlsx", school))
        school = paste0(school, ".xlsx")
    allSheet = excel_sheets(paste0(path, "/", school))
    return(allSheet)
}

RemoveWhiteSpace = function(string){
    return(gsub(" ", "", string, fixed = TRUE))
}

CollectAll = function(x, DataDir, xlsx.name, sheet.name, type){
    ## init
    if(type == "junior"){
        parts = rep(1:5,c(3,3,3,6,9)) # 各大題題數：1~5大題，分別有 3 3 3 6 9 題
        head.type = c("text", "numeric", "text", "text", "skip", "skip", rep("numeric", 24))
        head.name = c("ID", "school", "grade", "class",
              paste0("L", rep(1:3, each = 3), 1:9),
              paste0("R", rep(1:2, c(6,9)), 1:15)
        )
        rng = "A7:AD"
    }
    else if(type == "elementary"){
        parts = rep(1:10, c(5,5,10,10,5,5,5,5,5,5))
        head.type = c("text", "numeric", "text", "text", "skip", "skip", rep("numeric", 60))
        head.name = c("ID", "school", "grade", "class",
            paste0("T", parts, c(rep(1:5, 2), rep(1:10, 2), rep(1:5, 6)))
        )
        rng = "A7:BN"
    }
    else if(type == "toeic8"){
        if(!grep("[0-9]{2}-[0-9]{1}八年級", sheet.name[1])){
            print("wrong sheet to do toeic collection.")
            return(NULL)
        }
        head.type = c("text", "numeric", rep("skip", 2), "text", "text", "skip",
                      rep("numeric", 3), rep("text", 2), rep("numeric", 5)
        )
        head.name = c("ID", "school", "grade", "class",
                      "L", "R", "Total",
                      "L.lev", "R.lev",
                      "func", "gram", "Lstgy", "Rstgy", "vocb"
        )
        rng = "A2:Q"
    }
    else{
        print("wrong collection type")
        return(NULL)
    }
    ## Data Collecting
    JuniorAll = c()
    for(i in 1:length(x)){
        # delete any whitespace in xlsx sheet names
        allSheets = getAllSheet(xlsx.name[i], DataDir)
        
        for(j in 1:length(sheet.name)){
            # choose correct sheet
            real.sheet.name = allSheets[which(RemoveWhiteSpace(allSheets) == sheet.name[j])]
            # get number of rows in a xlsx file
            n = try(nrow(read_excel(paste0(DataDir,"/",xlsx.name[i],".xlsx"),
                                    sheet = real.sheet.name,
                                    range = cell_cols("A"))),
                    silent = T
            )
            if(class(n) == "try-error") next # if there is no proper sheet.name/xlsx.name, then skip
            # read data frame within xlsx file according to n
            if(type == "toeic8") n = n - 5
            school.table = as.data.frame(read_excel(paste0(DataDir,"/",xlsx.name[i],".xlsx"),
                                                    sheet = real.sheet.name,
                                                    range = paste0(rng,n+6),
                                                    col_names = head.name,
                                                    col_types = head.type))
            school.table[,2] = x[i]
            JuniorAll = rbind(JuniorAll, school.table)
        }
    }
    if(anyDuplicated(JuniorAll$ID)){
        warnings("There are duplicated IDs", call. = FALSE)
    }
    write.csv(JuniorAll, "Table 0 Cleaned Data.csv", row.names = FALSE)
    # rm(school.table, head.type, head.name, col_names, parts, rng)
    JuniorAll
}

##### Key對的題數：JuniorAll.sum #####
CreateAcu = function(JuniorAll, type, out = "acu"){
    JuniorAll.sum = c()
    if(type == "junior"){
        part.num = c(3,3,3,6,9)
        parts = rep(1:5, part.num)
        col_names = c("L1", "L2", "L3", "R1", "R2", "Total")
    }
    else if(type == "elementary"){
        part.num = c(5,5,10,10,5,5,5,5,5,5)
        parts = rep(1:10, part.num)
        col_names = c(paste0("P", 1:10), "Total")
    }
    else if(type == "toeic8"){
        JuniorAll.acu = JuniorAll["L", "R", "Total"]
        return(JuniorAll.acu)
    }
    else{
        print("Not supported type")
        return(NULL)
    }

    for(i in 1:length(part.num)){
        JuniorAll.sum = cbind(JuniorAll.sum, apply(JuniorAll[,which(parts == i)+4],1,sum))
    }
    JuniorAll.sum = cbind(JuniorAll.sum, apply(JuniorAll[,1:length(parts)+4],1,function(x)sum(x, na.rm=T)))
    JuniorAll.acu = t(t(JuniorAll.sum) / append(part.num, 1))
    JuniorAll.acu[,ncol(JuniorAll.acu)] = JuniorAll.acu[,ncol(JuniorAll.acu)]/ (!apply(JuniorAll.sum[,-ncol(JuniorAll.acu)], 2, is.na)) %*% part.num # calculate total answered questions
    JuniorAll.sum = as.data.frame(JuniorAll.sum) # 各大題答對題數
    JuniorAll.acu = as.data.frame(JuniorAll.acu) # 各大題答對率(accuracy)
    colnames(JuniorAll.sum) = col_names
    colnames(JuniorAll.acu) = col_names

    if(out == "sum"){
        return(JuniorAll.sum)
    }

    return(JuniorAll.acu)
}
