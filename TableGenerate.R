##### 表1 答對率標準差 #####
CreateAcuAll = function(JuniorAll, JuniorAll.acu, acuAll.name){
    x = unique(JuniorAll$school)
    acuAll = c()
    acu.col3 = c()
    acu.col2 = c()
    acu.col1 = c()
    for(school in 1:length(x)){
        students = which(JuniorAll$school == x[school])          # all students in this school
            for(j in unique(JuniorAll$grade[students])){
                # step 1 : calculate classes summary in this grade
                grade.j = which(JuniorAll[students,"grade"] == j)    # students of grade.j
                acuClass = c()                                       # initualize summary for this grade.j
                acu.col3 = append(acu.col3, unique(JuniorAll[students[grade.j], "class"]))
                acu.col2 = append(acu.col2, rep(j, length(unique(JuniorAll[students[grade.j], "class"]))))
                for(part in 1:length(JuniorAll.acu)){
                    m = tapply(JuniorAll.acu[students[grade.j], part], JuniorAll[students[grade.j], "class"], function(x)mean(x,na.rm=T))
                    s = tapply(JuniorAll.acu[students[grade.j], part], JuniorAll[students[grade.j], "class"], function(x)sd(x,na.rm=T))
                    acuClass = cbind(acuClass, m[unique(JuniorAll[students[grade.j], "class"])],s[unique(JuniorAll[students[grade.j], "class"])])
                }
                acuClass = cbind(table(JuniorAll[students[grade.j],"class"])[unique(JuniorAll[students[grade.j], "class"])], acuClass)
                # step 2 : calculate grade summary if needed
                if(nrow(acuClass) > 1){                              # if more than 1 class, then summary for this grade
                    acuGrade = as.vector(rbind(apply(JuniorAll.acu[students[grade.j],], 2, function(x)mean(x,na.rm=T)),
                                               apply(JuniorAll.acu[students[grade.j],], 2, function(x)sd(x,na.rm=T))))
                    acuClass = rbind(acuClass, c(length(students[grade.j]), acuGrade))
                    acu.col3 = append(acu.col3, "全年級")
                    acu.col2 = append(acu.col2, j)
                }
                # step 3 : combind a grade summary for this school into acuAll
                acuAll = rbind(acuAll, acuClass)
            }
            # step 4 : calculate school summary if needed
            if(length(unique(JuniorAll[students, "grade"])) > 1){    # if more than 1 grade, then summary for this school
                acuSchool = as.vector(rbind(apply(JuniorAll.acu[students,], 2, function(x)mean(x,na.rm=T)),
                                            apply(JuniorAll.acu[students,], 2, function(x)sd(x,na.rm=T))))
                acuAll = rbind(acuAll, c(length(students), acuSchool))
                acu.col3 = append(acu.col3, "")
                acu.col2 = append(acu.col2, "全校")
            }
        acu.col1 = append(acu.col1, c(x[school], rep("", length(acu.col2)-length(acu.col1)-1)))
    }

    acuAll = cbind.data.frame(acu.col1, acu.col2, acu.col3, acuAll)
    names(acuAll) = c("學校", "年級", "班級", "人數", rep(c("平均", "標準差"), length(JuniorAll.acu)))
    write.csv(acuAll, paste0(acuAll.name, ".csv"), row.names = F)
    return(acuAll)
}

##### 表2 國中各班級間 #####
CreateAnoClass = function(JuniorAll, JuniorAll.acu, anoClass.name){
    x = unique(JuniorAll$school)
    anoClass = c()
    anoClass.col1 = c()
    anoClass.col2 = c()
    for(school in 1:length(x)){
        schoolResult = c()
        students = which(JuniorAll$school == x[school])
        for(j in unique(JuniorAll$grade[students])){
            gradeResult = c()
            grade.j = which(JuniorAll[students,"grade"] == j)
            if(length(unique(JuniorAll[students[grade.j], "class"])) < 2){
                next
            }
            for(part in 1:length(JuniorAll.acu)){
                part.summary = try(c(summary(aov(JuniorAll.acu[students[grade.j],part]~JuniorAll[students[grade.j], "class"]))[[1]][["F value"]][[1]],
                                     summary(aov(JuniorAll.acu[students[grade.j],part]~JuniorAll[students[grade.j], "class"]))[[1]][["Pr(>F)"]][[1]]),
                                   silent = T)
                if(class(part.summary) == "try-error")
                    gradeResult = append(gradeResult, rep(NA, 2))
                else
                    gradeResult = append(gradeResult, part.summary)
            }
            schoolResult = rbind(schoolResult, gradeResult)
            anoClass.col2 = append(anoClass.col2, j)
            # anoClass.col1 = append(anoClass.col1, x[school])
        }
        if(length(anoClass.col2)>length(anoClass.col1)){
            anoClass.col1 = append(anoClass.col1, c(x[school], rep("", length(anoClass.col2)-length(anoClass.col1)-1)))
        }
        anoClass = rbind(anoClass, schoolResult)
    }
    if(length(anoClass)){
        anoClass = cbind.data.frame(anoClass.col1, anoClass.col2, anoClass)
        names(anoClass) = c("學校", "年級", rep(c("F值","P值"),length(JuniorAll.acu)))
        write.csv(anoClass, paste0(anoClass.name, ".csv"), row.names = F)
    }
    else{
        print("All schools has only one class.")
    }

    return(anoClass)
}

##### 表3 國中各學校間 #####
CreateAnoSchool = function(JuniorAll, JuniorAll.acu, anoSchool.name){
    if(length(unique(JuniorAll$school))<2){
        print("There is only one school.")
        return(NULL)
    }
    anoSchool = c()
    anoSchool.col1 = c()
    schoolResult = c()
    if(length(unique(JuniorAll$grade))>1){
        for(j in unique(JuniorAll$grade)){
            gradeResult = c()
            grade.j = which(JuniorAll[,"grade"] == j)
            if(length(unique(JuniorAll[grade.j, "school"])) < 2){
                next
            }
            for(part in 1:length(JuniorAll.acu)){
                part.summary = try(c(summary(aov(JuniorAll.acu[grade.j,part]~JuniorAll[grade.j,"school"]))[[1]][["F value"]][[1]],
                                     summary(aov(JuniorAll.acu[grade.j,part]~JuniorAll[grade.j,"school"]))[[1]][["Pr(>F)"]][[1]]),
                                   silent = T)
                if(class(part.summary) == "try-error")
                    gradeResult = append(gradeResult, rep(NA, 2))
                else
                    gradeResult = append(gradeResult, part.summary)
            }
            anoSchool = rbind(anoSchool, gradeResult)
            anoSchool.col1 = append(anoSchool.col1, paste0(j,"年級各校間"))
        }
    }

    for(part in 1:length(JuniorAll.acu)){
        schoolResult = cbind(schoolResult,
                            summary(aov(JuniorAll.acu[,part]~JuniorAll$"school"))[[1]][["F value"]][[1]],
                            summary(aov(JuniorAll.acu[,part]~JuniorAll$"school"))[[1]][["Pr(>F)"]][[1]])
    }
    anoSchool.col1 = append(anoSchool.col1, "各校間")
    anoSchool = cbind.data.frame(anoSchool.col1, rbind(anoSchool, schoolResult))

    names(anoSchool) = c("變異來源",rep(c("F值","P值"),length(JuniorAll.acu)))
    write.csv(anoSchool, paste0(anoSchool.name, ".csv"), row.names = F)

    return(anoSchool)
}

##### 表4 實驗對照間 #####
CreateAnoControl = function(JuniorAll, JuniorAll.acu, anoControl.name, x.control, x.compare.good, x.compare.norm){
    if(length(unique(JuniorAll$school))<2){
        return(NULL)
    }
    anoControl = c()
    anoControl.col1 = c()
    control = JuniorAll$school %in% x.control
    if(missing(x.compare.good)){
        for(j in unique(JuniorAll$grade)){
            gradeResult = c()
            grade.j = which(JuniorAll[,"grade"] == j)
            for(part in 1:length(JuniorAll.acu)){
                part.summary = try(c(summary(aov(JuniorAll.acu[grade.j,part]~control[grade.j]))[[1]][["F value"]][[1]],
                                     summary(aov(JuniorAll.acu[grade.j,part]~control[grade.j]))[[1]][["Pr(>F)"]][[1]]),
                                   silent = T)
                if(class(part.summary) == "try-error")
                    gradeResult = append(gradeResult, rep(NA, 2))
                else
                    gradeResult = append(gradeResult, part.summary)
            }
            anoControl = rbind(anoControl, c(paste0(j,"年級實驗對照"),round(gradeResult,4)))
        }
        anoControl = as.data.frame(anoControl)
        names(anoControl) = c("變異來源", rep(c("F值","P值"),length(JuniorAll.acu)))
    }
    else{
        for(j in unique(JuniorAll$grade)){
            anoControl.good = NULL
            anoControl.norm = NULL
            grade.j = which(JuniorAll$grade == j)
            good = which(JuniorAll[grade.j,"school"] %in% x.compare.good)
            norm = which(JuniorAll[grade.j,"school"] %in% x.compare.norm)
            for(part in 1:length(JuniorAll.acu)){
                good.summary = try(c(summary(aov(JuniorAll.acu[grade.j,part][-norm]~control[grade.j][-norm]))[[1]][["F value"]][[1]],
                                     summary(aov(JuniorAll.acu[grade.j,part][-norm]~control[grade.j][-norm]))[[1]][["Pr(>F)"]][[1]]),
                                   silent = T)
                norm.summary = try(c(summary(aov(JuniorAll.acu[grade.j,part][-good]~control[grade.j][-good]))[[1]][["F value"]][[1]],
                                     summary(aov(JuniorAll.acu[grade.j,part][-good]~control[grade.j][-good]))[[1]][["Pr(>F)"]][[1]]),
                                   silent = T)
                if(class(good.summary) == "try-error" | class(norm.summary) == "try-error"){
                    anoControl.good = append(anoControl.good, rep(NA, 2))    
                    anoControl.norm = append(anoControl.norm, rep(NA, 2))
                }
                else{
                    anoControl.good = append(anoControl.good,good.summary)    
                    anoControl.norm = append(anoControl.norm,norm.summary)
                }
                
            }
            if(length(anoControl.good) < 2*ncol(JuniorAll.acu)){
                anoControl.good = append(anoControl.good, rep(NA, 2*ncol(JuniorAll.acu)-length(anoControl.good)))
                anoControl.norm = append(anoControl.norm, rep(NA, 2*ncol(JuniorAll.acu)-length(anoControl.norm)))
            }
            anoControl = rbind(anoControl, anoControl.good, anoControl.norm)
            anoControl.col1 = append(anoControl.col1, paste0(j, "年級實驗對照", c("(優異)", "(一般)")))
        }
        anoControl = cbind.data.frame(anoControl.col1, anoControl)
        names(anoControl) = c("變異來源", rep(c("F值","P值"),length(JuniorAll.acu)))
    }
    write.csv(anoControl, paste0(anoControl.name, ".csv"), row.names = F)

    return(anoControl)
}


##### Other Functions #####
CreateTables = function(JuniorAll, JuniorAll.acu, x.control, x.compare.good, x.compare.norm){
    invisible(CreateAcuAll(JuniorAll, JuniorAll.acu, "Table 1 AcuAll"))
    print("done Table 1")
    invisible(CreateAnoClass(JuniorAll, JuniorAll.acu, "Table 2 AnoClass"))
    print("done Table 2")
    invisible(CreateAnoSchool(JuniorAll, JuniorAll.acu, "Table 3 AnoSchool"))
    print("done Table 3")
    if(!missing(x.compare.good) & !missing(x.compare.norm)){
        invisible(CreateAnoControl(JuniorAll, JuniorAll.acu, "Table 4 AnoControl", x.control, x.compare.good, x.compare.norm))
    }else{
        invisible(CreateAnoControl(JuniorAll, JuniorAll.acu, "Table 4 AnoControl", x.control))
    }
    print("done Table 4")
}
