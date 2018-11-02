library(Scale)

CreateReva = function(RealAns, type, fm = "gls", tableName = "Table 5 Reliability and Validity"){
    ans = RealAns[,5:ncol(RealAns)]
    if(type == "junior"){
        part.num = c(3,3,3,6,9)
        part = rep(1:5, part.num)
        # part[5] = NA
        part.name = c("L1", "L2", "L3", "R1", "R2")
    }
    else if(type == "elementary"){
        part.num = c(5,5,10,10,5,5,5,5,5,5)
        part = rep(1:10, part.num)
        part.name = paste0("Part", 1:10)
    }
    else{
        print("not support type for Reliability & Validity Table")
        return(NULL)
    }
    
    ReVa = c() # Reliability and Validity table
    for(i in 1:length(part.num)){
        ## step 1: remove duplicated/useless columns in this part
        qus = ans[,which(part == i)]                        # obtain questions in this part
        DP_qus = which(duplicated(lapply(qus, c)) == TRUE)  # get questions which have the same result
        NG_qus = which(lapply(qus, function(x)length(unique(na.exclude(x)))) < 2)  # get questions which have homogenious result(all 1 or 0s)
        
        bad_qus = c(DP_qus, NG_qus)
        if(length(bad_qus)){
            qus = qus[, -bad_qus]
            cat("\n")
            if(length(DP_qus)){
                print(paste(c(paste("part", i, "Duplicated:"), DP_qus), collapse=" "))
            }
            if(length(NG_qus)){
                print(paste(c(paste("part", i, "Useless:   "), NG_qus), collapse=" "))
            }
        }
        ## step 2: Start ItemAnalysis
        scale = Scale(qus)
        pre = suppressWarnings(PreProc(na.exclude(scale)))
        rel = suppressWarnings(ItemAnalysis(pre, fm = fm)) # fm is method to extract factor loadings
                                                           # see ?psych::fa
        # step 3: summarize Alpha and factor loadings
        v = c()
        for(q in 1:part.num[i]){
            if(q %in% bad_qus){
                v = append(v, NA)
            }
            else{
                v = append(v, rel$valid$loadings[1])
                rel$valid$loadings = rel$valid$loadings[-1]
            }
        }
        if(length(v) < max(part.num)){
            v = append(v, rep(NA, max(part.num)-length(v))) # add NA to this part's factor loadings if necessary
        }
        v = append(as.numeric(rel$rely$alpha$total[1]), v) # alpha, factor loadings
        ReVa = rbind(ReVa, v)
    }
    ReVa = as.data.frame(ReVa)
    names(ReVa) = c("Alpha", paste0("Q", 1:max(part.num)))
    row.names(ReVa) = part.name    
    write.csv(ReVa, paste0(tableName, ".csv"))
    return(ReVa)
}

CreateReva_Concept = function(RealAns, type, fm = "gls", tableName = "Table 5 Reliability and Validity (Concept)"){
  ## init
    ans = RealAns[,5:ncol(RealAns)]
  ## type setting
    if(type == "junior"){
        concept = rep(1:2, c(9,15))
        part.name = c("L1", "L2", "L3", "R1", "R2")
        part.num = c(3,3,3,6,9)
        part = rep(1:5, part.num)
        # part[5] = NA
    }
    else if(type == "elementary"){
        # concept = rep(1:3, c(30, 20, 10))
        concept = c(rep(1, 20), rep(2,10), rep(3,15), rep(2,5), rep(4,5), rep(5,5))
        # 1: alphbet                part 1, 2, 3
        # 2: natural pronunciation  part 4, 8
        # 3: vocabularies           part 5, 6, 7
        # 4: listening              part 9
        # 5: reading                part 10
        part.name = paste0("Part", 1:10)
        part.num = c(5,5,10,10,5,5,5,5,5,5)
        part = rep(1:10, part.num)
    }
    else{
        print("Not supported type for Reliability and Validity table")
        return(NULL)
    }
  ## Calculate Reliability and Validity for each part
    Reva = c()
    for(i in 1:length(unique(concept))){
        # step 1: obtain qus and clean it
        qus = ans[,which(concept == i)]
        DP_qus = which(duplicated(lapply(qus, c)) == TRUE)
        NG_qus = which(lapply(qus, function(x)length(unique(na.exclude(x)))) < 2)
        bad_qus = c(DP_qus, NG_qus)
        if(length(bad_qus)){
            qus = qus[, -bad_qus]
            cat("\n")
            if(length(DP_qus)){
                print(paste(c(paste("Concept", i, "Duplicated:"), DP_qus), collapse=" "))
            }
            if(length(NG_qus)){
                print(paste(c(paste("Concept", i, "Useless:   "), NG_qus), collapse=" "))
            }
        }
        
        # step 2: start ItemAnalysis
        scale = Scale(qus)
        pre = suppressWarnings(PreProc(na.exclude(scale)))
        rel = suppressWarnings(ItemAnalysis(pre, fm = fm))
        
        # step 3: summarize Alpha and factor loadings in this Concept
        v = c() # factor loading of each questions in this concept
        for(k in 1:sum(concept == i)){
            if(k %in% unique(bad_qus))
                v = append(v, NA)
            else{
                v = append(v, rel$valid$loadings[1])
                rel$valid$loadings = rel$valid$loadings[-1]
            }
        }
        u = split(v, part[which(concept == i)]) # split v into each part of this concept
        for(p in 1:length(unique(part[which(concept == i)]))){
            if(length(u[[p]]) < max(part.num))
                u[[p]] = append(u[[p]], rep(NA, max(part.num)-length(u[[p]])))
            Reva = rbind(Reva, append(as.numeric(rel$rely$alpha$total[1]), u[[p]]))
        }
    }
    Reva = as.data.frame(Reva)
    names(Reva) = c("Alpha", paste0("Q", 1:max(part.num)))
    row.names(Reva) = part.name    
    write.csv(Reva, paste0(tableName, ".csv"))
    return(Reva)
}
