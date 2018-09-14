library(Scale)

CreateReva = function(RealAns, type, fm = "gls", tableName = "Table 5 Reliability and Validity"){
    ans = RealAns[,5:ncol(RealAns)]
    if(type == "junior"){
        part.num = c(3,3,3,6,9)
        part = rep(1:5, part.num)
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
        NG_qus = c()                                        # get questions which have homogenious result(all 1 or 0s)
        for(j in 1:ncol(qus)){
            if(length(unique(na.exclude(qus[,j]))) < 2)
                NG_qus = append(NG_qus, j)
        }
        
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


