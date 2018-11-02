print("Should have sourced readTest.R")
dir = getwd()
dir.create(paste0(dir,"/compare"), showWarnings = FALSE)
##
# input two data frame, with ID on the first column
# return a list containing 2 dataframe: old, and new
##
Pairing = function(new, old){
    v = intersect(new[,1], old[,1])
    new = new[which(new[,1]%in%v),]
    old = old[which(old[,1]%in%v),]
    return(list(id = v, school = new$"school",new = new, old = old))
}

##
# input two vector of the same length, 
# return "df", "t", "p-value", "mean difference"
##
PairTtest = function(new, old, paired = T){
    comparison = try(t.test(new, old, paired = paired), silent = T)
    if(class(comparison) == "try-error"){
        print("t.test fails")
        return(NULL)
    }
    out = c(comparison[[2]],comparison[[1]],comparison[[3]],comparison[[5]])
    names(out) = c("df", "t", "p-value", "mean difference")
    return(out)
}

##
# input two dataframe, type of school, whether using paired sample t test
# return t test result of each school's total accuracy
##
CompareAcu = function(new, old, type, name="", paired = T){
    out = NULL
    if(paired){
       com = Pairing(new,old)
       newAcu = CreateAcu(com$new, type)
       oldAcu = CreateAcu(com$old, type)
       
       out = PairTtest(newAcu[,ncol(newAcu)], oldAcu[,ncol(oldAcu)])
    }
    else{
        newAcu = CreateAcu(com$new, type)
        oldAcu = CreateAcu(com$old, type)
        
        out = PairTtest(newAcu[,ncol(newAcu)], oldAcu[,ncol(oldAcu)], paired = F)
    }
    return(out)
}


##
# intput : two vectors of accuracy
# return : Paint Comparison histogram (old in gray, new in white, lightgray is overlap)
##
HistCompareAcu = function(new, old, slice, main, xlab, ylab){
    if(max(new,na.rm=T)>1 | max(old,na.rm=T)>1){
        print("accuracy > 1")
        return(NULL)
    }
    freq_new = max(hist(new, breaks = seq(0,1,by=slice),plot = FALSE)$counts)
    freq_old = max(hist(old, breaks = seq(0,1,by=slice),plot = FALSE)$counts)
    
    hist(old, breaks = seq(0,1,by=slice),col=rgb(0,0,0,0.5), main=main, xlab=xlab, ylab=ylab, ylim=c(0,max(freq_old,freq_new)))
    hist(new, breaks = seq(0,1,by=slice),col=rgb(1,1,1,0.5), add=T)
    box()
}

##
# intput : a vector of accuracy, main name
# return : Paint a histogram
##

HistAcu = function(acu, main, slice = .1, ylim = NULL){
    if(is.null(ylim))
        hist(acu, breaks = seq(0,1,by=slice),main=main,xlab="答對率", ylab="次數")
    else
        hist(acu, breaks = seq(0,1,by=slice),main=main,xlab="答對率", ylab="次數", ylim=c(0,ylim))
    box()
}

##
# input  : 2 dataframe, school type, test time for legend names, k plots a row, width, height, slice for histogram
# return : plot each part's accuracy histogram for each school 
##

PlotComparison = function(new, old, type, legendName,k, w, h, slice = 0.2){
    com = Pairing(new,old)
    newAcu = CreateAcu(com$new, type)
    oldAcu = CreateAcu(com$old, type)
    part.name = c(paste0("第",1:10,"部分\n答對率"), "總答對率")
    if(type == "junior"){
        part.name = paste0("國中",c("聽力一答對率", "聽力二答對率", "聽力三答對率", "閱讀一答對率", "閱讀二答對率", "總答對率"))
        newAcu = Junior_weight_correct(newAcu)
        oldAcu = Junior_weight_correct(oldAcu)
    }
    for(i in 1:length(unique(com$school))){
        plt_count = 0
        students = which(new$school == unique(com$school)[i])
        dir.create(paste0(dir,paste0("/compare/school",i)), showWarnings = FALSE)
        for(j in 1:length(newAcu)){
            if(plt_count %% k == 0){
                png(file = paste0(dir, "/compare/school",i,"/",j,".png"), width = w, height = h)
                par(mfrow=c(1,k), cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
            }
            HistCompareAcu(
                newAcu[students,j], oldAcu[students,j],
                main = paste0(unique(com$school)[i],part.name[j],"盒鬚圖"),
                xlab = "答對率", ylab = "人數"
            )
            legend("topleft", c(legendName,"重疊"), 
                    fill=c("gray46","white","gray"), cex=1.5,  horiz=T, bty="n")
            plt_count = plt_count+1
            if(plt_count %% k == 0) dev.off()
        }
        try(dev.off(), silent = T) # close last png file if necessary
    }
}

##
# input  : Paired Object, schools in experiment group, test time for legend names, k plots a row, width, height, slice for histogram
# return : plot each part's accuracy histogram for each school 
##

PlotComparison2 = function(new, old, type,name, w=850, h=360, main = c("前測","後測"), slice = .1){
    dir.create(paste0(dir,paste0("/compare/group")), showWarnings = FALSE)
    png(file = paste0(dir, "/compare/group/",name,".png"), width = w, height = h)
    par(mfrow=c(1,2), cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
    new=CreateAcu(new,type)
    old=CreateAcu(old,type)
    freq_new = max(hist(new[,ncol(new)], breaks = seq(0,1,by=slice),plot = FALSE)$counts)
    freq_old = max(hist(old[,ncol(old)], breaks = seq(0,1,by=slice),plot = FALSE)$counts)
    HistAcu(new[,ncol(new)], main[1], slice = slice, ylim = max(freq_old,freq_new))
    HistAcu(old[,ncol(old)], main[2], slice = slice, ylim = max(freq_old,freq_new))
    dev.off()
}
