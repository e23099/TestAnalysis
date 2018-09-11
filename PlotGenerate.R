dir = getwd()
dir.create(paste0(dir,"/boxplots"), showWarnings = FALSE)

##### 各校各部分盒鬚圖 #####

PlotEachSchool = function(JuniorAll, JuniorAll.acu, type, k1 = 2, w1 = 850, h1 = 365, h11 = 270, maxClass = 10){
    if(type == "junior"){
        topic = c("聽力一答對率", "聽力二答對率", "聽力三答對率", "閱讀一答對率", "閱讀二答對率", "總答對率")
        topic.score = rep("答對率", 6)
        topic.maxScore = rep(1,6)
    }
    else if(type == "elementary"){
        topic = c(paste0("第",1:10,"部分答對率"), "總答對率")
        topic.score = rep("答對率", 11)
        topic.maxScore = rep(1,11)
    }
    else if(type == "toeic8"){
        topic = c("聽力成績", "閱讀成績", "總成績")
        topic.score = rep("分數", 3)
        topic.maxScore = c(90, 90, 180)
    }
    else{
        print("not supported type")
        return(NULL)
    }

    for(i in 1:length(x)){
        dir.create(paste0(dir,"/boxplots/school",i), showWarnings = FALSE)      # 在boxplots中建立schooli資料夾
        students = which(JuniorAll$school == x[i])                              # 擷取第 i 間學校的學生
        school = JuniorAll.acu[students,]                                       # 擷取用來畫圖的資料中屬於該校的部分

        if(length(unique(JuniorAll$grade)) == 1){                               # 檢查是否只有一個年級
            if(length(unique(JuniorAll[students, "class"]))>maxClass){                # 若只有一年級，檢查該校班級數 > 10(maxClass)
                    for(tp in 1:length(topic)){
                        png(file = paste0(dir, "/boxplots/school", i, "/",tp,".png"), width = w1, height = h11)
                        par(mfrow=c(1,1), cex.main=2, cex.lab = 1.5, cex.axis = 1.5)
                        boxplot(school[,tp]~factor(JuniorAll[students, "class"], unique(JuniorAll[students, "class"])),
                                xlab="",ylab=topic.score[tp],
                                main=paste0(x[i],"各班",topic[tp],"盒鬚圖"),
                                col="lightgray",ylim=c(0,topic.maxScore[tp]))
                        dev.off()
                    }
                }
            else{
                    for(tp in seq(1,length(topic), by = k1)){
                        png(file = paste0(dir, "/boxplots/school", i, "/",tp,".png"), width = w1, height = h1)
                        par(mfrow=c(1,k1), cex.main=2, cex.lab = 1.5, cex.axis = 1.5)
                        for(j in 0:(k1-1)){
                            try(boxplot(school[,tp+j]~factor(JuniorAll[students, "class"], unique(JuniorAll[students, "class"])),
                                    xlab="",ylab=topic.score[tp+j],main=paste0(x[i],"國中各班",topic[tp+j],"盒鬚圖"),col="lightgray",ylim=c(0,topic.maxScore[tp+j])),
                                silent = T)
                            if(length(unique(JuniorAll[students, "class"])) < 2)
                                axis(1, at = 1, unique(JuniorAll[students, "class"]))
                        }
                        dev.off()
                    }
                }
        }
        else{
            for(tp in seq(1,length(topic), by = k1)){
                png(file = paste0(dir, "/boxplots/school", i, "/",tp,".png"), width = w1, height = h1)
                par(mfrow=c(1,k1), cex.main=2, cex.lab = 1.5, cex.axis = 1.5)
                for(j in 0:(k1-1)){
                    try(boxplot(school[,tp+j]~factor(JuniorAll[students, "grade"], unique(JuniorAll[students, "grade"])),
                            xlab="",ylab=topic.score[tp+j],
                            main=paste0(x[i],"各年級",topic[tp+j],"盒鬚圖"),
                            col="lightgray",ylim=c(0,topic.maxScore[tp+j])), silent = T)
                    if(length(unique(JuniorAll[students, "grade"])) < 2)
                        axis(1, at = 1, unique(JuniorAll[students, "grade"]))
                }
                dev.off()
            }
        }
    }
}

##### 全體各部分盒鬚圖 #####

PlotAllSchool = function(JuniorAll, JuniorAll.acu, x.control, type, k2 = 2, w2 = 850, h2 = 365){
    if(length(unique(JuniorAll$school))<2){
        return(NULL)
    }
    color = rep("lightgray", length(x))
    color[x %in% x.control] = "orange"
    dir.create(paste0(dir,"/boxplots/overall"), showWarnings = FALSE)
    
    if(type == "junior"){
        topic = c("聽力一答對率", "聽力二答對率", "聽力三答對率", "閱讀一答對率", "閱讀二答對率", "總答對率")
        topic.score = rep("答對率", 6)
        topic.maxScore = rep(1,6)
    }
    else if(type == "elementary"){
        topic = c(paste0("第",1:10,"部分答對率"), "總答對率")
        topic.score = rep("答對率", 11)
        topic.maxScore = rep(1,11)
    }
    else if(type == "toeic8"){
        topic = c("聽力成績", "閱讀成績", "總成績")
        topic.score = rep("分數", 3)
        topic.maxScore = c(90, 90, 180)
    }
    else{
        print("not supported type")
        return(NULL)
    }
    
    if(length(unique(JuniorAll$grade))>1){
        # each grade
        for(j in unique(JuniorAll$grade)){
            dir.create(paste0(dir,"/boxplots/overall/grade", j), showWarnings = FALSE)
            grade.j = which(JuniorAll$grade == j)
            plt.count = 0
            for(tp in 1:length(topic)){
                if(plt.count %% k2 == 0){
                    png(file = paste0(dir, "/boxplots/overall/grade",j,"/", tp, ".png"), width = w2, height = h2)
                    par(mfrow=c(1,k2), cex.main=2, cex.lab = 1.5, cex.axis = 1.5)    
                }
                
                if(sum(!is.na(JuniorAll.acu[grade.j,tp]))){
                    boxplot(JuniorAll.acu[grade.j,tp]~factor(JuniorAll[grade.j, "school"],x),
                                xlab="",ylab=topic.score[tp],main=paste0("各校",j,"年級",topic[tp],"盒鬚圖"),
                                col=color,ylim=c(0,topic.maxScore[tp]))
                    plt.count = plt.count + 1
                }
                
                if(plt.count %% k2 == 0) dev.off()
            }
            try(dev.off(), silent = T) # close last png file if necessary
        }
    }
    
    # overall
    for(tp in seq(1,length(topic), by = k2)){
        png(file = paste0(dir, "/boxplots/overall/", tp, ".png"), width = w2, height = h2)
        par(mfrow=c(1,k2), cex.main=2, cex.lab = 1.5, cex.axis = 1.5)
        for(j in 0:(k2-1)){
            try(boxplot(JuniorAll.acu[,tp+j]~factor(JuniorAll[, "school"],x),
                    xlab="",ylab=topic.score[tp+j],main=paste0("各校",topic[tp+j],"盒鬚圖"),
                    col=color,ylim=c(0,topic.maxScore[tp+j])),
                silent = T)
        }
        dev.off()
    }
}

##### Other Functions #####
PlotEverything = function(JuniorAll, JuniorAll.acu, type, x.control){
    if(type == "junior"){
        PlotEachSchool(JuniorAll, JuniorAll.acu, type)
        print("done individual plot")
        PlotAllSchool(JuniorAll, JuniorAll.acu, x.control, type)
        print("done overall plot")
    }
    else if(type == "elementary"){
        PlotEachSchool(JuniorAll, JuniorAll.acu, type, k1 = 3, h1 = 270)
        print("done individual plot")
        PlotAllSchool(JuniorAll, JuniorAll.acu, x.control, type, k2 = 3, h2 = 270)
        print("done overall plot")
    }
    else if(type == "toeic8"){
        PlotEachSchool(JuniorAll, JuniorAll.acu, type, k1 = 3, h1 = 270)
        print("done individual plot")
        PlotAllSchool(JuniorAll, JuniorAll.acu, x.control, type, k2 = 3, h2 = 270)
        print("done overall plot")
    }
    else{
        print("not supported type")
    }
}