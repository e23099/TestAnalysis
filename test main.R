source("E:/1061_extracurricular/TC/TestAnalysis/readTest.R", encoding = "UTF-8")
source("E:/1061_extracurricular/TC/TestAnalysis/TableGenerate.R", encoding = "UTF-8")
source("E:/1061_extracurricular/TC/TestAnalysis/PlotGenerate.R", encoding = "UTF-8")

DataDir = "E:/1061_extracurricular/TC/Hsinchu/test result"
x = c("峨眉", "尖石", "成功", "華山")
x.control = c("峨眉",  "尖石")
x.compare.good = "成功"
x.compare.norm = "華山"
xlsx.name = c("總表-實驗峨眉國中",
              
              "總表-實驗尖石國中",
              "總表-對照成功國中",
              "總表-對照華山國中")
# sheet.name = c("106-1七年級", "106-1八年級")
sheet.name = "106-2八年級"

JuniorAll = CollectAll(x, DataDir, sheet.name, xlsx.name, "toeic8")
JuniorAll.acu = CreateAcu(JuniorAll, "junior")
CreateTables(JuniorAll, JuniorAll.acu, x.control, x.compare.good, x.compare.norm)
