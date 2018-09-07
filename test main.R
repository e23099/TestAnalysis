source("E:/1061_extracurricular/TC/TestAnalysis/readTest.R", encoding = "UTF-8")
source("E:/1061_extracurricular/TC/TestAnalysis/TableGenerate.R", encoding = "UTF-8")
source("E:/1061_extracurricular/TC/TestAnalysis/PlotGenerate.R", encoding = "UTF-8")

## manual settings
# step 1: choose data location
DataDir = "E:/1061_extracurricular/TC/Hsinchu/test result"

# step 2: choose which schools to analyze
x = getSchoolNames(DataDir, "junior")
x = x[c(3,1,4,5)] # change school's order if necessary (and even remove some school)

# step 3: choose which schools are in control group / compare group
x.control = getControlSchool(DataDir, "junior")
x.control = x.control[c(3,1)] # modify if needed
x.compare.good = "成功"
x.compare.norm = "華山"

# step 4: get xlsx names
xlsx.name = getXlsxFiles(DataDir, "junior")
xlsx.name = xlsx.name[c(3,1,4,5)] # modify if needed

# step 5: choose which sheet to read as data
grade = "七八" # or c("七", "八")
semester = "106-1"

## Analysis
# step 1: collect cleaned data
JuniorAll = CollectAll(x, DataDir, grade, semester, xlsx.name, "junior")
JuniorAll.acu = CreateAcu(JuniorAll, "junior")

# step 2: create tables needed
CreateTables(JuniorAll, JuniorAll.acu, x.control, x.compare.good, x.compare.norm)

# step 3: create plots needed
PlotEverything(JuniorAll, JuniorAll.acu, "junior", x.control)

