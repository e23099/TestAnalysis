source("E:/1061_extracurricular/TC/TestAnalysis/readTest.R", encoding = "UTF-8")
source("E:/1061_extracurricular/TC/TestAnalysis/TableGenerate.R", encoding = "UTF-8")
source("E:/1061_extracurricular/TC/TestAnalysis/PlotGenerate.R", encoding = "UTF-8")
source("E:/1061_extracurricular/TC/TestAnalysis/Reliability_Validity.R", encoding = "UTF-8")

## manual settings
# step 1: choose data location
DataDir = "E:/1061_extracurricular/TC/Hsinchu/test result"

# step 2: choose which schools to analyze
x = getSchoolNames(DataDir, "junior")
x = x[c(3,1,2,4)] # change school's order if necessary (and even remove some school)

# step 3: choose which schools are in control group / compare group
x.control = getControlSchool(DataDir, "junior")
x.control = x.control[c(3,1,2)] # modify if needed
# x.compare.good = "??êÂ??"
# x.compare.norm = "?èØÂ±?"

# step 4: get xlsx names
xlsx.name = getXlsxFiles(DataDir, "junior")
xlsx.name = xlsx.name[c(3,1,2,4)] # modify if needed

# step 5: choose which sheet to read as data
sheet.name = c("106-1‰∏ÉÂπ¥Á¥?", "106-1?Ö´Âπ¥Á??")

## Analysis
# step 1: collect cleaned data
JuniorAll = CollectAll(x, DataDir, xlsx.name, sheet.name,"junior")
JuniorAll.acu = CreateAcu(JuniorAll, "junior")
JuniorAll.acu = Junior_weight_correct(JuniorAll.acu) # if project is junior, correct their accuracy rate

# step 2: create tables needed
CreateTables(JuniorAll, JuniorAll.acu, x.control, x.compare.good, x.compare.norm)

# step 3: create plots needed
PlotEverything(JuniorAll, JuniorAll.acu, "junior", x.control)

# step 4: create reliability-validity test
CreateReva(JuniorAll, "junior", fm = 'ml') # check if any printed message
                                           # also check if factor loadings is out of [-1,1], change fm = 'ml' or others
                                           # fm default is "gls"
CreateReva_Concept(JuniorAll, "junior")    # in printed message, the number of duplicated/useless questions is of the original order.(e.g. 12 means the 12th Q in that concept)
