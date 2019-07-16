# 1.  VECTORS 

varx = c(1,2,3,4)
varx[1:2]
is.character(varx)
seq(0,15,5)
names(varx) = c("first","second","third","fourth")
varx
varx = c(1,"two",TRUE,NULL)
as.list(varx)

#---------------------------------------------------------------
# 2.  MATRICES

matrix1 = matrix(nrow=3,ncol=3)
length(matrix1)
matrix2 = 1:9
dim(matrix2) = c(3,3)
matrix2

matrix3 = matrix(1:9, nrow=3, ncol=3, byrow=TRUE)
matrix3
class(matrix3)
typeof(matrix3)
attributes(matrix3)

matrix4 = cbind(matrix3, varx)
matrix4
matrix4[,4]
matrix5 = matrix3*5
matrix5

colnames(matrix3)=c("col1","col2","col3")
matrix3
rownames(matrix3)=c("row1","row2","row3")
matrix3

#---------------------------------------------------------------
# 3.  LISTS, like a vector but lists can contain other lists within elements (?)

list1 = list(One = 1,Two = "am",Three = TRUE)
list1
attributes(list1)

list2 = list(1,2,3)
names(list2) = c("one","two","three") #same as above
list2
attributes(list2)

str(list2) #in R, STR means structure, not string

list1[1] #name and value
list1[[1]] #just value
list1$One #case sensitive, gives value of 1, not name (like above)

#---------------------------------------------------------------
# 4.  DATA FRAMES

# df = data.frame(column1=1:5, column2=6:10)
df = data.frame(1:5, 6:10)
colnames(df) = c("col1","col2")
df

sample.df1 = as.data.frame(matrix3)
sample.df1

attributes(sample.df1)

sample.df1$col1 #column, dollarsign only works with columns
sample.df1$col1[1] #get coordinate value

sample.df1[1,] #row =, call by coordinate
sample.df1[,1] #column 

sample.df1[,"col1"] #column, call by name
sample.df1["row1",] #row

#add new column
sample.df1$col4 = 1:3 #created and named new column, name is $, equal is value within
sample.df1

str(sample.df1)

### factors are variables containing categorical data (names); each name is its own "level"

## to delete a column, assign it to NULL. sample.df1$col1=NULL delets col1. then check with sample.df1

