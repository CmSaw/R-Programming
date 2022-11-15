#Saw Chiou Ming
#TP063734

install.packages('pacman') #packages manager #Extra Feature 1
library(pacman) 
p_load(ggplot2,modest,dplyr,fmsb,scales,vcd) #Extra Feature 2

f_rank <- function(x) #an user-defined function to rank all the students' G3 result
{
  v <- c()
  for(grade in x)
  {
    if (grade > 18)
    {
      v = c(v,"A")
    }else if (grade > 15)
    {
      v = c(v,"A-")
    }else if (grade > 13)
    {
      v = c(v,"B")
    }else if (grade > 10)
    {
      v = c(v,"B-")
    }else if (grade > 8)
    {
      v = c(v,"C")
    }else if (grade > 5)
    {
      v = c(v,"C-")
    }else
    {
      v = c(v,"F")
    }
  }
  return(v)
}

#dataset path
students = read.csv("C:\\Users\\Saw Chiou Ming\\Documents\\uni\\Year 2 Sem 1\\PFDA\\Assignment\\student.csv",header = TRUE)
score = students$G3
students = mutate(students,Rank = f_rank(score))
View(students)



#question 1: What is the performance of the students' grade
#analysis 1-1 data exploration on students' grade 1
summary(students$G1)
ggplot(students,aes(x=G1)) + 
  geom_histogram(binwidth = 0.5,color = 'white',aes(fill=..count..)) + 
  geom_text(aes(label = ..count..),stat = "count",vjust = -0.3) +
  ggtitle("Histogram of students' first period grade ")

#analysis 1-2 data exploration on students' grade 2
summary(students$G2)
ggplot(students,aes(x=G2)) + 
  geom_histogram(binwidth = 0.5,color = 'white',aes(fill=..count..)) + 
  geom_text(aes(label = ..count..),stat = "count",vjust = -0.3) +
  scale_fill_gradient(low='purple',high='yellow') + 
  ggtitle("Histogram of students' second period grade ")

#analysis 1-3 data exploration on students' grade 3
summary(students$G3)
ggplot(students,aes(x=G3)) + 
  geom_histogram(binwidth = 0.5,color = 'black',aes(fill=..count..)) + 
  geom_text(aes(label = ..count..),stat = "count",vjust = -0.3) +
  scale_fill_gradientn(colours = rainbow(7)) + #Extra Feature 3 & 4
  ggtitle("Histogram of students' third period grade ")

#analysis 1-4 Find the correlation between G1 and G3
cor(students$G1, students$G3,method = "pearson") #Extra Feature 5
ggplot(students, aes(x=G1,y=G3)) + geom_point()+
  stat_smooth(method = lm) + 
  ggtitle("Scatter Plot of 1st Grade v.s. 3rd Grade")

#analysis 1-5 Find the correlation between G2 and G3
cor.test(students$G2, students$G3,method = "pearson") #Extra Feature 6
ggplot(students, aes(x=G2,y=G3)) + geom_point() +
  stat_smooth(method = lm) + 
  ggtitle("Scatter Plot of 2nd Grade v.s. 3rd Grade")

#analysis 1-6 Showing proportion of each rank
rank_df = count(students,Rank) %>% mutate(perc = n / sum(n)) %>% 
  mutate(labels = percent(perc)) #Extra Feature 7

ggplot(rank_df, aes(x="",y=perc,fill = Rank)) + 
  geom_bar(stat="identity",color="black") +
  coord_polar("y", start=0) + #Extra Feature 8
  geom_text(aes(label =labels), position=position_stack(vjust =0.5)) + 
  ggtitle("Pie Chart of Percentage of Students at Each Level") + 
  theme_void() #Extra Feature 9



#question 2: Does student's status affects their grades
#analysis 2-1 Find the relationship between student's school and student's grade
school_df1 = as.data.frame(students %>% group_by(school) %>% summarise(mean(G3)))
school_df2 = count(students,school)
school_df3 = cbind(school_df1,school_df2[,2])
names(school_df3) = c("school","average G3","No. of Students")
school_df3

ggplot(students,aes(y=G3,fill = school)) + geom_histogram(binwidth = 0.5) + 
  ggtitle("Histogram of Students Grade 3 Result based on School")

#analysis 2-2 Find the relationship between student's sex and student's grade
sex_df = as.data.frame(students %>% group_by(sex) %>% 
                          summarise("average G3" = mean(G3),n=n()))
sex_df

ggplot(students,aes(x=G3,fill = sex)) + geom_bar(position = "fill", width = 0.5) + 
  ggtitle("Histogram of Students Grade 3 Result based on Sex")

#analysis 2-3 Find the relationship between student's age and student's grade
age_df = as.data.frame(students %>% group_by(age) %>% 
                          summarise("average G3" = mean(G3),n=n()))
age_df

ggplot(students,aes(x=factor(age),y=G3)) + geom_boxplot(aes(color = factor(age))) + 
  ggtitle("Boxplot of Students Grade 3 Result based on their Age") +
  xlab("student's age") #Extra Feature 10

#analysis 2-4 Find the relationship between student's home type and student's grade
add_df = as.data.frame(students %>% group_by(address) %>% 
                          summarise("average G3" = mean(G3),n=n())) 
add_df

ggplot(students,aes(x=address,y=G3)) + geom_violin(aes(color= address)) + #Extra Feature 11
  ggtitle("Violin Plot of Students Grade 3 Result based on their home type")



#question 3: Will family status affect students grade
#analysis 3-1 Find the relationship between parent's cohabitation status and students Rank
nrow(students[students$Pstatus == 'A',])
nrow(students[students$Pstatus == 'T',])
  
ggplot(students, aes(x=Rank,fill=Pstatus)) + 
  geom_bar(stat = "count", position = "dodge",width = 0.5) +
  geom_text(aes(label = ..count..),stat = "count",
            position = position_dodge(width = 0.5),vjust=-0.3) +
  scale_fill_brewer(palette="Dark2") + #Extra Feature 12
  ggtitle("Column Bar of parent's cohabitation status v.s Student Rank")

#analysis 3-2 Find the relationship between guardian and student Grade & Rank
count(students,guardian)

ggplot(students, aes(x=Rank,fill=guardian)) + 
  geom_bar(stat = "count", position = "stack",width = 0.5) +
  ggtitle("Stacked Bar Chart of guardian by Rank")

#analysis 3-3 If guardian is father, find out the father's job & education level along with average G3
Fdf = filter(students,guardian == "father") %>% select(c(Fjob,Fedu,G3)) %>% 
  group_by(Fedu,Fjob) %>% summarise(n(),round(mean(G3),2))
names(Fdf)[3:4] = c("n","average G3")
as.data.frame(Fdf) 

ggplot(data = Fdf,aes(x=factor(Fedu),y=`average G3`,fill = Fjob)) + geom_col(position = "dodge") +
  geom_text(aes(label =n),position=position_dodge(width =1)) +
  ggtitle("Column chart of Father Education v.s G3 Based on Father Job") + 
  xlab("Father Education Level")

#analysis 3-4 If guardian is mother, find out the mother's job & education level along with average G3
Mdf = filter(students,guardian == "mother") %>% select(c(Mjob,Medu,G3)) %>% 
  group_by(Medu,Mjob) %>% summarise(n(),round(mean(G3),2))
names(Mdf)[3:4] = c("n","average G3")
as.data.frame(Mdf)

ggplot(data = Mdf,aes(x=factor(Medu),y=`average G3`,fill = Mjob)) + geom_col(position = "dodge") +
  geom_text(aes(label =n),position=position_dodge(width =1)) +
  ggtitle("Column chart of Mother Education v.s G3 Based on Mother Job")+ 
  xlab("Mother Education Level")

#analysis 3-5 Find the relationship between family relationship and students grade
ggplot(students,aes(x=factor(famrel),y=G3)) + 
  geom_boxplot(aes(fill = factor(famrel))) + xlab("family relationship") +
  ggtitle("Boxplot of family relationship v.s Student Grade") 

#analysis 3-6 Find the relationship between family size, family relationship and students grade
ggplot(students,aes(x=factor(famrel),y=G3)) + 
  geom_boxplot(aes(fill = factor(famrel))) + facet_wrap(~famsize) +
  ggtitle("Boxplot of family size and relationship v.s Student Grade") +
  xlab("family relationship")



#question 4: Whether grades vary with the amount of time students spend on studying
#analysis 4-1 Find the relationship between travel time and students grade
ggplot(students,aes(x=factor(traveltime),y=G3,color=traveltime)) + 
  geom_dotplot(binaxis = "y",binwidth = 0.1) + #Extra Feature 13
  ggtitle("Dotplot of Travel Time v.s Student Grade") + xlab("travel time (hour)")

#analysis 4-2 Find the relationship between study time and students grade
ggplot(students,aes(x=factor(studytime),y=G3,fill = "brown")) + 
  geom_dotplot(binaxis = "y",stackdir = "center",binwidth = 0.2,col = "brown") +
  ggtitle("Dotplot of Study Time v.s Student Grade") + xlab("study time (hour)")

#analysis 4-3 Find the relationship between free time and students grade
ggplot(students,aes(x=freetime,y=G3,color = freetime)) + 
  geom_count() + #Extra Feature 14
  ggtitle("Count Plot of Free Time v.s Student Grade")

#analysis 4-4 Find the relationship between going out time and students grade
ggplot(students,aes(x=goout,y=G3)) + 
  geom_count(color = "springgreen3") +
  ggtitle("Count Plot of Going Out Time v.s Student Grade") +
  stat_summary(fun = mean,geom="line",color = "blue") #Extra Feature 15

#analysis 4-5 For those who have extra-curricular activities, does it affected grade
ggplot(students,aes(x=activities,y=G3)) + geom_violin(aes(fill = activities)) + 
  geom_boxplot(width = 0.3) + 
  ggtitle("Violin Plot of Students Grade 3 Result based on having extra-curricular activities")

#analysis 4-6 For those who have romantic relation, does it affected grade
ggplot(students,aes(x=romantic,y=G3)) + geom_violin(aes(color = romantic)) + 
  geom_boxplot(aes(fill = romantic),width = 0.3) +
  ggtitle("Violin Plot of Students Grade 3 Result based on having romantic relationship")



#question 5: The association between student grade and student health status
#analysis 5-1 Find the relationship between weekday alcohol consumption and students grade
ggplot(count(students,Dalc,G3),aes(x=Dalc,y=G3,fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "sky blue", high = "yellow") +
  geom_text(aes(label = n)) +
  ggtitle("Heat Map of weekday alcohol consumption vs G3 grade")

#analysis 5-2 Find the relationship between weekend alcohol consumption and students grade
ggplot(count(students,Walc,G3),aes(x=Walc,y=G3,fill = n)) +
  geom_tile() + #Extra Feature 16
  scale_fill_gradient(low = "brown", high = "orange") +
  geom_text(aes(label = n),color = "white") + 
  ggtitle("Heat Map of weekend alcohol consumption vs G3 grade")

#analysis 5-3 relationship between number of absences and students grade
ggplot(students,aes(y=G3,x=absences,color = Rank)) + 
  geom_line() + geom_jitter() + #Extra Feature 17 & 18
  ggtitle("Line and Jitter Plot of number of absences within each rank")

#analysis 5-4 relationship between health and students grade
ggplot(students,aes(x=factor(health))) + 
  geom_bar(stat = "count",aes(color = Rank,fill =  Rank,),position="dodge") +
  ggtitle("Column chart of Student Rank Proportion Based on Health") +
  xlab("health")



#question 6: Does additional support for education have an impact on student achievement?
#analysis 6-1 Association between the availability of internet and student grade
mosaic(~ Rank + internet,students, main = "Mosaic Plot of Available for Network v.s Rank",
       highlighting = "internet",highlighting_fill = hcl.colors(2)) #Extra Feature 19

#analysis 6-2 Association between the presence or absence of nursery, extra paid classes, 
              #family and school support and student achievement
support_df = select(students,c(schoolsup,famsup,nursery,paid,G3)) %>% 
  group_by(schoolsup,famsup,nursery,paid) %>% summarise(n(),mean(G3))
names(support_df)[6] = "average_G3"
as.data.frame(support_df)

#analysis 6-3 Visualize analysis 6-2
ggplot(support_df,aes(x=schoolsup,y=average_G3)) + geom_boxplot() + 
  ggtitle("boxplot of school support v.s. average G3")
ggplot(support_df,aes(x=famsup,y=average_G3)) + geom_boxplot() + 
  ggtitle("boxplot of family support v.s. average G3")
ggplot(support_df,aes(x=nursery,y=average_G3)) + geom_boxplot() + 
  ggtitle("boxplot of nursery v.s. average G3")
ggplot(support_df,aes(x=paid,y=average_G3)) + geom_boxplot() + 
  ggtitle("boxplot of extra paid classes v.s. average G3")


#question 7: Does a student's enthusiasm for learning affect their academic performance
#analysis 7-1 Investigate different ranks of students' reasons for choosing schools
ggplot(students, aes(x=reason,y=Rank)) + geom_jitter(aes(color = Rank),width = 0.2) +
  ggtitle("Jitter Plot of students' reasons for choosing schools on different rank")

#analysis 7-2 Does a student's desire to progress to higher education affect grades
higher_df = select(students,c(higher,G3)) %>% group_by(higher) %>% summarise(n(),mean(G3))
as.data.frame(higher_df)

ggplot(students, aes(x=higher,y=G3)) + geom_violin(aes(fill = higher)) + 
  stat_summary(fun=mean, geom="point") +
  ggtitle("Violin Plot of student's desire to higher education v.s. G3 grade")

#analysis 7-3 The relationship between the number of past failures and their grade
ggplot(students, aes(failures,G3)) + geom_count() + 
  stat_summary(fun=mean, geom="line",color = "blue") +
  ggtitle("Count Plot of relationship between student's failures and their G3")


  


#question 8: From analysis 1-3, find out the reason that those students get 0 in G3
#analysis 8-1 Find out how these students have performed in the past
par(mfrow = c(1,2)) #Extra Feature 20
hist(students$G1[students$G3 == 0],main = "Histogram of G1 of Students who get 0 in G3"
     ,xlab = "G1 score")
hist(students$G2[students$G3 == 0],main = "Histogram of G2 of Students who get 0 in G3"
     ,xlab = "G2 score")
par(mfrow = c(1,1))

#analysis 8-2 From analysis 8-1 find out those 27 students' school
bad = subset(students,G3 == 0 & G2 == 0)
ggplot(bad,aes(x=school,fill=school)) + geom_bar() +
  ggtitle("Bar chart of school for students who scored 0 in G2 and G3")

#analysis 8-3 From analysis 8-1 find out those 27 students' family relationship
ggplot(bad,aes(x=factor(famrel),fill=famrel)) + geom_bar() +
  ggtitle("Bar chart of family relationship for students who scored 0 in G2 and G3")

#analysis 8-4 From analysis 8-1 find out those 27 students' Daily Alcohol Consumption
ggplot(bad,aes(x=factor(Dalc),fill=Dalc)) + geom_bar() +
  ggtitle("Bar chart of Daily Alcohol Consumption for students who scored 0 in G2 and G3")

#analysis 8-5 From analysis 8-1 find out whether those 27 students' having school support
ggplot(bad,aes(x=schoolsup,fill=schoolsup)) + geom_bar() +
  ggtitle("Bar chart of  school support for students who scored 0 in G2 and G3")

#analysis 8-6 From analysis 8-1 find out whether those 27 students' having extra paid class
ggplot(bad,aes(x=paid,fill=paid)) + geom_bar() +
  ggtitle("Bar chart of extra paid class for students who scored 0 in G2 and G3")



#question 9: What are the tendencies of students who receive a grade A
#analysis 9-1 Represent continuous attributes with radar chart based on rank (Grade excluded)
df = select(students,c(Rank,traveltime,studytime,freetime,goout,Medu,Fedu,
                       health,Dalc,Walc))
start_df = df %>% group_by(Rank) %>% summarise(mean(traveltime),mean(studytime)
                                               ,mean(freetime),mean(goout)
                                               ,mean(Medu),mean(Fedu),
                                               mean(health),mean(Dalc),mean(Walc))
start_df = as.data.frame(start_df)
names(start_df) <- c("Rank","TravelTime","StudyTime","FreeTime","Go Out"
                     ,"Mom edu","Dad edu","Health","Dalc","Walc")

col_max = apply(subset(df,select = -Rank),2,max) #Extra Feature 21
col_min = apply(subset(df,select = -Rank),2,min)

final_df = rbind(col_max,col_min,start_df)
final_df = subset(final_df,select = -Rank)

ranklabels = c("Rank A","Rank A-","Rank B","Rank B-","Rank C","Rank C-","Fail")
color = topo.colors(nrow(final_df)-2) #Extra Feature 22

radarchart(final_df,plty = 1,pfcol = alpha(color,0.1), #Extra Feature 23 & 24
           pcol = color,title="Radar Chart of multiple continuous attributes by Rank")
legend(
  x="bottomright",horiz = FALSE, legend = ranklabels,
  bty = "n", pch = 20 , col = color) #Extra Feature 25

#analysis 9-2 From analysis 9-1, separate the radar chart to justify Rank A students' tendencies
par(mfrow = c(2,4))
color = topo.colors(nrow(final_df)-2)
for(i in 1:7)
{
  radarchart(final_df[c(0:2,i+2),],plty = 1,pfcol = alpha(color[i],0.1),
             pcol = color[i],title = ranklabels[i])
}
par(mfrow = c(1,1))

#analysis 9-3 List out every Boolean attributes of grade A students
filter(students,Rank == "A") %>%
  count(schoolsup,famsup,paid,activities,nursery,higher,internet,romantic)

