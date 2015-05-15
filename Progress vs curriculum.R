#Track total progress during 2015 competition by exercise_name (maps to curriculum) 
#Compare progress made by different class_years (primary 3rd class to secondary 5th/6th year)

library(RMySQL)
# The following line connects to the database.  Fill in the required password.
con <- dbConnect(RMySQL::MySQL(), host = "mathletes2015.cxakoyfdtshs.eu-west-1.rds.amazonaws.com", 
                 dbname = "mathletes_anon", user = "reader", password = "?????")

progress_changes <- dbGetQuery(con, "SELECT * FROM progress_changes")

#add field "in_comp" which will indicate the time period as prior to 2015 competition start date (0) or during the competition (1)
progress_changes$in_comp <-ifelse(as.Date(progress_changes$date)<as.Date("2015-01-05"),"precomp","incomp")
#Establish what level was achieved on each activity
progress_changes$practiced <-ifelse(progress_changes$to_progress_level=="practiced",1,0)
progress_changes$mastery1 <-ifelse(progress_changes$to_progress_level=="mastery1",1,0)
progress_changes$mastery2 <-ifelse(progress_changes$to_progress_level=="mastery2",1,0)
progress_changes$mastery3 <-ifelse(progress_changes$to_progress_level=="mastery3",1,0)

#Group by user_id, exercise_name and in_comp - this condenses all activity per user on a particular activity to the highest mastery level achieved
progress_changes <-aggregate(cbind(practiced, mastery1, mastery2, mastery3)~user_id+exercise_name+in_comp, data=progress_changes, FUN=sum)


progress_changes$score <-ifelse(progress_changes$mastery3>0,3,ifelse(progress_changes$mastery2>0,2.5,ifelse(progress_changes$mastery1>0,1.5,ifelse(progress_changes$practiced>0,1,0))))

#Calculate cumulative progress on each activity per user at the end of 2014 and 2015 competitions 
progress_changes$pre_comp <-ifelse(progress_changes$in_comp=="precomp", progress_changes$score, 0)
progress_changes$both_comps <-ifelse(progress_changes$in_comp=="incomp", progress_changes$score, progress_changes$pre_comp)


#Now we group by user_id and exercise_name.  
progress_changes <-aggregate(cbind(pre_comp,both_comps)~user_id+exercise_name, data=progress_changes, FUN=max)
progress_changes$score <-progress_changes$both_comps - progress_changes$pre_comp
#We now have one row for each user and exercise, tracking progress for that user on that exercise.

#merge curriculum info onto progress_changes
curriculum <-read.csv("curriculum.csv")
progress_changes <-merge(progress_changes, curriculum, by.x = "exercise_name", by.y = "exercise_name", all.x=TRUE)

#merge columns to indicate the students class (grade level) and school
student_class_list <- dbGetQuery(con, "SELECT * FROM student_class_list")	#16876 rows
class_sort <-student_class_list[order(-student_class_list$mathletes_class, na.last=FALSE) , ]	#16876 rows
class_list <- class_sort[!duplicated(class_sort$student_user_id), ] 	#13486 rows (duplicates removed)
#DATA CLEANING
class_list$class_year[class_list$class_name=="3rdmathletes" & class_list$mathletes_class==0] <-"3rd class (ROI)/Primary 5 (NI)"
class_list$class_year[class_list$class_name=="5th class mathletes" & class_list$mathletes_class==0] <-"5th class (ROI)/Primary 7 (NI)"
class_list$class_year[class_list$class_name=="fifthclassmathletes" & class_list$mathletes_class==0] <-"5th class (ROI)/Primary 7 (NI)"
class_list$class_year[class_list$class_name=="6th class mathletes" & class_list$mathletes_class==0] <-"6th class (ROI)/Year 8 (NI)"
class_list$class_year[class_list$class_name=="6th ClassMathletes" & class_list$mathletes_class==0] <-"6th class (ROI)/Year 8 (NI)"
class_list$class_year[class_list$class_name=="6thmathletes" & class_list$mathletes_class==0] <-"6th class (ROI)/Year 8 (NI)"
class_list$class_year[class_list$class_name=="istyearmathletes" & class_list$mathletes_class==0] <-"1st year (ROI)/Year 9 (NI)"
class_list$class_year[class_list$class_name=="1031styearmathletes" & class_list$mathletes_class==0] <-"1st year (ROI)/Year 9 (NI)"
class_list$class_year[class_list$class_name=="year9mathletes" & class_list$mathletes_class==0] <-"1st year (ROI)/Year 9 (NI)"
class_list$class_year[class_list$class_name=="2nd Higher Level Maths" & class_list$mathletes_class==0] <-"2nd year (ROI)/Year 10 (NI)"
class_list$class_year[class_list$class_name=="2ndmathletes" & class_list$mathletes_class==0] <-"2nd year (ROI)/Year 10 (NI)"
class_list$class_year[class_list$class_name=="year10mathletes" & class_list$mathletes_class==0] <-"2nd year (ROI)/Year 10 (NI)"
class_list$class_year[class_list$class_name=="ThirdyearMathletes" & class_list$mathletes_class==0] <-"3rd year (ROI)/Year 11 (NI)"
class_list$class_year[class_list$class_name=="TY1" & class_list$mathletes_class==0] <-"4th year/TY (ROI)/Year 12 (NI)"
class_list$class_year[class_list$class_name=="ty1mathletes" & class_list$mathletes_class==0] <-"4th year/TY (ROI)/Year 12 (NI)"
class_list$class_year[class_list$class_name=="TY4Amathletes" & class_list$mathletes_class==0] <-"4th year/TY (ROI)/Year 12 (NI)"
class_list$class_year[class_list$class_name=="TY34thyearmathletes" & class_list$mathletes_class==0] <-"4th year/TY (ROI)/Year 12 (NI)"
class_list$class_year[class_list$class_name=="6thyearmathletes" & class_list$mathletes_class==0] <-"5th & 6th year (ROI)/Year 14 (NI)"
class_list <-class_list[ , c(2,4)]	#keep only the student_user_id and class_year
progress2 <-merge(progress_changes, class_list, by.x="user_id", by.y="student_user_id", all.x=TRUE)

student_school_list <- dbGetQuery(con, "SELECT student_user_id, school_roll_number, school_county FROM student_school_list")
student_school_list <- dbGetQuery(con, "SELECT * FROM student_school_list")	#13490 rows
school_sort <-student_school_list[order(student_school_list$school_roll_number, student_school_list$school_county, na.last=TRUE) , ]	#13490 rows
school_list <- school_sort[!duplicated(school_sort$student_user_id), ] 	#13486 rows
school_list <-school_list[ , c(2,4,5,6)] #keep only student_user_id, school_roll_number, school_town, school_county

progress2 <-merge(progress2, school_list, by.x="user_id", by.y="student_user_id", all.x=TRUE)	#1,168,383 rows
write.csv(progress2, file="progress2.csv")

progress3 <-subset(progress2,progress2$mission_base_year!="nonmath") #1,167,669 rows
progress3 <-aggregate(score~exercise_name+class_year+mission_base_year+weight+subject, data=progress2, FUN=function(progress2) c(sum=sum(progress2),mean=mean(progress2),count=length(progress2) ) )

write.csv(progress3, file="progress_vs_curriculum.csv")
