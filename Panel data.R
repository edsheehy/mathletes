#Turn the flatfile into a dataframe suitable for use with panel data models
#The time period for each student will reflect the time since they entered the competition (rather than the calendar week)

flatfile2 <-read.csv("flatfile.csv")

#Calculate average mission level across 2015 competition
flatfile2$avg_mission <-flatfile2$points_week1*flatfile2$week1_mission +
				flatfile2$points_week2*flatfile2$week2_mission +
				flatfile2$points_week3*flatfile2$week3_mission +
				flatfile2$points_week4*flatfile2$week4_mission +
				flatfile2$points_week5*flatfile2$week5_mission +
				flatfile2$points_week6*flatfile2$week6_mission +
				flatfile2$points_week7*flatfile2$week7_mission +
				flatfile2$points_week8*flatfile2$week8_mission +
				flatfile2$points_week9*flatfile2$week9_mission +
				flatfile2$points_week10*flatfile2$week10_mission +
				flatfile2$points_week11*flatfile2$week11_mission +
				flatfile2$points_week12*flatfile2$week12_mission
flatfile2$avg_mission <- flatfile2$avg_mission/(flatfile2$points_week1+flatfile2$points_week2+
					flatfile2$points_week3+flatfile2$points_week4+flatfile2$points_week5+
					flatfile2$points_week6+flatfile2$points_week7+flatfile2$points_week8 +
					flatfile2$points_week9+flatfile2$points_week10+flatfile2$points_week11+flatfile2$points_week12)


finals <-read.csv("FinalsResults.csv")
flatfile2<-merge(flatfile2, finals, by= "user_id",  all.x=TRUE)
prepfile <-flatfile2

# Calculate start week for each student
prepfile$start_week <-ifelse(prepfile$points_week1>0,1,ifelse(prepfile$points_week2>0,2,ifelse(prepfile$points_week3>0,3,ifelse(prepfile$points_week4>0,4,
				ifelse(prepfile$points_week5>0,5,ifelse(prepfile$points_week6>0,6,ifelse(prepfile$points_week7>0,7,ifelse(prepfile$points_week8>0,8,
				ifelse(prepfile$points_week9>0,9,ifelse(prepfile$points_week10>0,10,ifelse(prepfile$points_week11>0,11,12)))))))))))

#Would like to know school's cumulative points at end of previous week (total and by class)
school_points <-prepfile[,c("points_week0","points_week1","points_week2","points_week3","points_week4","points_week5",            
					"points_week6","points_week7","points_week8","points_week9","points_week10","points_week11","points_week12","school_roll_number")]

colnames(school_points) <- c("school_w0", "school_w1", "school_w2","school_w3","school_w4","school_w5","school_w6",
					"school_w7", "school_w8", "school_w9","school_w10","school_w11","school_w12","school_roll_number")
school_points <-aggregate(cbind(school_w0, school_w1, school_w2, school_w3, school_w4, school_w5, school_w6, 
	school_w7, school_w8, school_w9, school_w10, school_w11, school_w12)~ school_roll_number, data=school_points, FUN=sum)
#Calculate cumulative points at end of previous week
school_points$schoolendweek1 <- school_points$school_w1
school_points$schoolendweek2 <- school_points$schoolendweek1 + school_points$school_w2
school_points$schoolendweek3 <- school_points$schoolendweek2 + school_points$school_w3
school_points$schoolendweek4 <- school_points$schoolendweek3 + school_points$school_w4
school_points$schoolendweek5 <- school_points$schoolendweek4 + school_points$school_w5
school_points$schoolendweek6 <- school_points$schoolendweek5 + school_points$school_w6
school_points$schoolendweek7 <- school_points$schoolendweek6 + school_points$school_w7
school_points$schoolendweek8 <- school_points$schoolendweek7 + school_points$school_w8
school_points$schoolendweek9 <- school_points$schoolendweek8 + school_points$school_w9
school_points$schoolendweek10 <- school_points$schoolendweek9 + school_points$school_w10
school_points$schoolendweek11 <- school_points$schoolendweek10 + school_points$school_w11
school_points$schoolendweek12 <- school_points$schoolendweek11 + school_points$school_w12
#Drop incremental columns
school_points <-school_points[,c("school_roll_number","schoolendweek1","schoolendweek2","schoolendweek3","schoolendweek4","schoolendweek5",
"schoolendweek6","schoolendweek7","schoolendweek8","schoolendweek9","schoolendweek10","schoolendweek11","schoolendweek12")]

school_class_points <-prepfile[,c("points_week0","points_week1","points_week2","points_week3","points_week4","points_week5",            
					"points_week6","points_week7","points_week8","points_week9","points_week10","points_week11","points_week12","class_year","school_roll_number")]
colnames(school_class_points) <- c("class_w0", "class_w1", "class_w2","class_w3","class_w4","class_w5","class_w6",
					"class_w7", "class_w8", "class_w9","class_w10","class_w11","class_w12","class_year","school_roll_number")
school_class_points <-aggregate(cbind(class_w0, class_w1, class_w2, class_w3, class_w4, class_w5, class_w6, 
	class_w7, class_w8, class_w9, class_w10, class_w11, class_w12)~ class_year+school_roll_number, data=school_class_points, FUN=sum)
#Calculate cumulative points at end of previous week
school_class_points$classendweek1 <- school_class_points$class_w1
school_class_points$classendweek2 <- school_class_points$classendweek1 + school_class_points$class_w2
school_class_points$classendweek3 <- school_class_points$classendweek2 + school_class_points$class_w3
school_class_points$classendweek4 <- school_class_points$classendweek3 + school_class_points$class_w4
school_class_points$classendweek5 <- school_class_points$classendweek4 + school_class_points$class_w5
school_class_points$classendweek6 <- school_class_points$classendweek5 + school_class_points$class_w6
school_class_points$classendweek7 <- school_class_points$classendweek6 + school_class_points$class_w7
school_class_points$classendweek8 <- school_class_points$classendweek7 + school_class_points$class_w8
school_class_points$classendweek9 <- school_class_points$classendweek8 + school_class_points$class_w9
school_class_points$classendweek10 <- school_class_points$classendweek9 + school_class_points$class_w10
school_class_points$classendweek11 <- school_class_points$classendweek10 + school_class_points$class_w11
school_class_points$classendweek12 <- school_class_points$classendweek11 + school_class_points$class_w12
#Drop incremental columns
school_class_points <-school_class_points[,c("school_roll_number","class_year","classendweek1","classendweek2","classendweek3","classendweek4","classendweek5",
"classendweek6","classendweek7","classendweek8","classendweek9","classendweek10","classendweek11","classendweek12")]

#Merge this info onto flatfile
prepfile<-merge(prepfile, school_points, by= "school_roll_number",  all.x=TRUE)
prepfile<-merge(prepfile, school_class_points, by= c("school_roll_number","class_year"),  all.x=TRUE)

#Now shift all columns left by the start_week so that first column is the student's first week in the competition (even if they joined late)
#Shift the school and school_class points by one extra column so that we get the school's progress at the end of the previous week

prepfile$points_1<-0
prepfile$points_2<-0
prepfile$points_3<-0
prepfile$points_4<-0
prepfile$points_5<-0
prepfile$points_6<-0
prepfile$points_7<-0
prepfile$points_8<-0
prepfile$points_9<-0
prepfile$points_10<-0
prepfile$points_11<-0
base_col <-which(colnames(prepfile)=="points_week0")
shift_col <-which(colnames(prepfile)=="points_1")
for (i in 1:nrow(prepfile)){
	start_week <-prepfile[i,"start_week"]	#the week the student entered the competition.
	for (j in 1:11){			#11 columns for the shifted points
		prepfile[i,shift_col+j-1] <- ifelse(j+start_week>13,0,prepfile[i,base_col+start_week+j-1])
	}
}

prepfile$mission_1<-0
prepfile$mission_2<-0
prepfile$mission_3<-0
prepfile$mission_4<-0
prepfile$mission_5<-0
prepfile$mission_6<-0
prepfile$mission_7<-0
prepfile$mission_8<-0
prepfile$mission_9<-0
prepfile$mission_10<-0
prepfile$mission_11<-0
base_col <-which(colnames(prepfile)=="week0_mission")
shift_col <-which(colnames(prepfile)=="mission_1")
for (i in 1:nrow(prepfile)){
	start_week <-prepfile[i,"start_week"]	#the week the student entered the competition.	
	for (j in 1:11){					#11 columns for the shifted points
		prepfile[i,shift_col+j-1] <- ifelse(j+start_week>13,0,prepfile[i,base_col+start_week+j-1])
	}
}

prepfile$school_1<-0
prepfile$school_2<-0
prepfile$school_3<-0
prepfile$school_4<-0
prepfile$school_5<-0
prepfile$school_6<-0
prepfile$school_7<-0
prepfile$school_8<-0
prepfile$school_9<-0
prepfile$school_10<-0
prepfile$school_11<-0
base_col <-which(colnames(prepfile)=="schoolendweek1")-2
shift_col <-which(colnames(prepfile)=="school_1")
for (i in 1:nrow(prepfile)){
	start_week <-prepfile[i,"start_week"]	#the week the student entered the competition.
	for (j in 1:11){			#11 columns for the shifted points
		prepfile[i,shift_col+j-1] <- ifelse(j+start_week>14,0,prepfile[i,base_col+start_week+j-1])
	}
}

prepfile$class_1<-0
prepfile$class_2<-0
prepfile$class_3<-0
prepfile$class_4<-0
prepfile$class_5<-0
prepfile$class_6<-0
prepfile$class_7<-0
prepfile$class_8<-0
prepfile$class_9<-0
prepfile$class_10<-0
prepfile$class_11<-0
base_col <-which(colnames(prepfile)=="classendweek1")-2
shift_col <-which(colnames(prepfile)=="class_1")
for (i in 1:nrow(prepfile)){
	start_week <-prepfile[i,"start_week"]	#the week the student entered the competition.
	for (j in 1:11){			#11 columns for the shifted points
		prepfile[i,shift_col+j-1] <- ifelse(j+start_week>14,0,prepfile[i,base_col+start_week+j-1])
	}
}


#Drop non-shifted columns
prepfile2 <-prepfile[,c("user_id","points_1","points_2","points_3","points_4","points_5","points_6","points_7","points_8","points_9",                
 				"points_10","points_11","mission_1","mission_2","mission_3","mission_4","mission_5","mission_6",               
 				"mission_7","mission_8","mission_9","mission_10","mission_11","school_1","school_2","school_3",               
 				"school_4","school_5","school_6","school_7","school_8","school_9","school_10","school_11",               
				"class_1","class_2","class_3","class_4","class_5","class_6","class_7","class_8","class_9","class_10","class_11",
				"school_roll_number","class_year","school_town","school_county",           
 				"points_week0","week0_mission","points_in_comp","avg_mission","champions_league","bonus_points","total_points","total_hours",
	                  "avg_problem_level","hustle","provincial_result","provincial_rank_by_class","national_result","start_week")]     

#Rename points_week0 to points_earned_pre_comp
names(prepfile2)[names(prepfile2) == 'points_week0'] <- 'points_earned_pre_comp'
names(prepfile2)[names(prepfile2) == 'week0_mission'] <- 'mission_pre_comp'


#Reshape to long (rather than wide) format
prepfile3 <-reshape(prepfile2, direction="long", sep='_', varying=list(
	c("points_1","points_2","points_3","points_4","points_5","points_6","points_7","points_8","points_9","points_10","points_11"),
	c("mission_1","mission_2","mission_3","mission_4","mission_5","mission_6","mission_7","mission_8","mission_9","mission_10","mission_11"),
	c("school_1","school_2","school_3","school_4","school_5","school_6","school_7","school_8","school_9","school_10","school_11"),
	c("class_1","class_2","class_3","class_4","class_5","class_6","class_7","class_8","class_9","class_10","class_11")),
	v.names=c("weekly_points","weekly_mission_level","school_points_to_last_week","class_points_to_last_week"),times=c(1:11))

#Rearrange columns (id and time need to be on left)
prepfile3 <-prepfile3[,c("user_id","time","weekly_points","weekly_mission_level","school_points_to_last_week","class_points_to_last_week","start_week",
				"points_earned_pre_comp","mission_pre_comp","points_in_comp","avg_mission","champions_league","bonus_points","total_points","total_hours","avg_problem_level","hustle",
				"provincial_result","provincial_rank_by_class","national_result",
				"school_roll_number","class_year","school_town","school_county")]

#Sort by user_id, then time
prepfile3 <- prepfile3[order(prepfile3$user_id,prepfile3$time),]  #136,884 rows
write.csv(prepfile3,file="prepfile3.csv")

#Delete rows beyond end of competition (e.g. for competitor who started in week 10, we want to only keep 3 records as the competition ended in week 12.
paneldata <- subset(prepfile3,prepfile3$time + prepfile3$start_week<=13) #107,691 rows
write.csv(paneldata,file="paneldata.csv")
