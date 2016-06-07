# data cleaning

library(dplyr)
library(tidyr)

# read data and names
data <- read.csv("surveydata.csv", na = c("#NULL!", ""))
varlabels <- read.csv("varlabels.csv", header = FALSE)
var <- varlabels %>%
  select(V1, V3)

# save main columns
write.csv(var, file = "var_raw.csv")
View(var)


# multi-column questions

# Q7
rename("Bus" = Q7_1,
       "Carpool" = Q7_2,
       "Subway" = Q7_3,
       "Train" = Q7_4,
       "Taxi" = Q7_5,
       "Airplane" = Q7_6,
       "Other" = Q7_7,
       "Refused" = Q7_8,
       "Other_text" = Q7_otherText)
# Q8
rename("Work" = Q8_1,
       "School" = Q8_2,
       "Shopping" = Q8_3,
       "Visiting people" = Q8_4,
       "Recreation" = Q8_5,
       "Other" = Q8_6,
       "Refused"= Q8_7,
       "Other_text" = Q8_otherText)
# Q10
rename("Bus" = Q10_1,
       "Carpool" = Q10_2,
       "Subway" = Q10_3,
       "Train" = Q10_4,
       "Taxi" = Q10_5,
       "Airplane" = Q10_6,
       "Don_t know" = Q10_7,
       "Other" = Q10_8,
       "Refused" = Q10_9,
       "Other_text" = Q10_otherText)

# Q11
rename("Work" = Q11_1,
       "Schools" = Q11_2,
       "Day care" = Q11_3,
       "Stores" = Q11_4,
       "Restaurants" = Q11_5,
       "Libraries" = Q11_6,
       "Hospitals" = Q11_7,
       "Doctor_s office" = Q11_8,
       "Public transportation" = Q11_9,
       "Family/friends" = Q11_10,
       "Other" = Q11_11,
       "Other_text", =	Q11_OtherText,
       "Codes" = Q11_OtherText_Codes)


# Q12

"Avoid touching my eyes" = Q12_1
"Avoid touching my nose" = Q12_2
"Avoid touching my mouth" =	Q12_3
"Wash my hands with soap more often"= Q12_4
"Use hand sanitizers"	= Q12_5
"Clean the surfaces in my home" =	Q12_6
"Clean the surfaces at work" = Q12_7
"Eat nutritious food" = Q12_8
"Get adequate rest" = Q12_9
"Get recommended vaccine" =	Q12_10
"Take preventive medicine" = Q12_11
"Cover my nose and mouth with a surgical mask" = Q12_12
"Avoid contact with people who are sick" = Q12_13
"Avoid crowded places" = Q12_14
"Other" = Q12_15
"Other_text" = Q12_OtherText
"Codes" = Q12_OtherText_Codes


# Q18
"The vaccine costs too much" = Q18_1
"The vaccine is not very effective in preventing influenza" = Q18_2
"I am not likely to get influenza" = Q18_3
D"o not know where to get vaccine" = Q18_4
"The side effect of the vaccine are too risky" = Q18_5
"I am allergic to some of the ingredients in the vaccine" = Q18_6
"I do not like shots" = Q18_7,
"I just don_t get around to doing it" = Q18_8,
"I have to travel too far to get vaccine" = Q18_9
"Other" = Q18_10
"Refused" = Q18_11
"Other_text" = Q18_otherText


# Q22
"Go to a doctor_s office or medical clinic" = Q22_1,
"Decide on treatment without consulting a health practitioner" = Q22_2,
"Search the internet for a treatment" = Q22_3
"Get adequate sleep" = Q22_4
"Eat nutritious food" = Q22_5
"Take-over-counter medication for symptoms" = Q22_6
"Take an antiviral medicine" = Q22_7,
"Take no action to treat the illness" = Q22_8
"Other" = Q22_9
"Other_text" = Q22_OtherText
"Codes" = Q22_OtherText_Codes


# Q23
"Stand away from people" = Q23_1
"Avoid public places" = Q23_2
"Avoid public transportation" = Q23_3
"Stay at home" = Q23_4
"Wash my hands with soap more often" = Q23_5
"Use hand sanitizers" = Q23_6
"Clean the surfaces in my home" = Q23_7
"Clean the surfaces I use at work" = Q23_8
"Cover my nose and mouth with a surgical mask" = Q23_9
"Cover my nose and mouth when I sneeze or cough" = Q23_10
"Other" = Q23_11
"Other_text" = Q23_OtherText
"Codes" = Q23_OtherText_Codes


# Q24
"Print media such as newspapers and magazines" = Q24_1
"Traditional media such as television and radio" = Q24_2
"Social media such as internet and blogs" = Q24_3
"Word of mouth" = Q24_4
"None" = Q24_5
"Other" = Q24_6
"Refused" = Q24_7
"Other_text" = Q24_otherText


# Q25
"Stand away from people" = Q25_1
"Avoid public places" = Q25_2
"Avoid public transportation" = Q25_3
"Stay at home" = Q25_4
"Wash my hands with soap more often" = Q25_5
"Use hand sanitizers" = Q25_6,
"Clean the surfaces in my home" = Q25_7
"Clean the surfaces I use at work" = Q25_8
"Cover my nose and mouth with a surgical mask" = Q25_9
"Cover my nose and mouth when I sneeze or cough" = Q25_10
"Other" = Q25_11
"Other_text" = Q25_OtherText
"Codes" = Q25_OtherText_Codes


# Q27
"Keep the child away from the others in the residence	Q27_1
"Keep the child out of school/daycare	Q27_2
""Stop child_s social activities like play dates	Q27_3
"Other	Q27_4
"Other_text	Q27_OtherText
"Codes	Q27_OtherText_Codes


# Q29
"A parent brings the child to work	Q29_1
""""A parent stays home	Q29_2
"Another adult stays home	Q29_3
"Send the child to school sick	Q29_4
"Take the child to a relative or friends	Q29_5
"Other	Q29_6
"Other_text	Q29_OtherText
"Codes	Q29_OtherText_Codes


# Q30
"I bring the child to work	Q30_1
"I stay home	Q30_2
"Another adult stays home	Q30_3
"Send the child to school sick	Q30_4
"Take the child to a relative or friends	Q30_5
"Other	Q30_6
"Other_text	Q30_OtherText
"Codes"	Q30_OtherText_Codes








