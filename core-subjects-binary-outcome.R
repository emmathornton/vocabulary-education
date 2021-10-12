#load necessary packages####
library(haven)
require(swfscMisc)
require(sjmisc)
require(Hmisc)
require(psych)
library(plyr)
library(dplyr) 
library(tidyr) 
library(tidyverse)
library(ggplot2)
library(stats)

#binary variable of who took core subjects at gcse
#core subjects = maths, english (lang and/or lit), science
#for science, any form of science as due to self reports these may be reported differently 

#load in data 
mcs7_qualifications = read_sav("mcs7_cm_qualifications.sav")
mcs7_hh_grid = read_sav("mcs7_hhgrid.sav")
#names to lower case
names(mcs7_hh_grid) <- tolower(names(mcs7_hh_grid))
names(mcs7_qualifications) <- tolower(names(mcs7_qualifications))

country = mcs7_hh_grid %>% filter(gcnum00 == 1)
country = country %>%  select(mcsid, gactry00)
names(country) <- c("mcsid", "country")

#single cm per family 
qualifications1 = mcs7_qualifications %>% filter(gcnum00 == 1)

#convert gcse subject names into text names - key for this which will do later. 
gcse_subject_key <-c(`2`	= "Additional Applied Science",
                     `3`	 =" Additional Mathematics",
                     `4`	 = "Additional Science",
                     `261`	 = "Recoded due to low counts - check SA",
                     `7`	 = "Anthropology",
                     `9`	 = "Applied Art and Design",
                     `10`	 = "Applied Business",
                     `15`	 = "Applied ICT ",
                     `18`	 = "Applied Physical Education",
                     `19`	 = "Applied Science",
                     #`-8`	 = "Do not know",
                     `24` = "Art and Design (Art, Craft and Design / Fine Art / Graphic Communication / Photography / Textile Design / Three-Dimensio",
                     `25`	 = "Art and Design (short course)",
                     `30`	 = "Language: Biblical Hebrew",
                     `31`	 = "Biology",
                     `32`	 = "Biology (Human)",
                     `36`	 = "Business",
                     `37`	 = "Business and Communication Systems",
                     `39`	 = "Business Studies",
                     `42`	 = "Catering",
                     `43`	 = "Chemistry",
                     `47`	 = "Children s Learning and Development",
                     `49`	 = "Language: Chinese: Spoken Language ",
                     `51`	 = "Citizenship Studies",
                     `53`	 = "Civilisation",
                     `54`	 = "Classical Civilisation",
                     `57`	 = "Combined Science ",
                     `62`	 = "Communication and Culture",
                     `63`	 = "Computer Science",
                     `64`	 = "Computing",
                     `65`	 = "Construction and the Built Environment",
                     `68`	 = "Language: Cymraeg / Welsh",
                     `69`	 = "Language: Cymraeg Ail Iaith / Welsh Second Language",
                     `70`	 = "Language: Cymraeg Ail Iaith (Short Course) / Welsh Second Language",
                     `73`	 = "Dance",
                     `74`	 = "Design and Technology",
                     `76`	 = "Design and Technology: Graphic Products",
                     `78`	 = "Design and Technology: Textiles Technology",
                     `81`	 = "Drama",
                     `83`	 = "Language: Dutch: spoken language" ,
                     `84`	 = "Language: Dutch: written language ",
                     `85`	 = "Economics",
                     `88`	 = "Engineering",
                     `89`	 = "Language: English",
                     `92`	 = "Language: English - Spoken English Studies (Short Course) ",
                     `95`	 = "Language: English Language",
                     `96`	 = "Language: English Literature",
                     `101`	 = "Film Studies",
                     `102`	 = "Financial Services",
                     `259` = "Other",
                     `106`	 = "Language: French",
                     `110`	 = "Further Additional Science",
                     `111`	 = "Further Mathematics",
                     `113`	 = "Language: Gaidhlig (Scottish Gaelic)",
                     `115`	 = "Geography",
                     `122`	 = "Language: German",
                     `125`	 = "Language: Government and Politics",
                     `128`	 = "Language: Greek: Written Language ",
                     `130`	 = "Language: Gujarati: spoken language" ,
                     `131`	 = "Language: Gujarati: written language ",
                     `132`	 = "Health and Social Care",
                     `133`	 = "Language: Hebrew (Modern)" ,
                     `134`	 = "History",
                     `136`	 = "Home Economics",
                     `137` =" Home Economics Child Development",
                     `139`	 = "Home Economics: Textiles",
                     `141`	 =" Hospitality and Catering",
                     `146`	 = "Information and Communication Technology (ICT, IT)",
                     `150`	 = "Language: Irish",
                     `151`	 = "Language: Irish (short course)",
                     `152`	 = "Language: Irish; spoken language (Short Course)",
                     `155`	 = "Language: Italian: Spoken Language ",
                     `156	` = "Language: Italian: Written Language" ,
                     `158`	 = "Language: Japanese: Spoken Language ",
                     `159`	 =" Language: Japanese: Written Language ",
                     `163`	 = "Language: Latin",
                     `165`	 = "Learning for Life and Work",
                     `170`	 = "Mathematics",
                     `171` = "Mathematics - Linear",
                     `172`	 = "Mathematics - Numeracy",
                     `174`	 = "Media Studies",
                     `176`	 = "Language: Modern Greek",
                     `178`	 = "Language: Modern Hebrew: Spoken Language" ,
                     `179` = "Language: Modern Hebrew: Written Language " ,
                     `182`	 = "Music",
                     `185`	 = "Language: Panjabi: Spoken Language  ",
                     `186`	 = "Language: Panjabi: Written Language"  ,
                     `187`	 = "Performing Arts",
                     `190`	 = "Language: Persian: spoken language ",
                     `191	` = "Language: Persian: written language ",
                     `194`	 = "Philosophy",
                     `195`	 = "Physical Education (Games)",
                     `197`	 = "Physical Education (PE)",
                     `199`	 = "Physics",
                     #`-1`	 = "Not applicable",
                     `204	` = "Language: Polish: Spoken Language" , 
                     `205`	 = "Language: Polish: Written Language ",
                     `206`	 = "Politics",
                     `208`	 = "Language: Portuguese: spoken language" ,
                     `209`	 = "Language: Portuguese: written language ",
                     `211`	 = "Psychology",
                     `214`	 = "Religious Studies",
                     `215`	 = "Religious Studies (Short Course) ",
                     `216`	 = "Rural and Agricultural Science",
                     `219`	 = "Language: Russian: Written Language ",
                     `220`	 = "Science",
                     `222`	 = "Science (Modular)",
                     `224`	 = "Sociology",
                     `226`	 = "Language: Spanish",
                     `229`	 = "Statistics",
                     `233`	 = "Language: Turkish: Spoken Language" ,
                     `234	` = "Language: Turkish: Written Language ",
                     `235`	 = "Language: Urdu",
                     `239`	 = "Language: Welsh First Language",
                     `240`	 = "Welsh Literature",
                     `241`	 = "Work and Life Skills",
                     `245` = "Design and Technology: Product Design",
                     #`-9`	 = "Refusal",
                     `248`	 = "Design and Technology (Resistant Materials Technology)",
                     `249`	 = "Additional Science (Modular)",
                     `252`	 = "Art and Design",
                     `254`	 = "Design and Technology: Food technology")

#those who said yes to having gcse qualification
gcse = qualifications1 %>% filter(gc_s_qual_gcse == 1) # be careful here as for rest of rows per cm, this will be NA.
gcse = qualifications1[qualifications1$mcsid %in% gcse$mcsid,] #those who have yes to gcse, get rest of long data 
gcse = gcse %>% select(mcsid, gcnum00, gc_rowid, gc_s_qual_gcsn_r20, 
                       gc_l_gcsb_name_r40, gc_l_gcdb, gc_l_gcsb_code_r40, gc_l_gcgd) #get relevant variables 

names(gcse) <- c("mcsid", "cm_number", "row_id", "total_quals", "subject_name","double award", "subject_code", "subject_grade") #rename variables
gcse$subject_name = as.factor(gcse$subject_name)#convert subject_name into names
gcse$subject_name =  recode(gcse$subject_name, !!!gcse_subject_key)#convert subject_name into names
gcse = gcse %>%  filter(!is.na(subject_name)) #remove the NA subject name as due to long format, each cm has 20 rows and alot will be NA
gcse$subject_grade =  as.numeric(gcse$subject_grade) #recode grade so all on a 1-9 scale
gcse$subject_grade  = recode(gcse$subject_grade, `10` = 0, `11` = 8.5, `12` = 7, `13` =5.5, 
                             `14` = 4, `15` = 3, `16` = 2, `17` = 1.5, `18` = 1)

#iGCSEs #### 
igcse_subject_key <- c(`1`	 = "Accounting",
                       `2`	 = "Language: Arabic as first language",
                       `4`	 = "Language: Bangla",
                       `5`	 = "Bangladesh Studies",
                       `6`	 = "Language: Bengali",
                       `7`	 = "Biology",
                       `9`	 = "Chemistry",
                       `11`	 = "Language: Classical Arabic",
                       `12`	 = "Commerce",
                       `15`	 = "Language: English - Second language",
                       `16`	 = "Language: English - First Language",
                       `18`	 = "Language: English Language B",
                       `19`	 = "Language: English Literature",
                       `20`	 = "Language: French",
                       `22`	 = "Geography",
                       `23`	 = "Language: German",
                       `25`	 = "Language: Greek as first language",
                       `26`	 = "Language: Gujarati",
                       `27`	 = "Language: Hindi",
                       `28`	 = "History",
                       `31`	 = "Islamic Studies",
                       `32`	 = "Islamiyat",
                       `33`	 = "Mathematics",
                       `37`	 = "Pakistan Studies",
                       `38`	 = "Physics",
                       `39`	 = "Religious Studies",
                       `40`	 = "Science", 
                       `41`	 = "Language: Sinhala",
                       `42`	 = "Language: Spanish",
                       `43`	 = "Language: Swahili",
                       `44`	 = "Language: Tamil",
                       `46`	 = "Language: Urdu",
                       `47`	 = "Language: Afrikaans First Language",
                       `48`	 = "Language: Afrikaans Second Language",
                       `49`	 = "Agriculture",
                       `50`	 = "Language: Arabic - Foreign Language",
                       `51`	 = "Language: Bahasa Indonesia",
                       `52`	 = "Child Development",
                       `53`	 = "Language: Czech",
                       `55`	 = "Development Studies",
                       `57`	 = "Language: Dutch - First Language",
                       `58`	 = "Language: Dutch - Second Language",
                       `59`	 = "Enterprise",
                       `64`	 = "India Studies",
                       `65`	 = "Language: Indonesian - Foreign Language",
                       `66`	 = "Language: IsiZulu as a Second Language",
                       `71`	 = "Language: Kazakh",
                       `72`	 = "Language: Korean",
                       `74`	 = "Language: Malay - Foreign Language",
                       `76`	 = "Mathematics - International",
                       `80`	 = "Language: Portuguese - First Language",
                       `81`	 = "Language: Portuguese - Second Language",
                       `83`	 = "Language: Sanskrit",
                       `87`	 = "Language: Spanish - Literature",
                       `89`	 = "Language: Thai - First Language",
                       `90`	 = "Travel and Tourism",
                       `91`	 = "World Literature",
                       `94`	 = "Recoded due to low counts - check SA")
# `-9`	 = "Refusal
# `-8`	 = "Do not know
# `-1`	 = "Not applicable

Igcse = qualifications1 %>% filter(gc_s_qual_igcs == 1) # be careful here as for rest of rows per cm, this will be NA.
Igcse = qualifications1[qualifications1$mcsid %in% Igcse$mcsid,] 
Igcse = Igcse %>% select(mcsid, gcnum00, gc_rowid, gc_s_qual_igsn_rec,
                         gc_l_igsb_name_r30, gc_l_iggd)


names(Igcse) <- c("mcsid", "cm_number", "row_id", "total_quals", "subject_name", "subject_grade")
Igcse$subject_name = as.factor(Igcse$subject_name)
Igcse$subject_name =  recode(Igcse$subject_name, !!!igcse_subject_key)
Igcse = Igcse %>%  filter(!is.na(subject_name)) #remove the NA subject name as due to long format, each cm has 20 rows and alot will be NA

#these are different to GCSE grades!! so the gcse grade recoding will be different here - updated. 
# recode so that 1= 1, 2=2, 3=3, 4=4, 5=5, 6=6, 7=7, 8=8, 9=9. at the moment in the data, 1= grade 9 etc. 
#so when recode, 1=9, 2=8 etc 
Igcse$subject_grade =  as.numeric(Igcse$subject_grade)
Igcse$subject_grade  = recode(Igcse$subject_grade, `1` = 9, `2` = 8, `3` = 7, `4` = 6, `6`= 4, `7` = 3, `8` = 2, `9` = 1,
                              `10` = 8.5, `11` = 7, `12` = 5.5, `14` =4, 
                              `14` = 4, `15` = 3, `16` = 2, `17` = 1.5, `19` = 0)


#create binary variable for if have subject vs not ####
#1 = yes, 0= no
#will need to recode this later - want 0= yes have subjects, 1 = no don't have subject
#GCSE
#use xtabs to create a dataframe that has 0 or 1 for each cohort member having each subject
gcse_subject = as.data.frame(xtabs(~gcse$mcsid + gcse$subject_name))
#have_english <- gcse_subject %>% select(mcsid, Language: English, Language: English Language, Language: English Literature)

#convert this dataframe to wide so each column is a subject, entries will be 0 or 1 based on whether cm has that subject
wide_gcse_subject = pivot_wider(gcse_subject, id_cols=gcse.mcsid, names_from = gcse.subject_name,
                                values_from = Freq)
#iGCSE 
#use xtabs to create a dataframe that has 0 or 1 for each cohort member having each subject
Igcse_subject = as.data.frame(xtabs(~Igcse$mcsid + Igcse$subject_name))
#have_english <- gcse_subject %>% select(mcsid, Language: English, Language: English Language, Language: English Literature)

#convert this dataframe to wide so each column is a subject, entries will be 0 or 1 based on whether cm has that subject
wide_Igcse_subject = pivot_wider(Igcse_subject, id_cols=Igcse.mcsid, names_from = Igcse.subject_name,
                                 values_from = Freq)

#first, get those who have english subjects - select english subjects from wide_gcse_subject ####
have_english_gcse <- wide_gcse_subject %>% select(gcse.mcsid, c("Language: English", "Language: English Language", "Language: English Literature"))
names(have_english_gcse) <- c("mcsid", "english", "english_lang", "english_lit") #rename columns

#add in Igcse version of english subjects 
have_english_Igcse <- wide_Igcse_subject %>% select(Igcse.mcsid, contains("english"))
names(have_english_Igcse) <- c("mcsid", "english_first_language_I", "english_lit_I") #rename columns

#merge english gcse and english Igcse
have_english = merge(all=TRUE, have_english_gcse, have_english_Igcse, by="mcsid")

#add a new column of 0/1, will be 1 if a CM has any of these subjects
#check when meet if either lang or lit is fine or want both. if so, replace , in case_when with &
have_english <- have_english %>% mutate(english_gcse = case_when(english == 1 ~ 1, 
                                                                 english_lang == 1 ~ 1, 
                                                                 english_lit == 1 ~ 1, 
                                                                 english_first_language_I == 1 ~ 1, 
                                                                 english_lit_I == 1 ~ 1, 
                                                                 english == 2 ~ 1, 
                                                                 english_lang == 2 ~ 1, 
                                                                 english_lit == 2 ~ 1, 
                                                                 english_first_language_I == 2 ~ 1, 
                                                                 english_lit_I == 2 ~ 1, 
                                                                 TRUE  ~ 0 )) #check this bit!
#is.na(have_english) ~ 0))
#cm who havent satisifed conditions in case_when will be NA, convert these to 0 for no dont have subject 
#have_english$english_gcse[is.na(have_english$english_gcse)] <- 0
#pull out just mcsid, and final binary variable for if have english subjects
english_gcse <- have_english %>% select(mcsid, english_gcse)

#maths_gcse - repeat process above but this time for maths subjects ####
#first, get those who have maths subjects - select maths subjects from wide_gcse_subject
have_maths_gcse <- wide_gcse_subject %>% select(gcse.mcsid, c("Mathematics", "Mathematics - Linear", "Mathematics - Numeracy", "Further Mathematics"), contains("math"))
names(have_maths_gcse) <- c("mcsid", "maths", "maths_linear", "maths_numeracy", "further_maths", "additional_maths") # rename columns 
#add igcse maths 
have_maths_Igcse <- wide_Igcse_subject %>% select(Igcse.mcsid, contains("math"))
names(have_maths_Igcse) <- c("mcsid", "maths_I") #rename columns
have_maths = merge(all=TRUE, have_maths_gcse, have_maths_Igcse, by="mcsid")

#add a new column of 0/1, will be 1 if a CM has any of these subjects
have_maths <- have_maths %>% mutate(maths_gcse = case_when(maths == 1 ~ 1, 
                                                           maths_linear == 1 ~ 1, 
                                                           maths_numeracy == 1 ~ 1, 
                                                           further_maths == 1 ~ 1,
                                                           maths_I == 1 ~1, 
                                                           maths == 2 ~ 1, 
                                                           maths_linear == 2 ~ 1, 
                                                           maths_numeracy == 2 ~ 1, 
                                                           further_maths == 2 ~ 1,
                                                           maths_I == 2 ~1, 
                                                           additional_maths == 1 ~ 1, 
                                                           additional_maths==2 ~1, 
                                                           additional_maths == 3 ~1, 
                                                           TRUE ~ 0))
#is.na(have_maths) ~ 0))
#cm who havent satisifed conditions in case_when will be NA, convert these to 0 for no dont have subject 
#have_maths$maths_gcse[is.na(have_maths$maths_gcse)] <- 0
#pull out just mcsid, and final binary variable for if have mathssubjects
maths_gcse <- have_maths %>% select(mcsid, maths_gcse)

#have science? (NB if CM reports any form of science then this counts) ####
#first, get those who have science subjects - select science subjects from wide_gcse_subject - including computer science
have_science_gcse <- wide_gcse_subject %>% select(gcse.mcsid, contains("science"), contains("biology"), Physics, Chemistry)
names(have_science_gcse) <- c("mcsid", "additional_applied_science", "additional_science", "applied_science", "combined_science", "computer_science",
                         "further_additional_science", "science", "science_modular", "additional_science_modular", 
                         "biology", "human_biology", "physics", "chemistry") #rename columns

#add in Igcse version of science 
have_science_Igcse <- wide_Igcse_subject %>% select(Igcse.mcsid, Science, Biology, Chemistry, Physics)
names(have_science_Igcse) <- c("mcsid", "science_I", "biology_I", "chemistry_I", "physics_I") #rename columns

have_science = merge(all=TRUE, have_science_gcse, have_science_Igcse, by="mcsid")

#add a new column of 0/1, will be 1 if a CM has any of these subjects

have_science <- have_science %>% mutate(science_gcse = case_when(additional_applied_science == 1 ~ 1, 
                                                                 additional_science == 1 ~ 1, 
                                                                 applied_science == 1 ~ 1, 
                                                                 combined_science == 1 ~ 1, 
                                                                 computer_science == 1 ~ 1, 
                                                                 further_additional_science == 1 ~ 1,
                                                                 science == 1 ~ 1, 
                                                                 science_modular == 1 ~ 1,
                                                                 additional_science_modular == 1 ~ 1,
                                                                 biology == 1 ~ 1, 
                                                                 human_biology == 1 ~ 1,
                                                                 physics == 1 ~ 1, 
                                                                 chemistry == 1 ~ 1,
                                                                 science_I == 1 ~ 1, 
                                                                 biology_I == 1 ~ 1, 
                                                                 chemistry_I == 1 ~ 1, 
                                                                 physics_I == 1 ~ 1, 
                                                                 additional_applied_science == 2 ~ 1, 
                                                                 additional_science == 2 ~ 1, 
                                                                 applied_science == 2 ~ 1, 
                                                                 combined_science == 2 ~ 1, 
                                                                 computer_science == 2 ~ 1, 
                                                                 further_additional_science == 2 ~ 1,
                                                                 science == 2 ~ 1, 
                                                                 science_modular == 2 ~ 1,
                                                                 additional_science_modular == 2 ~ 1,
                                                                 biology == 2 ~ 1, 
                                                                 human_biology == 2 ~ 1,
                                                                 physics == 2 ~ 1, 
                                                                 chemistry == 2 ~ 1,
                                                                 science_I == 2 ~ 1, 
                                                                 biology_I == 2 ~ 1, 
                                                                 chemistry_I == 2 ~ 1, 
                                                                 physics_I == 2 ~ 1, 
                                                                 additional_science == 3 ~ 1, 
                                                                 computer_science == 3 ~ 1, 
                                                                 science == 3 ~ 1, 
                                                                 TRUE ~ 0))
#is.na(have_science) ~ 0))
#cm who havent satisifed conditions in case_when will be NA, convert these to 0 for no dont have subject 
#have_science$science_gcse[is.na(have_science$science_gcse)] <- 0
#pull out just mcsid, and final binary variable for if have science subjects
science_gcse <- have_science %>% select(mcsid, science_gcse)

#add in those who say have none of these subjects - for those who live in england, wales or NI (will do scotland separately)
#country = 1, 2 or 4. country !=3 (ie not scotland)
no_quals = qualifications1 %>% filter(gc_s_qual_none== 1) # be careful here as for rest of rows per cm, this will be NA.
no_quals = no_quals %>% select(mcsid, gcnum00, gc_rowid, gc_s_qual_none) #get relevant variables 
#add country 
no_quals = merge(no_quals, country, by = "mcsid")
no_quals = no_quals %>% filter(country !=3)
no_quals = no_quals %>% select(mcsid, gc_s_qual_none)
names(no_quals) = c("mcsid", "no_quals")


#create a final dataframe with binary variable for each of core subjects, to then create a final overall variable ####
#this overall variable will be 1 if cm has all of the core subjects and 0 if they dont have all of the core subjects.
core_gcse = merge(all=TRUE, english_gcse, maths_gcse, by="mcsid")
core_gcse = merge(all=TRUE, core_gcse, science_gcse, by="mcsid")
core_gcse = merge(all = TRUE, core_gcse, no_quals, by = "mcsid")
#create new core subjects variable, same process as above
#& in case_when means has to satisfy both of these conditions to get a 1 on the binary variable 
core_gcse = core_gcse %>% mutate(core_subjects = case_when(english_gcse == 1 & maths_gcse == 1 & science_gcse ==1 ~1, 
                                                           no_quals == 1 ~ 0, 
                                                           TRUE ~ 0)) #check this doing right thing
#core_gcse$core_subjects[is.na(core_gcse$core_subjects)] <- 0

#SELECT JUST MCSID AND CORE_SUBJECTS. 
core_gcse_subjects = core_gcse %>% select(mcsid, core_subjects)

#NATIONAL FIVE/ SCOTLAND BINARY VARIABLE####  
n5 = qualifications1 %>% filter(gc_s_qual_five == 1)
n5 = qualifications1[qualifications1$mcsid %in% n5$mcsid,] 



n5 = n5 %>% select(mcsid, gcnum00, gc_rowid,gc_s_qual_nfir_r20, gc_l_fvsb_name_r30, gc_l_fvgd) 
names(n5) <- c("mcsid", "cm_number", "row_id", "total_quals", "subject_name", "subject_grade")

n5_subject_key = c(`3`	 = "Design and Manufacture",
                   `4`	 = "Drama",
                   `7`	 = "Energy (Skills for Work)",
                   `8`	 = "Engineering Science",
                   `10`	 = "Language: English",
                   `12`	 = "ESOL (English for Speakers of Other Languages)",
                   `14`	 = "Financial Services (Skills for Work)",
                   `15`	 = "Food and Drink Manufcturing Industry (Skills for Work)",
                   `16`	 = "Language: French",
                   `19`	 = "Geography",
                   `20`	 = "Language: German",
                   `21`	 = "Graphic Communication",
                   `25`	 = "History",
                   `28`	 = "Hospitality:Practical Cookery",
                   `34`	 = "Martime Skills (Skills for Work)",
                   `35`	 = "Mathematics",
                   `37`	 = "Modern Studies",
                   `38`	 = "Music",
                   `41`	 = "Physical Education",
                   `42`	 = "Physics",
                   `45`	 = "Practical Woodworking",
                   `47`	 = "Religious, Moral and Philosophical Studies",
                   `50`	 = "Language: Spanish",
                   `52`	 = "Textiles Industry  (Skills for Work)",
                   `56`	 = "Art and Design",
                   `57`	 = "Biology",
                   `58`	 = "Business Management",
                   `59`	 = "Language: Cantonese",
                   `60`	 = "Care",
                   `61`	 = "Chemistry",
                   `63`	 = "Computing Science",
                   `64`	 = "Other",
                   `65`	 = "Recoded due to low counts - check SA")
#-9	 = Refusal
#-8	 = Do not know
#-1	 = Not applicable
n5$subject_name = as.factor(n5$subject_name)
n5$subject_name =  recode(n5$subject_name, !!!n5_subject_key)
n5 = n5 %>%  filter(!is.na(subject_name))
#grades
#Value = 1.0	Label = A
#Value = 2.0	Label = B
#Value = 3.0	Label = C
#Value = 4.0	Label = D
#Value = 5.0	Label = No award (fail)
#Value = -9.0	Label = Refusal
#Value = -8.0	Label = Do not know
#Value = -1.0	Label = Not applicable
#recode so that A is a higher number. 
#check what want scale of this to be  when meet!!

n5$subject_grade =  as.numeric(n5$subject_grade)
n5$subject_grade  = recode(n5$subject_grade, `1` = 5, `2` = 4, `3` = 3, `4` =2, `5` = 0)

#create binary variable for if have subject vs not ####
#1 = yes, 0= no
#will need to recode this later - want 0= yes have subjects, 1 = no don't have subject
#National five
#use xtabs to create a dataframe that has 0 or 1 for each cohort member having each subject
n5_subject = as.data.frame(xtabs(~n5$mcsid + n5$subject_name))
#convert this dataframe to wide so each column is a subject, entries will be 0 or 1 based on whether cm has that subject
wide_n5_subject = pivot_wider(n5_subject, id_cols=n5.mcsid, names_from = n5.subject_name,
                              values_from = Freq)

#first, get those who have english subjects - select english subjects from wide_n5_subject ####
have_english_n5 <- wide_n5_subject %>% select(n5.mcsid, contains("english"))
names(have_english_n5) <- c("mcsid", "english") #rename columns

#add a new column of 0/1, will be 1 if a CM has any of these subjects
#check when meet if either lang or lit is fine or want both. if so, replace , in case_when with &
SCOThave_english <- have_english_n5 %>% mutate(english_n5 = case_when(english == 1 ~ 1,
                                                                      english == 2 ~ 1, 
                                                                      TRUE ~ 0 ))

#is.na(have_english) ~ 0))
#cm who havent satisifed conditions in case_when will be NA, convert these to 0 for no dont have subject 
#SCOThave_english$english_n5[is.na(SCOThave_english$english_n5)] <- 0
#pull out just mcsid, and final binary variable for if have english subjects
english_n5 <- SCOThave_english %>% select(mcsid, english_n5)

#maths####
#first, get those who have maths subjects - select maths subjects from wide_n5_subject ####
have_maths_n5 <- wide_n5_subject %>% select(n5.mcsid, contains("math"))
names(have_maths_n5) <- c("mcsid", "maths") #rename columns

#add a new column of 0/1, will be 1 if a CM has any of these subjects
#check when meet if either lang or lit is fine or want both. if so, replace , in case_when with &
SCOThave_maths <- have_maths_n5 %>% mutate(maths_n5 = case_when(maths == 1 ~ 1,
                                                                maths == 2 ~ 1,
                                                                maths == 3 ~ 1, 
                                                                TRUE ~ 0))

#is.na(have_maths) ~ 0))
#cm who havent satisifed conditions in case_when will be NA, convert these to 0 for no dont have subject 
#SCOThave_maths$maths_n5[is.na(SCOThave_maths$maths_n5)] <- 0
#pull out just mcsid, and final binary variable for if have maths subjects
maths_n5 <- SCOThave_maths %>% select(mcsid, maths_n5)

#first, get those who have science subjects - select science subjects from wide_n5_subject ####
have_science_n5 <- wide_n5_subject %>% select(n5.mcsid, Biology, Chemistry, Physics)
names(have_science_n5) <- c("mcsid", "biology", "chemistry", "physics") #rename columns

#add a new column of 0/1, will be 1 if a CM has any of these subjects
#check when meet if either lang or lit is fine or want both. if so, replace , in case_when with &
SCOThave_science <- have_science_n5 %>% mutate(science_n5 = case_when(biology == 1 ~ 1,
                                                                      chemistry == 1 ~ 1,
                                                                      physics == 1 ~ 1,
                                                                      biology == 2 ~ 1,
                                                                      chemistry == 2 ~ 1,
                                                                      physics == 2 ~ 1,
                                                                      biology == 3 ~ 1, 
                                                                      TRUE ~ 0))

#is.na(have_science) ~ 0))
#cm who havent satisifed conditions in case_when will be NA, convert these to 0 for no dont have subject 
#SCOThave_science$science_n5[is.na(SCOThave_science$science_n5)] <- 0
#pull out just mcsid, and final binary variable for if have science subjects
science_n5 <- SCOThave_science %>% select(mcsid, science_n5)

#add in those who have N4 quals but not N5 quals --> these will be 0 in binary variable.####
#national 4 qualifications 

n4_only = qualifications1 %>% filter(gc_s_qual_four == 1 | gc_s_qual_five == 1) #n4 = yes, n5 = yes
n4_only = n4_only %>% select(mcsid, gc_s_qual_four, gc_s_qual_five)
n4_only = n4_only %>% filter(gc_s_qual_four == 1)
#n4_only = qualifications1[qualifications1$mcsid %in% n4_only$mcsid,] 
n4_only = n4_only %>% mutate(national_4_only = case_when(gc_s_qual_four == 1 & 
                                                           gc_s_qual_five == 2 ~ 1, 
                                                         TRUE ~ 0 ))
#n4_only$national_4_only[is.na(n4_only$national_4_only)] <- 0
n4_only = n4_only %>% filter(national_4_only == 1)

n4_only = n4_only %>% select(mcsid, national_4_only)


#add in those who say have none of these subjects - for those who live in Scotland)
#country = 3 
no_quals_scotland = qualifications1 %>% filter(gc_s_qual_none== 1) # be careful here as for rest of rows per cm, this will be NA.
no_quals_scotland = no_quals_scotland %>% select(mcsid, gcnum00, gc_rowid, gc_s_qual_none) #get relevant variables 
#add country 
no_quals_scotland = merge(no_quals_scotland, country, by = "mcsid")
no_quals_scotland = no_quals_scotland %>% filter(country ==3)
no_quals_scotland = no_quals_scotland %>% select(mcsid, gc_s_qual_none)
names(no_quals_scotland) = c("mcsid", "no_quals_scotland")


#create a final dataframe with binary variable for each of core subjects, to then create a final overall variable ####
#this overall variable will be 1 if cm has all of the core subjects and 0 if they dont have all of the core subjects.
core_n5 = merge(all=TRUE, english_n5, maths_n5, by="mcsid")
core_n5 = merge(all=TRUE, core_n5, science_n5, by="mcsid")
core_n5 = merge(all = TRUE, core_n5, no_quals_scotland, by = "mcsid")
core_n5 = merge(all = TRUE, core_n5, n4_only, by = "mcsid")
#create new core subjects variable, same process as above
#& in case_when means has to satisfy both of these conditions to get a 1 on the binary variable 
core_n5 = core_n5 %>% mutate(core_n5subjects = case_when(english_n5 == 1 & maths_n5 == 1 & science_n5 ==1 ~1, 
                                                       no_quals_scotland == 1 ~ 0, national_4_only == 1 ~ 0)) #add TRUE ~ 0 here too? 
core_n5$core_n5subjects[is.na(core_n5$core_n5subjects)] <- 0

#SELECT JUST MCSID AND CORE_SUBJECTS. 
core_n5_subjects = core_n5 %>% select(mcsid, core_n5subjects)

#COMBINE GCSE AND NATIONAL 5 BINARY VARIABLES INTO ONE FINAL VARIABLE - check if this is best thing to do or if want to keep sep ####
core_subjects_combined = merge(all=TRUE, core_gcse_subjects, core_n5_subjects, by = "mcsid") #check n of this

#combine into 1 variable
#core_subjects_combined$core_subjects_binary = ifelse(!is.na(core_subjects_combined$core_subjects), 
                                                     #core_subjects_combined$core_subjects, core_subjects_combined$core_n5subjects)
#five CM who have both gcse and national 5. see notebook for details. 
#if have core subjects in N5 but not GCSE (2 cases of this), make sure value of binary variable is 1
#in all other cases, where N5 is missing, use GCSE binary and where GCSE  missing use N5 binary
core_subjects_combined = core_subjects_combined %>% mutate(core_subjects_binary = 
                                                             case_when(core_subjects == 0 & core_n5subjects == 1 ~  1, 
                                                                       core_subjects == 1 & core_n5subjects == 1 ~ 1,
                                                                       core_subjects == 0 & core_n5subjects == 0 ~ 0,
                                                                       core_subjects == 1 & core_n5subjects == 0 ~ 1,
                                                                       is.na(core_n5subjects) ~ core_subjects, 
                                                                       is.na(core_subjects) ~ core_n5subjects))

taken_core_binary = core_subjects_combined %>% select(mcsid, core_subjects_binary)

#cm have grade 4 or above on core subjects ####


#binary variable of who got above benchmark in core subjects at gcse
#core subjects = maths, english (lang and/or lit), science
#for science, any form of science as due to self reports these may be reported differently 

#could put into wide format where the column names are subject name and column values are subject grade? 
gcse_grades <- gcse %>% select(mcsid, row_id,  subject_name, subject_grade)
gcse_grades$subject_name = as.character(gcse_grades$subject_name)
gcse_grades$subject_grade = as.numeric(gcse_grades$subject_grade)
#wide_gcse_grades = pivot_wider(gcse_grades, id_cols=c("mcsid"), names_from = subject_name,
#  values_from = subject_grade, values_fill = 0)


#identify those who have duplicate subjects based on wide data
#duplicated_wide = wide_grades$mcsid[duplicated(wide_grades$mcsid)]
#remove duplicate subjects and keep the subject with the higher grade
gcse_grades1 <- gcse_grades %>% group_by(mcsid) %>% 
  arrange(desc(subject_grade), .by_group = TRUE) %>%  #arrange subject_grade by descending order, so distinct can select the higher entry 
  distinct(subject_name, .keep_all = TRUE) #this will give only one of the duplicated values - because have sorted subject grade in descending order, this will keep the higher grade for the duplicated subject


#convert to wide dataset where each column name is a subject with the grade as the entry per CM
wide_grades = gcse_grades1 %>% select(!row_id)
wide_grades = wide_grades %>%
  group_by(mcsid, subject_name) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = subject_name, values_from = subject_grade) %>% 
  select(-row)


#wide_grades = wide_grades %>% rename(english_lit = "Language: English Literature", 
 #                                    english = "Language: English", 
  #                                   english_lang = "Language: English Language", 
   #                                  maths = "Mathematics", 
 #                                    maths_linear = "Mathematics - Linear", 
  #                                   maths_numeracy = "Mathematics - Numeracy", 
   #                                  further_maths = "Further Mathematics", 
    #                                 additional_maths =  " Additional Mathematics", 
     #                                biology = "Biology", 
      #                               chemistry = "Chemistry", 
       #                              physics = "Physics",
        #                             additional_science = "Additional Science", 
         #                            science = "Science", 
          #                           applied_science = "Applied Science", 
           #                          combined_science = `Combined Science `,
            #                         modular_science = "Science (Modular)",
             #                        further_additional_science = `Further Additional Science`,
              #                       computer_science = `Computer Science`,
               #                      additional_applied_science = `Additional Applied Science`,
                #                     additional_science_modular = `Additional Science (Modular)`,
                 #                    human_biology = `Biology (Human)`)
                                    

#select core subjects from this wide dataset

core_subjects_grades <- wide_grades %>% select(mcsid, c("Language: English", "Language: English Language", "Language: English Literature",
                                                        "Mathematics", "Mathematics - Linear", "Mathematics - Numeracy", contains("math"), "Biology", "Chemistry", "Physics"), #include additional mathematics?
                                               contains("science"), contains("biology"))

names(core_subjects_grades) <- c("mcsid", "english", "english_lang", "english_lit",
                                 "maths", "maths_linear", "maths_numeracy", "further_maths", "additional_maths", "biology", "chemistry", "physics", 
                                 "additional_science", "science", "applied_science", "combined_science", "modular_science", "further_additional_science", "computer_science", 
                                 "additional_applied_science", "additional_science_modular", "human_biology")

#core_grades$english[core_grades$english == 'NULL'] <- NA
#core_grades$english_lang[core_grades$english_lang == 'NULL'] <- NA
#core_grades$english_lit[core_grades$english_lit == 'NULL'] <- NA
#core_grades$maths[core_grades$maths == 'NULL'] <- NA
#core_grades$maths_linear[core_grades$maths_linear == 'NULL'] <- NA
#core_grades$maths_numeracy[core_grades$maths_numeracy == 'NULL'] <- NA

#create core grades binary variable - those with 4 and above in english, maths and science (in one of each of these subjects), then binary variable = 1
#make sure this also includes iGCSE grades

#add in Igcse to core_subjects data - merge? 

#could put into wide format where the column names are subject name and column values are subject grade? 
Igcse_grades <- Igcse %>% select(mcsid, row_id,  subject_name, subject_grade)
Igcse_grades$subject_name = as.character(Igcse_grades$subject_name)
Igcse_grades$subject_grade = as.numeric(Igcse_grades$subject_grade)

#identify those who have duplicate subjects based on wide data
#duplicated_wide = wide_grades$mcsid[duplicated(wide_grades$mcsid)]
#remove duplicate subjects and keep the subject with the higher grade
Igcse_grades1 <- Igcse_grades %>% group_by(mcsid) %>% 
  arrange(desc(subject_grade), .by_group = TRUE) %>%  #arrange subject_grade by descending order, so distinct can select the higher entry 
  distinct(subject_name, .keep_all = TRUE) #this will give only one of the duplicated values - because have sorted subject grade in descending order, this will keep the higher grade for the duplicated subject



#convert to wide dataset where each column name is a subject with the grade as the entry per CM
wide_IGCSE_grades = Igcse_grades1 %>% select(!row_id)
wide_IGCSE_grades = wide_IGCSE_grades %>%
  group_by(mcsid, subject_name) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = subject_name, values_from = subject_grade) %>% 
  select(-row)

core_iGCSE_subjects_grades <- wide_IGCSE_grades %>% select(mcsid, c("Biology", "Chemistry", "Physics", "Science"), #include additional mathematics?
                                                           contains("english"), contains("math"))

names(core_iGCSE_subjects_grades) <- c("mcsid", "biology_I", "chemistry_I", "physics_I", "science_I", "english_first_lang_I", "english_lit_I", "maths_I")

#add in those who say have none of these subjects - for those who live in england, wales or NI (will do scotland separately)
#country = 1, 2 or 4. country !=3 (ie not scotland)
no_quals = qualifications1 %>% filter(gc_s_qual_none== 1) # be careful here as for rest of rows per cm, this will be NA.
no_quals = no_quals %>% select(mcsid, gcnum00, gc_rowid, gc_s_qual_none) #get relevant variables 
#add country 
no_quals = merge(no_quals, country, by = "mcsid")
no_quals = no_quals %>% filter(country !=3)
no_quals = no_quals %>% select(mcsid, gc_s_qual_none)
names(no_quals) = c("mcsid", "no_quals")

#combine core grades together into one dataframe (for GCSE and iGCSE)
combined_core_grades = merge(all=TRUE, core_subjects_grades, core_iGCSE_subjects_grades, by = "mcsid")
combined_core_grades = merge(all=TRUE, combined_core_grades, no_quals, by="mcsid")

#create binary variable. those who score >=4 on at least an english subject, at least one maths subject and at least 1 science subject = 1. else = 0. 
core_grades <- combined_core_grades %>%  mutate(benchmark = case_when(
  (english >= 4  | english_lang >= 4  | english_lit >= 4 | english_first_lang_I >= 4 |english_lit_I >= 4 ) & 
    (maths >= 4 | maths_linear >= 4 | maths_numeracy >= 4 | further_maths >= 4 | additional_maths >= 4 | maths_I >= 4) &
    (biology >= 4 | chemistry >= 4 | physics >= 4 | additional_science >= 4 | science >= 4 | applied_science >= 4 |
       combined_science >= 4 | modular_science >= 4 | further_additional_science >= 4 |computer_science >=4 |
     additional_applied_science >= 4 | additional_science_modular >= 4 |human_biology >= 4 | 
       biology_I >= 4 | chemistry_I >= 4 | physics_I >= 4 | science_I >= 4) ~ 1,
  TRUE ~ 0))

#get counts
benchmark = table(core_grades$benchmark)
prop.table(benchmark)*100

#NATIONAL 5 - SCOTALND. GRADE C OR ABOVE. 

n5 = qualifications1 %>% filter(gc_s_qual_five == 1)
n5 = qualifications1[qualifications1$mcsid %in% n5$mcsid,] 



n5 = n5 %>% select(mcsid, gcnum00, gc_rowid,gc_s_qual_nfir_r20, gc_l_fvsb_name_r30, gc_l_fvgd) 
names(n5) <- c("mcsid", "cm_number", "row_id", "total_quals", "subject_name", "subject_grade")

n5_subject_key = c(`3`	 = "Design and Manufacture",
                   `4`	 = "Drama",
                   `7`	 = "Energy (Skills for Work)",
                   `8`	 = "Engineering Science",
                   `10`	 = "Language: English",
                   `12`	 = "ESOL (English for Speakers of Other Languages)",
                   `14`	 = "Financial Services (Skills for Work)",
                   `15`	 = "Food and Drink Manufcturing Industry (Skills for Work)",
                   `16`	 = "Language: French",
                   `19`	 = "Geography",
                   `20`	 = "Language: German",
                   `21`	 = "Graphic Communication",
                   `25`	 = "History",
                   `28`	 = "Hospitality:Practical Cookery",
                   `34`	 = "Martime Skills (Skills for Work)",
                   `35`	 = "Mathematics",
                   `37`	 = "Modern Studies",
                   `38`	 = "Music",
                   `41`	 = "Physical Education",
                   `42`	 = "Physics",
                   `45`	 = "Practical Woodworking",
                   `47`	 = "Religious, Moral and Philosophical Studies",
                   `50`	 = "Language: Spanish",
                   `52`	 = "Textiles Industry  (Skills for Work)",
                   `56`	 = "Art and Design",
                   `57`	 = "Biology",
                   `58`	 = "Business Management",
                   `59`	 = "Language: Cantonese",
                   `60`	 = "Care",
                   `61`	 = "Chemistry",
                   `63`	 = "Computing Science",
                   `64`	 = "Other",
                   `65`	 = "Recoded due to low counts - check SA")
#-9	 = Refusal
#-8	 = Do not know
#-1	 = Not applicable
n5$subject_name = as.factor(n5$subject_name)
n5$subject_name =  recode(n5$subject_name, !!!n5_subject_key)
n5 = n5 %>%  filter(!is.na(subject_name))
#grades
#Value = 1.0	Label = A
#Value = 2.0	Label = B
#Value = 3.0	Label = C
#Value = 4.0	Label = D
#Value = 5.0	Label = No award (fail)
#Value = -9.0	Label = Refusal
#Value = -8.0	Label = Do not know
#Value = -1.0	Label = Not applicable
#recode so that A is a higher number. 
#check what want scale of this to be  when meet!!

n5$subject_grade =  as.numeric(n5$subject_grade)
n5$subject_grade  = recode(n5$subject_grade, `1` = 5, `2` = 4, `3` = 3, `4` =2, `5` = 0)

#convert to wide format
#could put into wide format where the column names are subject name and column values are subject grade? 
n5_grades <- n5 %>% select(mcsid, row_id,  subject_name, subject_grade)
n5_grades$subject_name = as.character(n5_grades$subject_name)
n5_grades$subject_grade = as.numeric(n5_grades$subject_grade)
#wide_n5_grades = pivot_wider(n5_grades, id_cols=c("mcsid"), names_from = subject_name,
#  values_from = subject_grade, values_fill = 0)


#identify those who have duplicate subjects based on wide data
#duplicated_wide = wide_grades$mcsid[duplicated(wide_grades$mcsid)]
#remove duplicate subjects and keep the subject with the higher grade
n5_grades1 <- n5_grades %>% group_by(mcsid) %>% 
  arrange(desc(subject_grade), .by_group = TRUE) %>%  #arrange subject_grade by descending order, so distinct can select the higher entry 
  distinct(subject_name, .keep_all = TRUE) #this will give only one of the duplicated values - because have sorted subject grade in descending order, this will keep the higher grade for the duplicated subject


#convert to wide dataset where each column name is a subject with the grade as the entry per CM
wide_n5_grades = n5_grades1 %>% select(!row_id)
wide_n5_grades = wide_n5_grades %>%
  group_by(mcsid, subject_name) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = subject_name, values_from = subject_grade) %>% 
  select(-row)

core_n5_grades <- wide_n5_grades %>% select(mcsid, c("Language: English",  contains("math"), "Computing Science", "Biology", "Chemistry", "Physics"))
names(core_n5_grades) <- c("mcsid", "english", "maths","computer_science", "biology", "chemistry", "physics")

core_n5_grades <- core_n5_grades%>%  mutate(benchmarkN5 = case_when((english >= 3 )  & 
                                                                      (maths >= 3 )  &
                                                                      (computer_science >=3 |
                                                                         biology >= 3 | chemistry >= 3 | physics >= 3 ) ~ 1,
                                                                    TRUE ~ 0))



#combine N5 and GCSE core grades into one variable
#first combine the dataframes
#replace is/na with the other one 
core_grades_combined = merge (all=TRUE, core_grades, core_n5_grades, by="mcsid")
core_grades_combined = core_grades_combined %>% select(mcsid, benchmark, benchmarkN5)
#merge into one variable
#core_grades_combined$core_grades_binary = ifelse(!is.na(core_grades_combined$benchmark), 
# core_grades_combined$benchmark, core_grades_combined$benchmarkN5)

core_grades_combined = core_grades_combined %>% mutate(benchmark_binary = 
                                                         case_when(benchmark == 0 & benchmarkN5 == 1 ~  1, 
                                                                   benchmark == 1 & benchmarkN5 == 1 ~ 1,
                                                                   benchmark == 0 & benchmarkN5 == 0 ~ 0,
                                                                   benchmark == 1 & benchmarkN5 == 0 ~ 1,
                                                                   is.na(benchmarkN5) ~ benchmark, 
                                                                   is.na(benchmark) ~ benchmarkN5))

#take those who sat the core subjects  - so binary variable here will be those who got 4 and above but only for those who took the subjects in the first place. 

took_core_binary = taken_core_binary %>% filter(core_subjects_binary == 1)

core_benchmark_binary = core_grades_combined[core_grades_combined$mcsid %in% took_core_binary$mcsid,]
#select relevant variables 
core_benchmark_binary = core_benchmark_binary %>% select(mcsid, benchmark_binary)

#welsh GCSE - also include as a core subject if CM from wales (version with and version without, one will be a sensitivity) ####

#welsh  - sensitivity check? ####
have_welsh <- wide_gcse_subject %>% select(gcse.mcsid, contains("welsh"))
names(have_welsh) <- c("mcsid", "welsh", "welsh_second_language", "welsh_second_language_short", "welsh_first_language", 
                       "welsh_literature") #rename columns

#merge with country so can select those who live in wales
have_welsh = merge(have_welsh, country, by="mcsid")
#add a new column of 0/1, will be 1 if a CM has any of these subjects

#have_welsh$country = country$gactry00

have_welsh = have_welsh %>% filter(country == 2)
have_welsh <- have_welsh %>% mutate(welsh_gcse = case_when(welsh == 1 ~ 1, 
                                                           welsh_second_language == 1 ~ 1, 
                                                           welsh_second_language_short == 1 ~ 1, 
                                                           welsh_first_language == 1 ~ 1,
                                                           welsh_literature== 1 ~ 1))
#is.na(have_science) ~ 0))
#cm who havent satisifed conditions in case_when will be NA, convert these to 0 for no dont have subject 
have_welsh$welsh_gcse[is.na(have_welsh$welsh_gcse)] <- 0
#pull out just mcsid, and final binary variable for if have english subjects
welsh_gcse <- have_welsh %>% select(mcsid, welsh_gcse)

#add in no quals for wales
no_quals_wales = qualifications1 %>% filter(gc_s_qual_none== 1) # be careful here as for rest of rows per cm, this will be NA.
no_quals_wales = no_quals_wales %>% select(mcsid, gc_s_qual_none) #get relevant variables 
#add country 
no_quals_wales = merge(no_quals_wales, country, by = "mcsid")
no_quals_wales = no_quals_wales %>% filter(country ==2)
no_quals_wales = no_quals_wales %>% select(mcsid, gc_s_qual_none)
names(no_quals_wales) = c("mcsid", "no_quals")

core_gcse_wales = merge(all=TRUE, english_gcse, maths_gcse, by="mcsid")
core_gcse_wales = merge(all=TRUE, core_gcse_wales, science_gcse, by="mcsid")
core_gcse_wales = merge(all=TRUE, core_gcse_wales, welsh_gcse, by="mcsid") 
core_gcse_wales = merge(all=TRUE, core_gcse_wales, no_quals_wales, by="mcsid") 
core_gcse_wales = merge(core_gcse_wales, country, by = "mcsid")
core_gcse_wales = core_gcse_wales %>% filter(country == 2)
core_gcse_wales = core_gcse_wales %>% mutate(core_subjects1 = case_when(english_gcse == 1 & maths_gcse == 1 & science_gcse ==1 & welsh_gcse ==1 ~1, 
                                                                        no_quals == 1 ~ 0))
core_gcse_wales$core_subjects1[is.na(core_gcse_wales$core_subjects1)] <- 0

core_gcse_wales = core_gcse_wales %>% select(mcsid, core_subjects1)
core_gcse = core_gcse %>% select(mcsid, core_subjects)
core_subjects = merge(all=TRUE, core_gcse, core_gcse_wales, by = "mcsid")
core_subjects$have_core = ifelse(!is.na(core_subjects$core_subjects1), core_subjects$core_subjects1, core_subjects$core_subjects)


#add in binary grade variable here for this sensitivity check #