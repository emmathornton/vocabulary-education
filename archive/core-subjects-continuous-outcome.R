#continuous outcome variable
#average english, maths and science score 

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

#convert gcse subject names into text names - key for this which will do later. ####
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

#those who said yes to having gcse qualification####
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

gcse_grades <- gcse %>% select(mcsid, row_id,  subject_name, subject_grade)
gcse_grades$subject_name = as.character(gcse_grades$subject_name)
gcse_grades$subject_grade = as.numeric(gcse_grades$subject_grade)
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

#select core subjects from this wide dataset

core_subjects_grades <- wide_grades %>% select(mcsid, c("Language: English", "Language: English Language", "Language: English Literature",
                                                        "Mathematics", "Mathematics - Linear", "Mathematics - Numeracy", contains("math"), "Biology", "Chemistry", "Physics"), #include additional mathematics?
                                               contains("science"), contains("biology"))

names(core_subjects_grades) <- c("mcsid", "english", "english_lang", "english_lit",
                                 "maths", "maths_linear", "maths_numeracy", "further_maths", "additional_maths", "biology", "chemistry", "physics", 
                                 "additional_science", "science", "applied_science", "combined_science", "modular_science", "further_additional_science", "computer_science", 
                                 "additional_applied_science", "additional_science_modular", "human_biology")
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

combined_core_grades = merge(all=TRUE, core_subjects_grades, core_iGCSE_subjects_grades, by = "mcsid")

