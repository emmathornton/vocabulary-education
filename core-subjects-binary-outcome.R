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

#add in BTEC science at level 2 (equiv to a pass at GCSE)

btec_subject_key <- c(`1`	= "3D Design",
                      `2`	= "Achieving Excellence in Skills Performance",
                      `3`	= "Administration",
                      `4`	= "Adult Care",
                      `6`	= "Advertisement Production for the Electronic and the Printed Page",
                      `7`	= "Aeronautical Engineering",
                      `8`	= "Aerospace and Aviation Engineering",
                      `9`	= "Aerospace and Aviation Engineering",
                      `13`	= "Airline and Airport Operations",
                      `14`	= "Ambulance Care Assistance",
                      `19`	= "Anti-Armour Weapon Systems",
                      `20`	= "Apparel, Footwear, Leather or Textile Production",
                      `23`	= "Applied Science",
                      `24`	= "Aromatherapy Massage",
                      `28`	= "Assisting and Moving Individuals for Social Care Settings",
                      `29`	= "Auto Electrical and Mobile Electrical Principles",
                      `32`	= "Awareness of Dementia",
                      `33`	= "Awareness of End of Life Care",
                      `34`	= "Barista Skills",
                      `37`	= "Beauty Therapy Sciences",
                      `39`	= "Body Building Principles",
                      `40`	= "Body Massage",
                      `42`	= "Business",
                      `49`	= "Call Handling Operations",
                      `50`	= "Caring",
                      `53`	= "Carry and Deliver Goods",
                      `54`	= "CCTV Operations (Public Space Surveillance)",
                      `60`	= "Cleaning Principles",
                      `61`	= "Cleaning Principles (Confined Spaces)",
                      `62`	= "Cleaning Principles (Deep Cleaning of Internal Equipment Surfaces and Areas)",
                      `63`	= "Cleaning Principles (External Surfaces and Areas)",
                      `64`	= "Cleaning Principles (Food Areas)",
                      `65`	= "Cleaning Principles (Glazed Surfaces and Facades)",
                      `66`	= "Cleaning Principles (High Risk Areas; Controlled Environments)",
                      `67`	= "Cleaning Principles (Interiors and Washrooms)",
                      `68`	= "Cleaning Principles (Maintenance and Minor Repairs of Property)",
                      `69`	= "Cleaning Principles (Manual Street Cleaning)",
                      `70`	= "Cleaning Principles (Mechanical Street Cleaning)",
                      `71`	= "Cleaning Principles (Passenger Transport Interiors)",
                      `72`	= "Cleaning Principles (Periodic Cleaning of Hard and Semi-hard Floors)",
                      `73`	= "Cleaning Principles (Periodic Cleaning of Soft Floors and Furnishings)",
                      `74`	= "Cleaning Principles (Specialist Electronic Equipment)",
                      `75`	= "Cleaning Principles (Water-Fed Pole Systems)",
                      `76`	= "Cleaning Principles (Working Safely at Heights)",
                      `77`	= "Close Protection Operations",
                      `78`	= "Coastal Zone Management",
                      `80`	= "Communications Electronic Engineering",
                      `81`	= "Communications Technology",
                      `82`	= "Community Safety for Accredited Persons",
                      `88`	= "Computing for Creative Industries",
                      `89`	= "Conflict Management",
                      `91`	= "Construction",
                      `94`	= "Construction Plant Operations",
                      `96`	= "Contact Centre Operations",
                      `97`	= "Contact Centre Skills",
                      `98`	= "Control and Administration of Medicines",
                      `99`	= "Counselling Skills",
                      `100`	= "Countryside Access and Recreation",
                      `102`	= "Countryside Management",
                      `103`	= "Countryside Studies",
                      `104`	= "Craft Cuisine Skills",
                      `109`	= "Customer Contact",
                      `110`	= "Customer Service",
                      `111`	= "Customer Service Operations",
                      `112`	= "Dementia Care",
                      `113`	= "Dental Technology",
                      `117`	= "Developing An Entrepreneurial Approach",
                      `118`	= "Diagnosing Faults in Land-based Machines",
                      `119`	= "Digital Audio Editing",
                      `121`	= "Digital Content Production",
                      `122`	= "Digital Film and Video Production",
                      `123`	= "Digital Games Design and Development",
                      `124`	= "Digital Games Production",
                      `125`	= "Digital Literacy Skills (Entry 1)",
                      `128`	= "Digital Publishing",
                      `130`	= "Digital Tools and Technologies",
                      `131`	= "Digital Video Editing",
                      `132`	= "Disengagement and Non-restrictive Physical Intervention Skills",
                      `133`	= "Disengagement and Physical Intervention Skills",
                      `134`	= "Door Supervision",
                      `135`	= "Drugs Awareness in the Licensed Retail Sector",
                      `136`	= "Early Years",
                      `139`	= "e-business",
                      `140`	= "Ecological Surveys and Techniques",
                      `145`	= "Employment Awareness in Active Leisure and Learning",
                      `146`	= "Employment Responsibilities and Rights in Health, Social Care and Children and Young People s Settings",
                      `147`	= "Engineering",
                      `152`	= "Engineering Electronics and Computer Control Technologies",
                      `155`	= "Enterprise and Entrepreneurship",
                      `158`	= "Environmental Sustainability",
                      `159`	= "Equine Management",
                      `160`	= "Equine Management (Equitation)",
                      `161`	= "Equine Management (Yard Management)",
                      `163`	= "e-Responsibility for Education",
                      `166`	= "Essential Digital Literacy Skills",
                      `167`	= "Essential Employability Skills",
                      `169`	= "Event Support",
                      `171`	= "Explore Complementary Therapies",
                      `172`	= "Exploring the Caring Sectors",
                      `173`	= "Exploring the Construction and Engineering Sectors",
                      `175`	= "Facilities Services Principles",
                      `178`	= "Film and Television Production",
                      `179`	= "Film and Television Visual Effects",
                      `180`	= "Fine Art",
                      `182`	= "Fish Husbandry",
                      `184`	= "Fitness Instructing",
                      `185`	= "Fitness Services",
                      `186`	= "Floristry",
                      `187`	= "Food and Beverage Service",
                      `189`	= "Food Safety for Logistics",
                      `190`	= "Food Safety for Retail",
                      `194`	= "Forensic and Criminal Investigation",
                      `195`	= "Forensic Investigation",
                      `196`	= "Forestry and Arboriculture",
                      `197`	= "Foster Care",
                      `199`	= "Front Line Services",
                      `200`	= "Front of House Operations",
                      `201`	= "General Beverage Service Skills",
                      `204`	= "General Front Office Operations",
                      `205`	= "General Housekeeping Operations",
                      `208`	= "HACCP Based Food Safety Systems in Manufacturing",
                      `210`	= "Hairdressing Services",
                      `212`	= "Health and Safety in a Construction Environment",
                      `214`	= "Health and Social Care",
                      `217`	= "Healthcare Science",
                      `219`	= "Heavy Machine Gun Operations",
                      `220`	= "Heavy Vehicle Maintenance and Repair Principles",
                      `222`	= "Horse Care",
                      `223`	= "Horse Management",
                      `226`	= "Hospitality and Catering Principles (Beverage Service)",
                      `228`	= "Hospitality and Catering Principles (Food Production and Cooking)",
                      `232`	= "Hospitality and Catering Principles (Housekeeping)",
                      `233`	= "Hospitality and Catering Principles (Kitchen Services)",
                      `234`	= "Hospitality and Catering Principles (Professional Cookery)",
                      `235`	= "Hospitality and Tourism",
                      `236`	= "Hospitality Skills",
                      `237`	= "Hospitality Supervision",
                      `239`	= "Housing Practice",
                      `240`	= "Human Resource Practices in Hospitality",
                      `243`	= "ICT Systems and Principles",
                      `245`	= "Improving Performance for Manufacturing Engineering Operations",
                      `246`	= "Indian Head Massage",
                      `247`	= "Induction into Adult Social Care in Northern Ireland",
                      `248`	= "Induction to Supporting People who have Learning Disabilities",
                      `249`	= "Infection Control",
                      `250`	= "Infection Prevention and Control",
                      `251`	= "Information and Creative Technology",
                      `252`	= "Information Technology",
                      `253`	= "Information Technology (Specialist)",
                      `254`	= "Instructional Techniques",
                      `256`	= "Interactive Use of Media",
                      `257`	= "Intermediate Overseas Resort Operations",
                      `258`	= "Introducing Team Leading",
                      `259`	= "Introduction to Administering Examinations",
                      `260`	= "Introduction to Cabin Crew",
                      `261`	= "Introduction to Childcare",
                      `262`	= "Introduction to Contact Centres",
                      `263`	= "Introduction to Counselling Skills",
                      `264`	= "Introduction to Culinary Skills",
                      `266`	= "Introduction to Healthcare Science",
                      `267`	= "Introduction to Life Coaching Skills",
                      `268`	= "Introduction to Professional Cookery",
                      `270`	= "Introduction to Professional Food and Beverage Service Skills",
                      `271`	= "Introduction to the Hospitality Industry",
                      `272`	= "Introduction to the Role of the Professional Taxi and Private Hire Driver",
                      `274`	= "Investigating the Hospitality Industry",
                      `275`	= "IT",
                      `276`	= "IT at Work",
                      `277`	= "IT Practitioners (Software Development)",
                      `278`	= "IT Support",
                      `280`	= "Knowledge for a Professional Bus or Coach Driver",
                      `281`	= "Knowledge of Court/Tribunal Administration",
                      `282`	= "Knowledge of Custodial Care",
                      `283`	= "Knowledge of Providing Security Services",
                      `284`	= "Laboratory Science",
                      `285`	= "Land and Environment",
                      `286`	= "Land-based Studies",
                      `288`	= "Laundry and Dry Cleaning Technology",
                      `289`	= "Law and Legal Work",
                      `290`	= "Leadership Skills",
                      `292`	= "Lean Organisation Management Techniques",
                      `294`	= "Legal Secretaries",
                      `295`	= "Leisure Operations",
                      `297`	= "Lift Truck Maintenance and Repair Principles",
                      `299`	= "Lingerie fitting",
                      `300`	= "Lloyds and London Market Insurance",
                      `301`	= "Logistics",
                      `302`	= "Management (Supervision and Leadership)",
                      `303`	= "Managing Business Performance in Hospitality",
                      `304`	= "Managing Examinations",
                      `306`	= "Marketing for Hospitality",
                      `311`	= "Medical Administration",
                      `312`	= "Meeting the Requirements of Customers with Specific Needs in Hospitality, Leisure, Travel and Tourism",
                      `313`	= "Military Engineering",
                      `317`	= "Music",
                      `322`	= "Nutrition Awareness",
                      `324`	= "Operations and Maintenance Engineering",
                      `325`	= "Paediatric First Aid",
                      `328`	= "Performing Arts",
                      `330`	= "Performing Arts (Dance)",
                      `332`	= "Personal Safety Awareness",
                      `333`	= "Pharmaceutical Science",
                      `334`	= "Pharmacy Services",
                      `337`	= "Plant Maintenance",
                      `338`	= "Playwork",
                      `339`	= "Playwork for Early Years and Child Care Workers",
                      `340`	= "Polymer Processing and Materials Technology",
                      `342`	= "Preparation and Operation of a Tractor",
                      `343`	= "Preparation for Air Cabin Crew Service",
                      `344`	= "Preparation for Tourist Guiding",
                      `345`	= "Preparing for a Career in the Hospitality Industry",
                      `347`	= "Preparing to Work in Adult Social Care",
                      `348`	= "Prevention and Control of Infection",
                      `349`	= "Principles and Practice of Care",
                      `350`	= "Principles for Carrying and Delivering Goods by Road",
                      `353`	= "Principles of Bus and Coach Engineering and Maintenance (Electrical)",
                      `354`	= "Principles of Bus and Coach Engineering and Maintenance (Mechanical)",
                      `355`	= "Principles of Business Administration",
                      `357`	= "Principles of Customer Service",
                      `358`	= "Principles of Customer Service in Hospitality, Leisure, Travel and Tourism",
                      `359`	= "Principles of Funeral Operations and Services",
                      `360`	= "Principles of Holland & Barrett Vitamins, Minerals, Supplements and Health Products",
                      `361`	= "Principles of Marketing",
                      `362`	= "Principles of Providing Security Services",
                      `363`	= "Principles of Sales",
                      `364`	= "Principles of Team Leading",
                      `365`	= "Principles of the Creative and Cultural Sector",
                      `366`	= "Principles of the Cultural Heritage Sector",
                      `367`	= "Principles of Trade Business Services",
                      `368`	= "Principles of Working in Business Administration",
                      `369`	= "Principles of Working in Cleaning",
                      `370`	= "Principles of Working in Customer Service",
                      `371`	= "Principles of Working in Facilities Services",
                      `373`	= "Principles of Working in Hospitality and Catering (Hospitality Services)",
                      `374`	= "Principles of Working in Trade Business Services",
                      `375`	= "Principles of Working in Warehousing and Storage",
                      `380`	= "Professional Cookery for Professional Chefs (Kitchen and Larder)",
                      `381`	= "Professional Cookery for Professional Chefs (Patisserie and Confectionery)",
                      `382`	= "Professional Development for Working in Childrens Services",
                      `383`	= "Professional Food and Beverage Service",
                      `384`	= "Professional Patisserie and Confectionery",
                      `387`	= "Providing Financial Advice",
                      `388`	= "Public Sector Practice",
                      `390`	= "Recreation Assistants",
                      `391`	= "Recruitment Resourcing",
                      `392`	= "Retail",
                      `393`	= "Retail Beauty Consultancy",
                      `394`	= "Retail Knowledge",
                      `395`	= "Retail Knowledge (Beauty)",
                      `396`	= "Retail Operations",
                      `397`	= "Retailing",
                      `400`	= "Sailing and Watersports",
                      `401`	= "Sales",
                      `402`	= "Sales and Marketing",
                      `404`	= "Security Guarding",
                      `405`	= "Security Keyholding and Alarm Response",
                      `406`	= "Security Operations",
                      `407`	= "Setting up a Business Online",
                      `408`	= "Signing with Babies and Young Children",
                      `411`	= "Social Media",
                      `412`	= "Social Networking for Business",
                      `414`	= "Sound Engineering",
                      `415`	= "Sound Production",
                      `416`	= "Specialised Play for Sick Children and Young People",
                      `417`	= "Sport",
                      `424`	= "Staff Training and Development for Hospitality",
                      `425`	= "STEM",
                      `427`	= "Subsidiary Design",
                      `428`	= "Subsidiary Sailing and Watersports",
                      `429`	= "Supply Chain Management Principles",
                      `430`	= "Support for Deaf Learners",
                      `431`	= "Support Work in Social Care",
                      `432`	= "Supporting Activities and Events for Sport and Active Leisure",
                      `433`	= "Supporting Activity Provision in Social Care",
                      `434`	= "Supporting Employability and Personal Effectiveness",
                      `435`	= "Supporting Individuals with Learning Disabilities",
                      `436`	= "Supporting Learning in the Workplace",
                      `437`	= "Supporting Teaching and Learning in Schools",
                      `438`	= "Sustainability Skills",
                      `439`	= "Teaching Assistants",
                      `440`	= "Teaching Employability Skills",
                      `441`	= "Teaching Employability Skills and Vocational Learning",
                      `447`	= "Telecommunications",
                      `449`	= "Thermal Insulation",
                      `450`	= "Tourism Events Assistant",
                      `451`	= "Trade Business Services",
                      `453`	= "Transporting Passengers by Bus and Coach",
                      `454`	= "Transporting Passengers by Taxi and Private Hire",
                      `455`	= "Travel and Tourism",
                      `457`	= "Travel Operations",
                      `458`	= "Travel Services",
                      `459`	= "Underage Sales Prevention",
                      `460`	= "Understanding An Entrepreneurial Approach",
                      `462`	= "Understanding Enterprise and Entrepreneurship",
                      `463`	= "Understanding Stewarding at Spectator Events",
                      `464`	= "Understanding the Laws of Sport",
                      `465`	= "Understanding the Safe Use of Medicines",
                      `467`	= "Using and Promoting Multi-Channel Retailing in Store",
                      `468`	= "Vehicle Accident Repair Body Principles",
                      `470`	= "Vehicle Accident Repair Paint Principles",
                      `471`	= "Vehicle Fitting Principles",
                      `472`	= "Vehicle Immobilisation",
                      `473`	= "Vehicle Repair and Technology",
                      `474`	= "Vehicle Sales Principles",
                      `475`	= "Vehicle Service and Technology",
                      `477`	= "Video Journalism",
                      `480`	= "Warehouse Operatives",
                      `481`	= "Warehousing and Storage Principles",
                      `483`	= "Welcoming Tourists and Visitors to their Destination in Hospitality, Leisure, Travel and Tourism",
                      `484`	= "Work Experience for Enterprise",
                      `486`	= "Working in the Health Sector",
                      `489`	= "Animal Care and Veterinary Science",
                      `490`	= "Art, Design and Media",
                      `493`	= "Drama, Theatre Studies and Performing Arts",
                      `494`	= "Economics",
                      `495`	= "Education and Training",
                      `496`	= "Energy and Utilities",
                      `498`	= "Environmental Conservation",
                      `501`	= "Funeral Services",
                      `503`	= "Geography",
                      `504`	= "Government and Politics",
                      `507`	= "Horticulture and Forestry",
                      `510`	= "Manufacturing Technologies",
                      `512`	= "Medicine",
                      `513`	= "Dentistry",
                      `514`	= "Physical Education and Sport",
                      `516`	= "Property and Housing",
                      `518`	= "Retail and Wholesaling",
                      `519`	= "Science ",
                      `520`	= "Social Services",
                      `521`	= "Textiles and Apparel Manufacturing",
                      `522`	= "Transportation, Operations and Maintenance",
                      `523`	= "Warehousing and Distribution",
                      `524`	= "Other",
                      `526`	= "Recoded due to low counts - check SA")

btec = qualifications1 %>%  filter(gc_s_qual_btec == 1)
btec = qualifications1[qualifications1$mcsid %in% btec$mcsid,]
btec_subjects = btec %>%  select(mcsid, gcnum00, gc_rowid, gc_s_qual_btcn_rec,
                                 gc_l_btec_name_r30,gc_l_btlv, gc_l_btgd)
names(btec_subjects)<- c("mcsid", "cm_number", "row_number", "btec_number","subject_name", "level", "grade")
btec_subjects$subject_name = as.factor(btec_subjects$subject_name)
btec_subjects$subject_name =  recode(btec_subjects$subject_name, !!!btec_subject_key)
btec_subjects = btec_subjects %>%  filter(!is.na(subject_name)) #remove the NA subject name as due to long format, each cm has 20 rows and alot will be NA

#want Science and Applied science, where level = 3 in data (this is level 2)

#select science subjects
btec_science = btec_subjects %>% filter(subject_name == "Science " | subject_name ==  "Applied Science")
#select level == 3 (level 2 btec)
btec_science = btec_science %>% filter(level == 3)
btec_science = btec_science %>% select(mcsid, subject_name, level)

#make wide so will fit with other variables when creating binary variable.
wide_btec_science = btec_science %>%
  group_by(mcsid, subject_name) %>%
  tidyr::pivot_wider(names_from = subject_name, values_from = level) 
names(wide_btec_science) <- c("mcsid", "science_btec", "applied_science_btec")


#add in those who say have none of these subjects - for those who live in england, wales or NI (will do scotland separately)
#country = 1, 2 or 4. country !=3 (ie not scotland)
no_quals = qualifications1 %>% filter(gc_s_qual_none== 1) # be careful here as for rest of rows per cm, this will be NA.
no_quals = no_quals %>% select(mcsid, gcnum00, gc_rowid, gc_s_qual_none) #get relevant variables 
#add country 
no_quals = merge(no_quals, country, by = "mcsid")
no_quals = no_quals %>% filter(country !=3)
no_quals = no_quals %>% select(mcsid, gc_s_qual_none)
names(no_quals) = c("mcsid", "no_quals")

#btec only. this will = 1 only if all other quals reported are answeered no (2)
btec_only = qualifications1 %>% filter(gc_s_qual_btec== 1)
btec_only = btec_only %>% select(mcsid, gc_s_qual_btec, gc_s_qual_five, gc_s_qual_gcse, gc_s_qual_igcs)
btec_only = merge(btec_only, country, by = "mcsid")
btec_only = btec_only %>%  filter(country !=3)
btec_only = btec_only %>% mutate(btec_only1 = case_when((gc_s_qual_btec == 1) & (gc_s_qual_five == 1) | (gc_s_qual_gcse == 1)  |(gc_s_qual_igcs == 1) ~  2, (gc_s_qual_btec == 1) |(gc_s_qual_five == 2) | (gc_s_qual_gcse == 2) |(gc_s_qual_igcs == 2) ~ 1))
btec_only = btec_only %>% filter(btec_only1 == 1)
btec_only = btec_only %>% select(mcsid,btec_only1)


#combine core grades together into one dataframe (for GCSE and iGCSE)
combined_core_grades = merge(all=TRUE, core_subjects_grades, core_iGCSE_subjects_grades, by = "mcsid")
combined_core_grades = merge(all=TRUE, combined_core_grades, no_quals, by="mcsid")
combined_core_grades = merge(all=TRUE, combined_core_grades, btec_only, by="mcsid")
combined_core_grades = merge(all=TRUE, combined_core_grades, wide_btec_science, by = "mcsid")


#create binary variable. those who score >=4 on at least an english subject, at least one maths subject and at least 1 science subject = 1. else = 0. 
core_grades <- combined_core_grades %>%  mutate(benchmark = case_when(
  (english >= 4  | english_lang >= 4  | english_lit >= 4 | english_first_lang_I >= 4 |english_lit_I >= 4 ) & 
    (maths >= 4 | maths_linear >= 4 | maths_numeracy >= 4 | further_maths >= 4 | additional_maths >= 4 | maths_I >= 4) &
    (biology >= 4 | chemistry >= 4 | physics >= 4 | additional_science >= 4 | science >= 4 | applied_science >= 4 |
       combined_science >= 4 | modular_science >= 4 | further_additional_science >= 4 |computer_science >=4 |
     additional_applied_science >= 4 | additional_science_modular >= 4 |human_biology >= 4 | 
       biology_I >= 4 | chemistry_I >= 4 | physics_I >= 4 | science_I >= 4 | science_btec == 3 | applied_science_btec == 3) ~ 1,
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


n4_only = qualifications1 %>% filter(gc_s_qual_four == 1 & gc_s_qual_five == 2) #n4 = yes, n5 = no
#n4_only = n4_only %>% select(mcsid, gc_s_qual_four, gc_s_qual_five)
#n4_only = n4_only %>% filter(gc_s_qual_four == 1)
#n4_only = qualifications1[qualifications1$mcsid %in% n4_only$mcsid,] 
#n4_only = n4_only %>% mutate(national_4_only = case_when(gc_s_qual_four == 1 & 
                                           #                gc_s_qual_five == 2 ~ 1, 
                                                 #        TRUE ~ 0 ))

n4_only = n4_only %>% select(mcsid, gc_s_qual_four)


#add in those who say have none of these subjects - for those who live in Scotland)
#country = 3 
no_quals_scotland = qualifications1 %>% filter(gc_s_qual_none== 1) # be careful here as for rest of rows per cm, this will be NA.
no_quals_scotland = no_quals_scotland %>% select(mcsid, gcnum00, gc_rowid, gc_s_qual_none) #get relevant variables 
#add country 
no_quals_scotland = merge(no_quals_scotland, country, by = "mcsid")
no_quals_scotland = no_quals_scotland %>% filter(country ==3)
no_quals_scotland = no_quals_scotland %>% select(mcsid, gc_s_qual_none)
names(no_quals_scotland) = c("mcsid", "no_quals_scotland")

#btec only scotland. this will = 1 only if all other quals reported are answeered no (2)
btec_only_scotland = qualifications1 %>% filter(gc_s_qual_btec== 1)
btec_only_scotland  = btec_only_scotland  %>% select(mcsid, gc_s_qual_btec, gc_s_qual_five, gc_s_qual_gcse, gc_s_qual_igcs)
btec_only_scotland  = merge(btec_only_scotland , country, by = "mcsid")
btec_only_scotland  = btec_only_scotland  %>%  filter(country ==3)
btec_only_scotland  = btec_only_scotland  %>% mutate(btec_only1_scotland = case_when((gc_s_qual_btec == 1) & (gc_s_qual_five == 1) | (gc_s_qual_gcse == 1)  |(gc_s_qual_igcs == 1) ~  2, (gc_s_qual_btec == 1) |(gc_s_qual_five == 2) | (gc_s_qual_gcse == 2) |(gc_s_qual_igcs == 2) ~ 1))
btec_only_scotland  = btec_only_scotland  %>% filter(btec_only1_scotland == 1)
btec_only_scotland  = btec_only_scotland  %>% select(mcsid,btec_only1_scotland)

core_grades_scotland = merge(all = TRUE, core_n5_grades, n4_only, by = "mcsid")
core_grades_scotland = merge(all = TRUE, core_grades_scotland, no_quals_scotland, by = "mcsid")
core_grades_scotland = merge(all = TRUE, core_grades_scotland, btec_only_scotland, by = "mcsid")


core_grades_n5 <-  core_grades_scotland %>%  mutate(benchmarkN5 = case_when((english >= 3 )  & 
                                                                      (maths >= 3 )  &
                                                                      (computer_science >=3 |
                                                                         biology >= 3 | chemistry >= 3 | physics >= 3 ) ~ 1,
                                                                    TRUE ~ 0))

#combine N5 and GCSE core grades into one variable
#first combine the dataframes
#replace is/na with the other one 
core_grades_binary= merge (all=TRUE, core_grades, core_grades_n5, by="mcsid")
core_grades_binary = core_grades_binary %>% select(mcsid, benchmark, benchmarkN5)



core_grades_binary= core_grades_binary %>% mutate(benchmark_binary = 
                                                         case_when(benchmark == 0 & benchmarkN5 == 1 ~  1, 
                                                                   benchmark == 1 & benchmarkN5 == 1 ~ 1,
                                                                   benchmark == 0 & benchmarkN5 == 0 ~ 0,
                                                                   benchmark == 1 & benchmarkN5 == 0 ~ 1,
                                                                   is.na(benchmarkN5) ~ benchmark, 
                                                                   is.na(benchmark) ~ benchmarkN5, 
                                                  TRUE ~ 0)) %>% 
  select(mcsid, benchmark_binary)


benchmark_binary = table(core_grades_binary$benchmark_binary)
prop.table(benchmark_binary)*100

#CONTINOUS OUTCOME VARIABLE. ####
#merge together core subejcts for GCSE and iGCSE
core_subjects_continuous = merge(all=TRUE, core_subjects_grades, core_iGCSE_subjects_grades, by = "mcsid")
#create english data, maths data, science data. 
#english 
english_subjects_gcse = core_subjects_continuous %>% select(mcsid, contains("english")) %>% 
  mutate (english_score =
            rowMeans(.[-1], na.rm = TRUE), .after = 1) #calculate means across rows excluding row 1 (mcsid)
#convert NaN to NA
english_subjects_gcse$english_score[is.nan(english_subjects_gcse$english_score)]<-NA
#maths                                                                              
maths_subjects_gcse = core_subjects_continuous %>% select(mcsid, contains("math")) %>% 
  mutate (maths_score = 
            rowMeans(.[-1], na.rm = TRUE), .after = 1) #calculate means across rows excluding row 1 (mcsid)
#convert NaN to NA
maths_subjects_gcse$maths_score[is.nan(maths_subjects_gcse$maths_score)]<-NA

#science
science_subjects_gcse = core_subjects_continuous %>% select(mcsid, contains("biology"), contains("chemistry"), 
                                                            contains("physics"), contains("science")) %>% 
  mutate (science_score = 
            rowMeans(.[-1], na.rm = TRUE), .after = 1) #calculate means across rows excluding row 1 (mcsid)
#convert NaN to NA
science_subjects_gcse$science_score[is.nan(science_subjects_gcse$science_score)]<-NA

#pull out english score, maths score and science score, and mcsid

english_score = english_subjects_gcse %>% select(mcsid, english_score)
maths_score = maths_subjects_gcse %>% select(mcsid, maths_score)
science_score = science_subjects_gcse %>% select(mcsid, science_score)

#combine english, maths and science score together
core_subjects_score = merge(all=TRUE, english_score, maths_score, by = "mcsid")
core_subjects_score = merge(all=TRUE, core_subjects_score, science_score, by = "mcsid")

#get mean of these scores. need to have a response for english, maths and science score
core_subjects_score = core_subjects_score %>% mutate(average_grade = rowMeans(.[-1]), 
                                                           .after = 1) %>% 
  select(mcsid, average_grade)
core_subjects_score$average_grade = round(core_subjects_score$average_grade, 2)

#N5 CONTINOUS VARIABLE
n5_english_score = as.data.frame(core_n5_grades %>% select(mcsid, english))
n5_maths_score = as.data.frame(core_n5_grades %>% select(mcsid, maths))
n5_science_subjects = as.data.frame(core_n5_grades %>% select(mcsid, computer_science, biology, chemistry, physics)) %>% 
  mutate (science_score = rowMeans(.[-1], na.rm = TRUE), .after = 1)
#convert NaN to NA
n5_science_subjects$science_score[is.nan(n5_science_subjects$science_score)]<-NA
n5_science_subjects$science_score = round(n5_science_subjects$science_score, 2)
n5_science_score = n5_science_subjects %>% select(mcsid, science_score)

n5_core_subjects_score = merge(all=TRUE, n5_english_score, n5_maths_score, by="mcsid")
n5_core_subjects_score = merge(all= TRUE, n5_core_subjects_score, n5_science_score, by="mcsid")

#get mean of these scores. need to have a response for english, maths and science score
n5_core_subjects_score = n5_core_subjects_score %>% mutate(average_grade_n5 = rowMeans(.[-1]), 
                                                     .after = 1) %>% 
  select(mcsid, average_grade_n5)
n5_core_subjects_score$average_grade_n5 = round(n5_core_subjects_score$average_grade_n5, 2)

education_main_outcomes = merge(all=TRUE, core_grades_binary, core_subjects_score, by = "mcsid")
education_main_outcomes = merge(all=TRUE, education_main_outcomes, n5_core_subjects_score, by="mcsid")
  
#continous score version 2 - average of highest grade for each subject ####

english_subjects_highest = english_subjects_gcse %>% select(!english_score) %>% 
  mutate(highest_english = pmax(english,english_lang,english_lit,
  english_first_lang_I,english_lit_I, na.rm= TRUE), .after = 1) #get this to give the highest per row without having to name each column??


#merge into one variable
#core_grades_combined$core_grades_binary = ifelse(!is.na(core_grades_combined$benchmark), 
# core_grades_combined$benchmark, core_grades_combined$benchmarkN5)

#core_grades_combined = core_grades_combined %>% mutate(benchmark_binary = 
   #                                                      case_when(benchmark == 0 & benchmarkN5 == 1 ~  1, 
   #                                                                benchmark == 1 & benchmarkN5 == 1 ~ 1,
    #                                                               benchmark == 0 & benchmarkN5 == 0 ~ 0,
     #                                                              benchmark == 1 & benchmarkN5 == 0 ~ 1,
     #                                                              is.na(benchmarkN5) ~ benchmark, 
        #                                                           is.na(benchmark) ~ benchmarkN5))

#take those who sat the core subjects  - so binary variable here will be those who got 4 and above but only for those who took the subjects in the first place. 

#took_core_binary = taken_core_binary %>% filter(core_subjects_binary == 1)

#core_benchmark_binary = core_grades_combined[core_grades_combined$mcsid %in% took_core_binary$mcsid,]
#select relevant variables 
#core_benchmark_binary = core_benchmark_binary %>% select(mcsid, benchmark_binary)

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