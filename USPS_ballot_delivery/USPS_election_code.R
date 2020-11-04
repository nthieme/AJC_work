###Code for scraping USPS delivery scores from PACER PDFs
#this uses tabulizer to scrape the infomation and then organizes it a little bit

library(tabulizer)
library(tidyverse)
library(ggpubr)

# extract table from first page of example PDF. 
# This section here repairs the names because when there the name of the district ends up on separate
# lines, tabulizer puts the names in different rows of the data frame. Repeat this code for each table

tab <- extract_tables("table_from_pleading.pdf")
rb_2<-do.call("rbind", tab)
rb_2_t<-rb_2
rb_2_new<-rb_2

for(i in 1:nrow(rb_2)){
  
  if(rb_2_t[i,1]!=""){
    next
  }else{
    if(rb_2_t[i,3]!=""){
      missing_row <- rb_2_t[i,]
      row_to_add_to <- rb_2_t[i+1,]
      
      if(missing_row[3]=="Colorado/Wyo"){
        name<-str_c(missing_row[3],row_to_add_to[3])
        row_to_add_to[3]<-name
      }else{
        name<-str_c(missing_row[3]," ",row_to_add_to[3])
        row_to_add_to[3]<-name
        rb_2_t[i+1,3]<-name
      }
      
      rb_2_new[i+1,3]<-row_to_add_to[3]
    }
  }
  
}

rb_2_new_f<-rb_2_new %>% data.frame 

rb_2_new_f_2<-rb_2_new_f %>% mutate(X2 = str_split(X1 %>% as.character(), "2020") %>% 
                                      lapply(function(x)return(x[2])),
                                    X1 = str_split(X1 %>% as.character(), "2020") %>% 
                        lapply(function(x)return(x[1])) %>% unlist %>% trimws,
                      )

name_l <-c("Date", "Area","District","Inbound","Outbound","Outbound_non" )

names(rb_2_new_f_2)<-name_l

rb_2_new_f_3<-rb_2_new_f_2 %>% filter(Date%in%c("","Date","Date Area")==FALSE) %>% 
  mutate(Date = str_c(Date,"2020") %>% mdy)%>% mutate_all(unlist)

write_csv(rb_2_new_f_3, "rb_2_new_f_2.csv")

##
D_1<-read_csv("rb_1_new_f_2.csv")

D_1<-D_1 %>% mutate(Date = mdy(Date), 
               First_class = str_remove(First_class, "%") %>% as.numeric,
               Marketing_mail = str_remove(Marketing_mail, "%") %>% as.numeric)

D_1 %>% write_csv("rb_1_new_f_3.csv")

##
D_2<-read_csv("rb_2_new_f_2.csv")

D_2<-D_2 %>% mutate( 
               Inbound = str_remove(Inbound, "%") %>% as.numeric,
               Outbound = str_remove(Outbound, "%") %>% as.numeric,
               Outbound_non = str_remove(Outbound_non, "%") %>% as.numeric
               )

D_2 %>% write_csv("rb_2_new_f_3.csv")

##
D_3<-read_csv("rb_3_new_f_2.csv")

D_3<-D_3 %>% mutate(Date = mdy(Date), 
               Score_inb_ball = str_remove(Score_inb_ball, "%") %>% as.numeric,
               Score_p_1_inb_ball = str_remove(Score_p_1_inb_ball, "%") %>% as.numeric,
               Score_p_2_inb_ball = str_remove(Score_p_2_inb_ball, "%") %>% as.numeric,
               Score_p_3_inb_ball = str_remove(Score_p_3_inb_ball, "%") %>% as.numeric
)

D_3 %>% write_csv("rb_3_new_f_3.csv")
