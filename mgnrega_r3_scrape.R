library(RSelenium) 
library(gtools)
library(rvest)
library(dplyr)
library(data.table)
library(stringr)
library(wdman)
library(readxl)

####### Running a firefox session #########
driver<- rsDriver(port=4447L,browser=c("firefox"))
remDr <- driver[["client"]]
remDr$navigate("http://mnregaweb4.nic.in/netnrega/MISreport4.aspx") #navigating to the website
remDr$screenshot(display = TRUE)

####### Enter the correct CAPTCHA required at <here> as you see in the screenshot #######
captcha <- remDr$findElement(using = 'id', "ContentPlaceHolder1_txtCaptcha")
captcha$sendKeysToElement(list("32"))    #fill the number as per the screenshot

verifycode <- remDr$findElement(using = 'id', "ContentPlaceHolder1_btnLogin")
verifycode$clickElement()
remDr$screenshot(display = TRUE) ##Check if you have entered correct CAPTCHA

######## Selecting Financial Year

########need to change year
year <- remDr$findElement(using = 'xpath', "//select[@id='ContentPlaceHolder1_ddlfinyr']/option[@value='2021-2022']") #enter the year manually
year$clickElement()
remDr$screenshot(display = TRUE)

states <- remDr$findElement(using = 'xpath', "//select[@id='ContentPlaceHolder1_ddl_States']/option[@value='00']") #selecting All states
states$clickElement()

remDr$screenshot(display = TRUE) 

########## Selects the work R3 Employment status link

select.link <- remDr$findElement(using = "xpath", "//a[contains(@href,'demand_emp_demand.aspx')]")
select.link$clickElement()
remDr$screenshot(display = TRUE)

#switch from household level data to person level data, ignore this step if only household level data needed
hhs_to_person <- remDr$findElement(using = 'id', "ContentPlaceHolder1_RadioButtonList1_1")  
hhs_to_person$clickElement()
remDr$screenshot(display = TRUE) ##selecting person instead of households


webtable <- remDr$findElements(using = "css", "table")
len_t<-length(webtable)
data_html <- xml2::read_html(remDr$getPageSource()[[1]])
state_table <- html_table(html_nodes(data_html, "table")[[len_t]],trim=T,fill = T, header = T)  ### Extracting the table
state_name<-state_table[,2]
write.csv(state_table,"D:/state_hfi_21-22.csv", row.names=F)   #saving the state table

webElems <- remDr$findElements(using = "css", "[href]")
states_url<-unlist(sapply(webElems, function(x) {x$getElementAttribute("href")}))  #storing the url of the states
len_s<-length(states_url)-1                          #used in for-loop

#for loop [Note: this is till district level, you can extract till block level by repeating script for block level - making it a nested loop(refer the other script to understand)]
for (i in 4:len_s) {                                # 4 - because i am ignoring initial 3 irrelevant rows
  remDr$navigate(states_url[i])                     #for navigating to districts within each state
  
  webtable2 <- remDr$findElements(using = "css", "table")
  len_t2<-length(webtable2)
  data_html <- xml2::read_html(remDr$getPageSource()[[1]])
  dist_table <- html_table(html_nodes(data_html, "table")[[len_t2]],trim=T,fill = T, header = T)  ### Extracting the table
  dist_name<-dist_table[,2]
  

  dff<-dist_table
  a1 <- gsub("/","_", as.character(state_name[i,]))
  dff$State <-as.character(state_name[i,])
  file1<-paste0("D:/sample_dist/",a1,"_district",".csv")
  write.csv(dff,file1,row.names = F)                        #saving district level tables
  
  
  #remDr$goBack()
}

