library(RSelenium) 
library(gtools)
library(rvest)
library(dplyr)
library(data.table)
library(stringr)
library(wdman)

####### Running a firefox session #########
driver<- rsDriver(port=4444L,browser=c("firefox"))
remDr <- driver[["client"]]
remDr$navigate("http://mnregaweb4.nic.in/netnrega/MISreport4.aspx") #navigating to the website
remDr$screenshot(display = TRUE)

####### Enter the correct CAPTCHA required at <here> as you see in the screenshot #######
captcha <- remDr$findElement(using = 'id', "ContentPlaceHolder1_txtCaptcha")
captcha$sendKeysToElement(list("4"))  #enter captcha manually

verifycode <- remDr$findElement(using = 'id', "ContentPlaceHolder1_btnLogin")
verifycode$clickElement()
remDr$screenshot(display = TRUE) ##Check if you have entered correct CAPTCHA

######## Selecting Financial Year

########need to change year
year <- remDr$findElement(using = 'xpath', "//select[@id='ContentPlaceHolder1_ddlfinyr']/option[@value='2020-2021']") #enter the year manually
year$clickElement()
remDr$screenshot(display = TRUE)

states <- remDr$findElement(using = 'xpath', "//select[@id='ContentPlaceHolder1_ddl_States']/option[@value='00']")  #selecting All states
states$clickElement()

remDr$screenshot(display = TRUE) 

########## Selects the SC/ST Employment status link

select.link <- remDr$findElement(using = "xpath", "//a[contains(@href,'empstatusnewall_scst.aspx')]") 
select.link$clickElement()
remDr$screenshot(display = TRUE)

webtable <- remDr$findElements(using = "css", "table")  
len_t<-length(webtable)
data_html <- xml2::read_html(remDr$getPageSource()[[1]])
state_table <- html_table(html_nodes(data_html, "table")[[len_t]],trim=T,fill = T, header = T)  ### Extracting the state table
state_name<-state_table[,2] #storing the state names
write.csv(state_table,"D:/state_nrega_Sc_st.csv", row.names=F)  #saving the state table

webElems <- remDr$findElements(using = "css", "[href]")
states_url<-unlist(sapply(webElems, function(x) {x$getElementAttribute("href")})) #storing the url of the states
len_s<-length(states_url)-1

#nested for loop
for (i in 4:len_s) {                             # 4 - because i am ignoring initial 3 irrelevant rows 
  remDr$navigate(states_url[i])                  #for navigating to districts within each state
  
  webtable2 <- remDr$findElements(using = "css", "table")  
  len_t2<-length(webtable2)
  data_html <- xml2::read_html(remDr$getPageSource()[[1]])
  dist_table <- html_table(html_nodes(data_html, "table")[[len_t2]],trim=T,fill = T, header = T)  ### Extracting the district table
  dist_name<-dist_table[,2]
  
  webElems2 <- remDr$findElements(using = "css", "[href]")
  dist_url<-unlist(sapply(webElems2, function(x) {x$getElementAttribute("href")})) #storing the url of the districts
  len_d<-length(dist_url)-1
  
  dff<-dist_table
  a1 <- gsub("/","_", as.character(state_name[i,]))
  dff$State <-as.character(state_name[i,])
  file1<-paste0("D:/sample_dist/",a1,"_district",".csv") #saving district level tables
  write.csv(dff,file1,row.names = F)
  

for (j in 4:len_d) {
  remDr$navigate(dist_url[j])   #navigating to blocks within each district
  
  webtable3 <- remDr$findElements(using = "css", "table")
  len_t3<-length(webtable3)
  data_html <- xml2::read_html(remDr$getPageSource()[[1]])
  block_table <- html_table(html_nodes(data_html, "table")[[len_t3]],trim=T,fill = T, header = T)  ### Extracting the block table
  block_name<-block_table[,2]
  
  webElems3 <- remDr$findElements(using = "css", "[href]")
  block_url<-unlist(sapply(webElems3, function(x) {x$getElementAttribute("href")}))
  len_b<-length(block_url)-1
  
  df<-block_table
  b <- gsub("/","_", as.character(dist_name[j,]))
  df$District<-as.character(dist_name[j,])
  a <- gsub("/","_", as.character(state_name[i,]))
  df$State <-as.character(state_name[i,])
  file<-paste0("D:/sample/",a,"_",b,"_block",".csv")  
  write.csv(df,file,row.names = F)                    #saving block level tables
  
  remDr$goBack()
}
  remDr$goBack()
}
