# set the working directory and load the required packages into the script
start.time <- Sys.time()
directory_path <- "C:/Users/Gourab.Chanda/Desktop/vinay_req/Vinay_Generalised_Code/Vinay_Generalised_Code/Code_Optimisation_text_Analytics/Test_Run/ML SAMPLES 2"
setwd(directory_path)
packgs_required <- c("pdftools",
                     "tesseract",
                     "magick",
                     "magrittr",
                     "readxl",
                     "stringr",
                     "dplyr",
                     "purrr",
                     "writexl")
extra_pkgs <- setdiff(packgs_required, rownames(installed.packages()))

if(length(extra_pkgs)>0){
  install.packages(extra_pkgs, dependencies = T)
}

invisible(sapply(packgs_required,library,character.only=T))

######OCR-Script for the document validation############# 

#create the vector for all the pdf documents path and store it in pdf_path variable
pdfs_path <- paste(directory_path, "/", list.files(directory_path, pattern = "*.pdf|*.PDF"), sep = "")

#get the list of all pdf files in the given folder 
pdf_names <- list.files(directory_path, pattern = "*.pdf|.*PDF")

#get the informatioin about the pdf files in the given directory
pdf_info <- purrr::map(pdfs_path,pdftools::pdf_info)

#get the page count for each pdf files in the given directory
page_count <- sapply(pdf_info,"[[",2)

#create the data frame with the pdf names , pdf paths and the page count 

pdf_details <- data.frame(pdf_name=character(),
                          pdf_path=character(),
                          page_count=integer())

pdf_details <- as.data.frame(cbind(pdf_names,page_count,pdfs_path))

#segregate the pdf files which has number of pages>1 

pdf_multiple_pages <- pdf_details[which(pdf_details$page_count!=1),]
pdf_multiple_pages$text <- purrr::map(as.vector(pdf_multiple_pages$pdfs_path),pdftools::pdf_text)
pdf_multiple_pages[] <- lapply(pdf_multiple_pages, function(x) as.character(gsub("[\\,]", "", x)))
pdf_multiple_pages[] <- lapply(pdf_multiple_pages, function(x) as.character(gsub("\\bnr\\b","", x)))


colnames(pdf_multiple_pages) <- c("File_name","page_count","File_path","text")
pdf_pages <- pdf_multiple_pages%>%
  select(c(File_name,text))

pdf_single_pages <- pdf_details[which(pdf_details$page_count==1),]

purrr::map(as.vector(pdf_single_pages$pdfs_path),pdftools::pdf_convert)

#convert the pdfs into images
#pdfs_images <- purrr::map(pdfs_path, pdftools::pdf_convert)
#create the vector for all the image (png) documents path and store it in image_path variable
image_path <- paste(directory_path, "/", list.files(directory_path, pattern = "*.png"), sep = "")
#get the list of all image files in a given folder
image_names <- list.files(directory_path, pattern = "*.png")
image_names <- gsub(".png","",image_names)
#extract the text from the images using OCR function 
purrr::map(image_path, ~{
  curr_fil <- .x
  image_read(curr_fil) %>%
    image_resize("2500") %>% 
    image_trim() %>%
    image_ocr(language = c("eng","deu","fra"))
}) -> ocr_result
#create the dataframe and remove the "\n" stopword from the text
#text_df <- data_frame(File_name=pdf_names[1:length(pdf_names)],text=ocr_result)
image_df <- data_frame(File_name=image_names[1:length(image_names)],text=ocr_result)
#text_df$text <- gsub("\n","",text_df$text)
image_df$text <- gsub('[\r\t\n]',"",image_df$text)
pdf_pages$File_name <- gsub(".pdf","",pdf_pages$File_name)
pdf_pages$File_name <- sub("\\.","",pdf_pages$File_name)

files_text <- rbind(image_df,pdf_pages)
files_text$File_name <- gsub("_[1-9]","",files_text$File_name)
writexl::write_xlsx(x = files_text,path = paste0(directory_path,"/files_text.xlsx"))
#read the demo master file from the given directory and create a data frame for the same 
demo_master <- read_xlsx("C:/Users/Gourab.Chanda/Desktop/vinay_req/Vinay_Generalised_Code/Vinay_Generalised_Code/Code_Optimisation_text_Analytics/Test_Run/ML SAMPLES 2/Samples.xlsx",sheet = 1)
input.data <- demo_master
demo_master <- as.data.frame(demo_master)
input.cols <- colnames(demo_master)
transpose_dm <- reshape::melt.data.frame(data = demo_master,id.vars = c(1:37),measure.vars = c(38:ncol(demo_master)))
transpose_dm <- transpose_dm%>%
  dplyr::rename(Document_Class=variable,File_name=value)

#write_xlsx(transpose_dm,"C:/Users/Gourab.Chanda/Desktop/vinay_req/Vinay_Generalised_Code/Vinay_Generalised_Code/Code_Optimisation_text_Analytics/Sample.xlsx")

demo_master <- transpose_dm

demo_master$File_name <- as.character(demo_master$File_name)
#perform the join operation to add the text column from the for the corrosponding documents and vendor
#joined_demo_master <- left_join(demo_master,text_df)
joined_demo_master <- left_join(demo_master,files_text)

joined_demo_master <- joined_demo_master[,c(1:37,39:40)]
#take the backup of the joined_demo_master df into back_up data frame
back_up <- joined_demo_master
writexl::write_xlsx(back_up,"C:/Users/Gourab.Chanda/Desktop/vinay_req/Vinay_Generalised_Code/Vinay_Generalised_Code/Code_Optimisation_text_Analytics/joined_ds.xlsx")
#create a function which will do the validation of the text for each observation in the text column
validator <- function(x,y){
  if(grepl(x,y,fixed=T)&!is.na(x)){
    check <- 1
  } else{
    check <- 0
  }
  check
}
#get the list of columns from the joined_demo_master data frame
varlist <- colnames(joined_demo_master)
req_length <- length(varlist)-1
#run a loop for each observations in the joined_demo_master file


for(i in 2:req_length){
  code<-paste0("joined_demo_master<- joined_demo_master%>% mutate(flag_",varlist[i],"=purrr::map2(",varlist[i],",text,validator))%>%
               mutate(flag_",varlist[i],"=list(flag_",varlist[i],")%>% unlist())",sep="")
  eval(parse(text=code))
}


#fetch the updated column names in the joined demo master and create a new column named Column_Avail 
varlist<-colnames(joined_demo_master)
flag_var<-varlist[grep("flag",varlist)]
joined_demo_master$Column_Avail<-""

#create a loop to fetch the flags which has value of 1 and group the column names for the same 

for(j in 1:nrow(joined_demo_master)){
  column_avail<-c()
  for(i in flag_var){
    if(joined_demo_master[j,i]==1)
      column_avail<-c(i,column_avail)
  }
  if(length(column_avail>0))joined_demo_master$Column_Avail[j]<-paste(column_avail,collapse=", ")
}

#create a data frame to fetch the final output 
final_demo_master <- joined_demo_master%>%
  select(`Vendor_Request:Vendor_Request`:text,Column_Avail)

final_demo_master$Column_Avail <- gsub("flag_","",final_demo_master$Column_Avail) 

x <- final_demo_master

x<-x[order(x$`Vendor_Request:Vendor_Request`),]
y<-data.frame(Vendor_Request=unique(x$`Vendor_Request:Vendor_Request`))
y$Column_Avail=""

for(i in 1:nrow(y)){
  VR<-as.character(y$Vendor_Request[i])
  Columns=x[which(x$`Vendor_Request:Vendor_Request`==VR),'Column_Avail']
  Columns<-unique(unlist(strsplit(as.character(Columns),",",fixed=T)))
  if(length(Columns)>0) y$Column_Avail[i]=paste(Columns,collapse=",")
}



#colnames(y) <- c("Vendor Request: Vendor Request","Column_Avail")

y$col_unavailable<-""
y$Column_Avail <- as.character(y$Column_Avail)
for (i in 1:nrow(y)) {
  col.split <- strsplit(y$Column_Avail[i],",")[[1]]
  col_unavailable <- input.cols[!input.cols %in% col.split]
  col_unavailable <- paste(col_unavailable,collapse = ",")
  y$col_unavailable[i] <- col_unavailable
}

output.data <- y

colnames(output.data)[colnames(output.data)=="Vendor_Request"] <- "Vendor_Request:Vendor_Request"


output.data1 <- merge(input.data,output.data,by="Vendor_Request:Vendor_Request")

write_xlsx(x = output.data1,path = paste0(directory_path,"/output_final.xlsx"))


