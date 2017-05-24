library(RCurl)
library(tidyverse)
library(stringr)

# download report pdf's ####

# get content of sdp capstone report webpage
sdp_url <- getURL("https://sdp.cepr.harvard.edu/fellowship-capstone-reports")
sdp_webpage <- readLines(tc <- textConnection(sdp_url)); close(tc)

# create df of webpage content
sdp_df <- tibble(line = 1:360, content = sdp_webpage)

# find and clean links to report pdf's
capstone_links <- sdp_df %>%
  mutate(link_present = str_detect(content, "https://sdp.cepr.harvard.edu/files/cepr-sdp/files/")) %>%
  filter(link_present == TRUE) %>%
  mutate(clean_link = str_extract(content, "https://sdp.cepr.harvard.edu/files/cepr-sdp/files/.+\\.pdf"))

for(i in seq_along(1:length(capstone_links$clean_link))){
  
  report_url <- capstone_links$clean_link[i]
  
  download.file(report_url, str_c("reports/capstone", i,".pdf"))
  
}
