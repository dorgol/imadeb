library(xml2)
library(rvest)
library(RCurl)
library(XML)
library(plyr)
library(qpcR)
library(stringr)
{url<-"https://www.imdb.com/search/title?release_date=2017-01-01,2017-12-31"
url2<-getURL(url)
parsed<-htmlParse(url2)
link<-xpathSApply(parsed,path = "//a",xmlGetAttr,"href")
head(link)
as.data.frame(link)->link
subset(link,grepl("^/title/tt.*?ref_=adv_li_tt",link))->y

for (url in y) {
  paste("https://www.imdb.com",url,sep = "")->hrefs
  cbind(urls,hrefs)->urls
}
as.data.frame(hrefs)->hrefs
rm(list=setdiff(ls(), "hrefs"))}
 
numbers<-c(1:15)
Actors_names<-data.frame((NULL),stringsAsFactors = TRUE)
for (number in numbers) {
  paste("Actor",number,sep = "_")->Actor_number
  data.frame(Actor_number)->Actor_number
  rbind(Actors_names,Actor_number)->Actors_names
  
}

aviya<-function(site){
  
  name_data_html<-html_nodes(read_html(as.character(site)),
                             '#ratingWidget strong')
  name_data<-html_text(name_data_html)
  
  runtime_data_html<-html_nodes(read_html(as.character(site)),
                                '.txt-block:nth-child(23)')
  runtime_data<-html_text(runtime_data_html)
  gsub("Runtime:","",runtime_data)->runtime_data
  gsub("min","",runtime_data)->runtime_data
  
  rating_data_html<-html_nodes(read_html(as.character(site)),
                               '.ratingValue')
  rating_data<-html_text(rating_data_html)
  gsub("/10","",rating_data)->rating_data
  as.numeric(rating_data)->rating_data
  
  metascore_data_html<-html_nodes(read_html(as.character(site)),
                                  '.score_favorable')
  metascore_data<-html_text(metascore_data_html)
  
  director_data_html<-html_nodes(read_html(as.character(site)),
                                 '.summary_text+ .credit_summary_item')
  director_data<-html_text(director_data_html)
  gsub("Director[s]?:","",director_data)->director_data
  
  writer_data_html<-html_nodes(read_html(as.character(site)),
                               '.credit_summary_item:nth-child(3)')
  writer_data<-html_text(writer_data_html)
  gsub("Writer[s]?:","",writer_data)->writer_data
  #str_split_fixed(writer_data,",",str_count(writer_data,",")-1)->writer_data
  
  cast_data_html<-html_nodes(read_html(as.character(site)),
                             '#titleCast .itemprop')
  cast_data<-html_text(cast_data_html)
  
  even_indexes<-seq(2,length(cast_data),2)
  
  cast_data<-cast_data[even_indexes]
  
  genre_data_html<-html_nodes(read_html(as.character(site)),
                              '.see-more.canwrap~ .canwrap a')
  genre_data<-html_text(genre_data_html)
  
  voters_data_html<-html_nodes(read_html(as.character(site)),
                               '.small')
  voters_data<-html_text(voters_data_html)
  
  #voters_data[[3]]->voters_data
  
  gross_worldwid_data_html<-html_nodes(read_html(as.character(site)),
                                       '.txt-block:nth-child(14)')
  gross_worldwide_data<-html_text(gross_worldwid_data_html)
  #gsub('\\D+','', gross_worldwide_data)->gross_worldwide_data
  
  gross_USA_data_html<-html_nodes(read_html(as.character(site)),
                                  '#titleDetails .txt-block:nth-child(13)')
  gross_USA_data<-html_text(gross_USA_data_html)
  
  opening_week_data_html<-html_nodes(read_html(as.character(site)),
                                     '#titleDetails .txt-block:nth-child(12)')
  opening_week_data<-html_text(opening_week_data_html)
  #opening_week_data[[2]]->opening_week_data
  
  budget_data_html<-html_nodes(read_html(as.character(site)),
                               '.txt-block:nth-child(12)')
  budget_data<-html_text(budget_data_html)
  #budget_data[[2]]->budget_data
  #gsub('\\D+','', budget_data)->budget_data
  
  country_data_html<-html_nodes(read_html(as.character(site)),
                                '.txt-block:nth-child(4)')
  country_data<-html_text(country_data_html)
  gsub("Country:","",country_data)->country_data
  
  ratio_data_html<-html_nodes(read_html(as.character(site)),
                              '.txt-block:nth-child(25)')
  ratio_data<-html_text(ratio_data_html)
  gsub("Aspect Ratio: ","",ratio_data)->ratio_data
  
  color_data_html<-html_nodes(read_html(as.character(site)),
                              '.txt-block:nth-child(24)')
  color_data<-html_text(color_data_html)
  gsub("Color:","",color_data)->color_data
  
  sound_mix_data_html<-html_nodes(read_html(as.character(site)),
                                  '.txt-block:nth-child(23)')
  sound_mix_data<-html_text(sound_mix_data_html)
  gsub("Sound Mix:","",sound_mix_data)->sound_mix_data
  #str_split_fixed(sound_mix_data,",",str_count(sound_mix_data,"|")-1)->sound_mix_data
  
  producers_data_html<-html_nodes(read_html(as.character(site)),
                                  '.txt-block:nth-child(18)')
  producers_data<-html_text(producers_data_html)
  gsub("Productio Co:","",producers_data)
  
  location_data_html<-html_nodes(read_html(as.character(site)),
                                 '.txt-block:nth-child(8)')
  location_data<-html_text(location_data_html)
  #location_data[[2]]->location_data
  gsub("Filming Locations:","",location_data)->location_data
  
  release_date_data_html<-html_nodes(read_html(as.character(site)),
                                     '.txt-block:nth-child(6)')
  release_date_data<-html_text(release_date_data_html)
  
  
  qpcR:::cbind.na(name_data,runtime_data,rating_data,metascore_data,
                  director_data,writer_data,
                  (t(cast_data)),t(genre_data),voters_data,
                  gross_worldwide_data,gross_USA_data,
                  opening_week_data,budget_data,country_data,
                  ratio_data,color_data,sound_mix_data,
                  producers_data,
                  location_data,
                  release_date_data)->x
  
  as.data.frame(x)->x
}


