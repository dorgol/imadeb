library(rvest)
library(stringr)
library(ggplot2)
years<-c(1970:2017)
for (i in years) {
url<-paste("http://www.imdb.com/search/title?count=100&release_date=",i,",",i,"&title_type=featurel", sep = "")
webpage <- read_html(url)
rank_data_html <- html_nodes(webpage,'.text-primary')
rank_data <- html_text(rank_data_html)
gsub("\n","",rank_data)->rank_data
gsub("\\.","",rank_data)->rank_data
as.numeric(rank_data)->rank_data
name_data_html<-html_nodes(webpage, '.lister-item-header a')
name_data<-html_text(name_data_html)
rating_data_html<-html_nodes(webpage,'.ratings-imdb-rating')
rating_data<-html_text(rating_data_html)
as.numeric(rating_data)->rating_data
gsub("\n","",rating_data)->rating_data
runtime_data_html<-html_nodes(webpage,'.text-muted .runtime')
runtime_data<-html_text(runtime_data_html)
gsub("min","",runtime_data)->runtime_data
as.numeric(runtime_data)->runtime_data
genre_data_html<-html_nodes(webpage,'.genre')
genre_data<-html_text(genre_data_html)
gsub("\n","",genre_data)->genre_data
str_split_fixed(genre_data,",",3)->splitted_genre
votes_data_html<-html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
votes_data<-html_text(votes_data_html)
gsub(",","",votes_data)->votes_data
as.numeric(votes_data)->votes_data
rated_data_html<-html_nodes(webpage,'.certificate')
rated_data<-html_text(rated_data_html)
stars_data_html<-html_nodes(webpage,'.text-muted+ p')
stars_data<-html_text(stars_data_html)
gsub("\n","",stars_data)->stars_data
str_split_fixed(stars_data,",",10)->splitted_stars
str_match(stars_data,"Director[s]?:(.*)\\|")[,2]->directors
str_split_fixed(directors,",",4)->splitted_directors
str_match(stars_data,"Stars:(.*)")[,2]->Actors
str_split_fixed(Actors,",",4)->splitted_Actors
year<-rep(i,100)
cbind(rank_data,name_data,year,rating_data,votes_data,runtime_data
      ,splitted_directors,splitted_Actors)->temporary
as.data.frame(temporary)->temporary
paste(i,"data",sep = "_")->name
names_vector<-c("rank_data","name_data","year","rating_data","votes_data",
                "runtime_data","director_1","director_2"
                ,"director_3","director_4","actor_1"
                ,"actor_2","actor_3","actor_4")
names(temporary)<-names_vector
assign(name, temporary)
}       
dfs <- sapply(.GlobalEnv, is.data.frame) 
do.call(rbind, mget(names(dfs)[dfs]))->gross_data
rm(list=setdiff(ls(),"gross_data"))

