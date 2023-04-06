### Przetwarzanie Danych Ustrukturyzowanych 2023L
### Praca domowa nr. 3
###
### UWAGA:
### nazwy funkcji oraz ich parametrow powinny pozostac niezmienione.
###  
### Wskazane fragmenty kodu przed wyslaniem rozwiazania powinny zostac 
### zakomentowane
###

# -----------------------------------------------------------------------------#
# Wczytanie danych oraz pakietow.
# !!! Przed wyslaniem zakomentuj ten fragment
# -----------------------------------------------------------------------------#




# -----------------------------------------------------------------------------#
# Zadanie 1
# -----------------------------------------------------------------------------#

sql_1 <- function(Users){
  sqldf("SELECT Location, SUM(UpVotes) as TotalUpVotes
FROM Users
WHERE Location != ''                                                   
GROUP BY Location
ORDER BY TotalUpVotes DESC
LIMIT 10")
}

base_1 <- function(Users){
    Users_without_empty_location <- Users[Users$Location!='',]                                          #wybieramy wiersze z niepustą lokalizacją
    upvotes_by_location <- aggregate(UpVotes ~ Location, data= Users_without_empty_location, FUN=sum)   # grupujemy po lokalizacji i zliczamy upvote'y
    upvotes_by_location_sorted_desc <- upvotes_by_location[order(-upvotes_by_location$UpVotes),]        # sortujemy malejąco po upvotach
    names(upvotes_by_location_sorted_desc) <- c('Location', 'TotalUpVotes')                             # zmieniamy nazwę kolumny na porządaną
    upvotes_by_location_top10 <- upvotes_by_location_sorted_desc[1:10,]                                 # wybieramy pierwsze 10 wierszy z posortowanej już ramki
}

dplyr_1 <- function(Users){
    Users %>%
    filter(Location!='')%>%                    # wybieramy wiersze z niepustą lokalizacją
    group_by(Location)%>%                      # grupujemy po lokalizacji
    summarise(TotalUpVotes=sum(UpVotes))%>%    # zliczamy upvote'y i nazywamy kolumnę odpowiednio
    arrange(-TotalUpVotes)%>%                  # sortujemy malejąco po upvotach
    slice(1:10) -> wyn                         # wybieramy pierwsze 10 wierszy z posortowanej już ramki
}

table_1 <- function(Users){
   wyn <- setorder(as.data.table(Users)[Location!='', .(TotalUpVotes=sum(UpVotes)), by=Location], -TotalUpVotes)[1:10] #wszystko w jednej linii, bo mogę
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
#all_equal(SQLDF1,BASE1)
#all_equal(SQLDF1,DPLYR1)
#all_equal(SQLDF1,TABLE1)
# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
#microbenchmark(sql_1(Users), base_1(Users), dplyr_1(Users), table_1(Users))

# -----------------------------------------------------------------------------#
# Zadanie 2
# -----------------------------------------------------------------------------#

sql_2 <- function(Posts){
    sqldf("SELECT STRFTIME('%Y', CreationDate) AS Year, STRFTIME('%m', CreationDate) AS Month,
COUNT(*) AS PostsNumber, MAX(Score) AS MaxScore
FROM Posts
WHERE PostTypeId IN (1, 2)
GROUP BY Year, Month
HAVING PostsNumber > 1000")
}

base_2 <- function(Posts){
    posts_with_correct_id <- Posts[Posts$PostTypeId<3,]
    posts_with_correct_id$Year <- strftime(as.Date(posts_with_correct_id$CreationDate), format = "%Y")
    posts_with_correct_id$Month <- strftime(as.Date(posts_with_correct_id$CreationDate), format = "%m")
    posts_number_counted <- aggregate(Id~Year+Month, data=posts_with_correct_id, FUN=function(x) {length(x)})
    posts_max_cal <- aggregate(Score~Year+Month, data=posts_with_correct_id, FUN=max)
    names(posts_number_counted)[3]="PostsNumber"
    posts_number_counted$MaxScore <- posts_max_cal$Score
    result <- posts_number_counted[posts_number_counted$PostsNumber>1000,]
    result <- result[order(result$Year),]
}

dplyr_2 <- function(Posts){
    Posts %>%
    filter(PostTypeId<3)%>%
    mutate(Year = format(as.Date(CreationDate), "%Y"), Month = format(as.Date(CreationDate), "%m"))%>%
    group_by(Year,Month)%>%
    summarise(PostsNumber =n(), MaxScore=max(Score))%>%
    filter(PostsNumber>1000) -> costam
}

table_2 <- function(Posts){
   as.data.table(Posts)[PostTypeId<3,.(CreationDate, Id, Score)] -> chlop_pofiltrowany_odroczony
   chlop_pofiltrowany_odroczony[, Year := format(as.Date(CreationDate), "%Y")] -> nowy_chlop_pofiltrowany_odroczony
   nowszy_chlop <- nowy_chlop_pofiltrowany_odroczony[, Month :=format(as.Date(CreationDate), "%m")]
   agregowany_chlop <- nowszy_chlop[,.(PostsNumber = .N, MaxScore = max(Score)), by=.(Year,Month)]
   wyn<- agregowany_chlop[PostsNumber>1000]
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
#all_equal(SQLDF2,BASE2)
#all_equal(SQLDF2,DPLYR2)
#all_equal(TABLE2, SQLDF2)
# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
#microbenchmark(sql_2(Posts), base_2(Posts), dplyr_2(Posts), table_2(Posts))

# -----------------------------------------------------------------------------#
# Zadanie 3
# -----------------------------------------------------------------------------#

sql_3 <- function(Posts, Users){
    sqldf("SELECT Id, DisplayName, TotalViews
FROM (
SELECT OwnerUserId, SUM(ViewCount) as TotalViews
FROM Posts
WHERE PostTypeId = 1
GROUP BY OwnerUserId
) AS Questions
JOIN Users
ON Users.Id = Questions.OwnerUserId
ORDER BY TotalViews DESC
LIMIT 10")
}

base_3 <- function(Posts, Users){
   post_filter <- Posts[Posts$PostTypeId==1,]
   Questions <- aggregate(ViewCount~OwnerUserId, data=post_filter, FUN=sum)
   names(Questions)[2] <- "TotalViews"
   Questions <- merge(Questions, Users, by.x="OwnerUserId", by.y="Id", all.x=TRUE)
   Questions <- Questions[order(-Questions$TotalViews), c("OwnerUserId", "DisplayName", "TotalViews")]
   names(Questions)[1] <- "Id"
   Questions[1:10, ]
}

dplyr_3 <- function(Posts, Users){
   Posts %>%
   filter(PostTypeId==1) %>%
   group_by(OwnerUserId) %>%
    summarise(TotalViews=sum(ViewCount)) %>%
    inner_join(Users, by=c("OwnerUserId"="Id")) %>%
    select(Id=OwnerUserId, DisplayName, TotalViews) %>%
    arrange(-TotalViews) %>%
    slice(1:10) 
}

table_3 <- function(Posts, Users){
    Questions <- as.data.table(Posts)[PostTypeId==1, .(TotalViews=sum(ViewCount)), by=OwnerUserId][!is.na(OwnerUserId)]
    Questions <- as.data.table(Users)[Questions, on = c(Id = "OwnerUserId")]
    setorder(Questions[, .(Id, DisplayName, TotalViews)], -TotalViews)[1:10]
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
#all_equal(SQLDF3, BASE3)
#all_equal(DPLYR3, BASE3)
#all_equal(DPLYR3, TABLE3)
# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
microbenchmark(sql_3(Posts,Users), base_3(Posts,Users), dplyr_3(Posts,Users), table_3(Posts, Users))

# -----------------------------------------------------------------------------#
# Zadanie  4
# -----------------------------------------------------------------------------#

sql_4 <- function(Posts, Users){
    sqldf("SELECT DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes
FROM (
SELECT *
FROM (
SELECT COUNT(*) as AnswersNumber, OwnerUserId
FROM Posts
WHERE PostTypeId = 2
GROUP BY OwnerUserId
) AS Answers
JOIN
(
SELECT COUNT(*) as QuestionsNumber, OwnerUserId
FROM Posts
WHERE PostTypeId = 1
GROUP BY OwnerUserId
) AS Questions
ON Answers.OwnerUserId = Questions.OwnerUserId
WHERE AnswersNumber > QuestionsNumber
ORDER BY AnswersNumber DESC
LIMIT 5
) AS PostsCounts
JOIN Users
ON PostsCounts.OwnerUserId = Users.Id")
}

base_4 <- function(Posts, Users){
    costam <-Posts[Posts$PostTypeId == 2,]
    costam <- aggregate(Title ~ OwnerUserId, data = costam, FUN = function(x) length(x))
    names(costam)[2]="AnswersNumber"
    cosinnegotam <- Posts[Posts$PostTypeId==1,]
    cosinnegotam <- aggregate(Title ~ OwnerUserId, data=cosinnegotam, FUN = function(x) length(x))
    names(cosinnegotam)[2]="QuestionsNumber"
    merge(costam, cosinnegotam, by.x="OwnerUserId", by.y="OwnerUserId",) -> zmerdz
    zmerdz[zmerdz$AnswersNumber>zmerdz$QuestionsNumber, ] -> zmerdz
    zmerdz[order(-zmerdz$AnswersNumber),] -> zmerdz
    zmerdz[1:5,] -> zmerdz
    merge(zmerdz, Users[,], by.x="OwnerUserId", by.y="Id", ) -> jzmerdz
    wyn <- jzmerdz[, c("DisplayName", "QuestionsNumber", "AnswersNumber", "Location", "Reputation", "UpVotes", "DownVotes")]
}

dplyr_4 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

table_4 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
#all_equal(SQLDF4, BASE4, ignore_row_order = TRUE) #niewiadomo czemu nie zwraca 
#TRUE, jako że mamy tylko 5 wierszy można porównać wyświetlając ramki i przekonać 
#się, że wychodzi to samo 

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem


# -----------------------------------------------------------------------------#
# Zadanie 5
# -----------------------------------------------------------------------------#

sql_5 <- function(Posts, Comments, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

base_5 <- function(Posts, Comments, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

dplyr_5 <- function(Posts, Comments, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

table_5 <- function(Posts, Comments, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem


