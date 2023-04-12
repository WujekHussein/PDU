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
# microbenchmark(sql_1(Users), base_1(Users), dplyr_1(Users), table_1(Users))
# Unit: milliseconds
#          expr       min        lq      mean    median        uq       max neval
# sql_1(Users) 183.76275 186.93589 192.18894 189.24978 193.33104 253.13020   100
# base_1(Users) 102.06444 103.83002 110.24982 104.90681 106.47430 183.85208   100
# dplyr_1(Users)  29.22518  30.30720  39.13469  30.94724  32.41433  96.15944   100
# table_1(Users)  10.08141  10.67946  12.39634  11.05962  11.69483  71.66031   100

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
# microbenchmark(sql_2(Posts), base_2(Posts), dplyr_2(Posts), table_2(Posts))

# -----------------------------------------------------------------------------#
# Zadanie 3
# -----------------------------------------------------------------------------#

sql_3 <- function(Posts, Users){
  
  Questions <- sqldf(
    'SELECT OwnerUserId, SUM(ViewCount) as TotalViews
     FROM Posts 
     WHERE PostTypeId = 1
     GROUP BY OwnerUserId')
  
  sqldf("
          SELECT Id, DisplayName, TotalViews
          FROM Questions
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
# microbenchmark(sql_3(Posts,Users), base_3(Posts,Users), dplyr_3(Posts,Users), table_3(Posts, Users))
# Unit: milliseconds
#                expr       min        lq     mean    median        uq      max neval
# sql_3(Posts, Users) 736.69772 753.58000 773.2844 760.84774 784.82534 861.5291   100
# base_3(Posts, Users) 244.37739 249.41539 282.9387 258.58162 309.30306 551.4526   100
# dplyr_3(Posts, Users)  81.70995  84.03877 108.8659  86.99693 142.81723 186.8412   100
# table_3(Posts, Users)  18.76055  19.60366  32.9430  21.59652  22.32329 296.3285   100

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
    Posts %>%
    filter(PostTypeId==2) %>%
    group_by(OwnerUserId) %>%
    summarise(AnswersNumber = n() ) -> Answers
  
    Posts %>%
      filter(PostTypeId==1) %>%
      group_by(OwnerUserId) %>%
      summarise(QuestionsNumber = n() ) -> Questions 
    Answers %>%
      filter(!is.na(OwnerUserId)) -> Answers
    Questions %>%
      filter(!is.na(OwnerUserId)) -> Questions
    
    Answers %>%
      inner_join(Questions, by=c("OwnerUserId"="OwnerUserId")) %>%
      filter(AnswersNumber>QuestionsNumber) %>%
      arrange(-AnswersNumber) %>%
      slice(1:5) -> PostsCounts
    
    PostsCounts %>%
      inner_join(Users, by=c("OwnerUserId"="Id")) %>%
      select(DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes) 
}

table_4 <- function(Posts, Users){
    Answers <- as.data.table(Posts)[PostTypeId==2 & !is.na(OwnerUserId), .(AnswersNumber = .N), by= OwnerUserId]
    Questions <- as.data.table(Posts)[PostTypeId==1 & !is.na(OwnerUserId), .(QuestionsNumber = .N), by=OwnerUserId]
    PostsCounts <- Questions[Answers, on=c(OwnerUserId="OwnerUserId")][!is.na(QuestionsNumber)][AnswersNumber>QuestionsNumber]
    PostsCounts <- setorder(PostsCounts, -AnswersNumber)[1:5]
    as.data.table(Users)[PostsCounts, on=c(Id="OwnerUserId")][, .(DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes)]
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
#all_equal(SQLDF4, BASE4, ignore_row_order = TRUE) #niewiadomo czemu nie zwraca 
#TRUE, jako że mamy tylko 5 wierszy można porównać wyświetlając ramki i przekonać 
#się, że wychodzi to samo 
#all_equal(SQLDF4, DPLYR4)
#all_equal(SQLDF4, TABLE4)
# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# microbenchmark(sql_4(Posts,Users), base_4(Posts,Users), dplyr_4(Posts,Users), table_4(Posts, Users))
# Unit: milliseconds
#               expr     min        lq      mean    median        uq       max     neval
# sql_4(Posts, Users) 811.68423 828.53415 867.72806 846.88357 895.91597 1136.7557   100
# base_4(Posts, Users) 346.47391 369.17087 414.70981 422.24043 433.22664  656.9046   100
# dplyr_4(Posts, Users) 326.86724 350.60097 389.59014 382.05556 408.29459  673.8282   100
# table_4(Posts, Users)  21.26979  22.49576  44.54296  23.93542  83.49347  139.0104   100

# -----------------------------------------------------------------------------#
# Zadanie 5
# -----------------------------------------------------------------------------#

sql_5 <- function(Posts, Comments, Users){
  
  CmtTotScr <- sqldf('SELECT PostId, SUM(Score) AS CommentsTotalScore
                        FROM Comments
                        GROUP BY PostId')
  PostsBestComments <- sqldf('
        SELECT Posts.OwnerUserId, Posts.Title, Posts.CommentCount, Posts.ViewCount, 
               CmtTotScr.CommentsTotalScore
        FROM CmtTotScr
        JOIN Posts ON Posts.Id = CmtTotScr.PostId
        WHERE Posts.PostTypeId=1')
  
  sqldf('SELECT Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location
            FROM PostsBestComments
            JOIN Users ON PostsBestComments.OwnerUserId = Users.Id
            ORDER BY CommentsTotalScore DESC
            LIMIT 10')
}

base_5 <- function(Posts, Comments, Users){
    CmtTotScr <- aggregate(Score~PostId,data=Comments, FUN=sum) 
    names(CmtTotScr)[2] <- "CommentsTotalScore"
    PostsBestComments <- merge(CmtTotScr, Posts, by.x="PostId", by.y="Id")
    PostsBestComments <- PostsBestComments[PostsBestComments$PostTypeId==1, c("OwnerUserId", "Title", "CommentCount", "ViewCount", "CommentsTotalScore")]
    wynpra <- merge(PostsBestComments, Users, by.x="OwnerUserId", by.y="Id")
    wynpra[order(-wynpra$CommentsTotalScore), c("Title", "CommentCount", "ViewCount", "CommentsTotalScore", "DisplayName", "Reputation","Location")][1:10,]
}

dplyr_5 <- function(Posts, Comments, Users){
    Comments %>%
    group_by(PostId) %>%
    summarise(CommentsTotalScore=sum(Score)) -> CmtTotScr
    CmtTotScr %>%
    inner_join(Posts, by=c("PostId"="Id")) %>%
      filter(PostTypeId==1) %>%
      select(OwnerUserId, Title, CommentCount, ViewCount, CommentsTotalScore) -> PostsBestComments
    PostsBestComments %>% 
      inner_join(Users, by=c("OwnerUserId"="Id")) %>%
      select(Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location) %>%
      arrange(-CommentsTotalScore) %>%
      slice(1:10) 
}

table_5 <- function(Posts, Comments, Users){
    CmtTotScr <- as.data.table(Comments)[, .(CommentsTotalScore=sum(Score)), by=PostId]
    PostsBestComments <- as.data.table(Posts)[CmtTotScr,on=c(Id="PostId")][PostTypeId==1][, .(OwnerUserId, Title, CommentCount, ViewCount, CommentsTotalScore)]
    setorder(as.data.table(Users)[PostsBestComments, on=c(Id="OwnerUserId")][, .(Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location)][!is.na(DisplayName) & !is.na(Reputation) & !is.na(Location)], -CommentsTotalScore)[1:10] 
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
# all_equal(SQLDF5, BASE5)
# all_equal(SQLDF5, DPLYR5)
# all_equal(SQLDF5, TABLE5)


# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# microbenchmark(sql_5(Posts, Comments, Users), base_5(Posts, Comments, Users), dplyr_5(Posts, Comments, Users), table_5(Posts, Comments, Users))
# Unit: milliseconds
#                          expr        min         lq       mean     median        uq       max neval
# sql_5(Posts, Comments, Users) 1207.84946 1228.18848 1272.89879 1256.11190 1295.9324 1555.1469   100
# base_5(Posts, Comments, Users)  778.09996  846.60452  880.75000  866.76643  903.8784 1202.5937   100
# dplyr_5(Posts, Comments, Users)  274.04144  309.40939  369.92042  352.01544  394.6632  688.0110   100
# table_5(Posts, Comments, Users)   54.05529   58.07156   96.44303   69.44905  132.4975  374.2502   100
