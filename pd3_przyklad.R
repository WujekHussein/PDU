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

# ....

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

# microbenchmark::microbenchmark( sql_3 = sql_3(Posts, Users) )
# Unit: milliseconds
# expr   min      lq       mean     median   uq       max
# sql_3  957.7503 986.7745 1033.813 1014.249 1064.611 1396.934
# neval
# 100

# microbenchmark::microbenchmark( sql_3 = sql_3(Posts, Users), times = 25 )
# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# sql_3 971.6418 977.6637 1001.585 982.1214 999.0253 1093.475    25

# -----------------------------------------------------------------------------#
# Zadanie 5
# -----------------------------------------------------------------------------#

sql_5 <- function(Posts, Comments, Users){
    con <- sqldf()
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
    sqldf()
}

# microbenchmark::microbenchmark(sql_5 = sql_5(Posts, Comments, Users), 
#                                times = 25)
# Unit: seconds
# expr      min      lq    mean  median      uq
# sql_5 1.584512 1.61264 1.69366 1.68763 1.75951
# max         neval
# 1.925591    25
