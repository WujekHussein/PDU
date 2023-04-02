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
   wyn <- as.data.table(Users)[Location!='', .(TotalUpVotes=sum(UpVotes)), by=Location][order(-TotalUpVotes)][1:10] #wszystko w jednej linii, bo mogę
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem


# -----------------------------------------------------------------------------#
# Zadanie 2
# -----------------------------------------------------------------------------#

sql_2 <- function(Posts){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

base_2 <- function(Posts){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

dplyr_2 <- function(Posts){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

table_2 <- function(Posts){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem


# -----------------------------------------------------------------------------#
# Zadanie 3
# -----------------------------------------------------------------------------#

sql_3 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

base_3 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

dplyr_3 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

table_3 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem


# -----------------------------------------------------------------------------#
# Zadanie  4
# -----------------------------------------------------------------------------#

sql_4 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
}

base_4 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
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


