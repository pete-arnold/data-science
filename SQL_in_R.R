# ------------------------------------------------------------------------------
# Using SQL in R
# Pete Arnold
# 21.10.2022
# ------------------------------------------------------------------------------

# 1. Getting it working.
# ------------------------------------------------------------------------------
library(RPostgreSQL)    # To access the database.
library(GetoptLong)     # To substitute variables into strings.

drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname='gp_practice_data', host='localhost',
                 port=5433, user='postgres',
                 password=.rs.askForPassword('Password:'))
tables <- dbListTables(con)
print(tables)
con <- dbConnect(drv, dbname='gp_practice_data', host='localhost',
port=5433, user='postgres',
password=.rs.askForPassword('Password:'))

# 2. Getting it working with queries (after exercise 2).
# ------------------------------------------------------------------------------
df <- dbGetQuery(con, "
    select  gp.practiceid as practice_id,
            min(gp.period) as earliest_date,
            max(gp.period) as latest_date
    from    gp_data_up_to_2015 as gp
    inner join
        address as ad
    on  gp.practiceid = ad.practiceid
    where 
        ad.postcode like 'CF%'
    group by
        gp.practiceid
")

# 3. Some extras that make life easier.
# ------------------------------------------------------------------------------
# Escaped quotes:
df <- dbGetQuery(con, '
    select  gp.practiceid, max(gp.period), min(gp.period)
    from    gp_data_up_to_2015 as gp
    inner join
        address as ad
    on  gp.practiceid = ad.practiceid
    where 
        ad.postcode like \'CF%\'
    group by
        gp.practiceid
')

# Adding variables to the queries:
# (a) Using paste():
df <- dbGetQuery(con,
    paste("
        select  gp.practiceid as practice_id,
                min(gp.period) as earliest_date,
                max(gp.period) as latest_date
        from    gp_data_up_to_2015 as gp
        inner join
            address as ad
        on  gp.practiceid = ad.practiceid
        where 
            ad.postcode like '", postcode, "%'
        group by
            gp.practiceid
    ", sep=""))

# (b) Using qq():
df <- dbGetQuery(con, qq("
    select  gp.practiceid as practice_id,
            min(gp.period) as earliest_date,
            max(gp.period) as latest_date
    from    gp_data_up_to_2015 as gp
    inner join
        address as ad
    on  gp.practiceid = ad.practiceid
    where 
        ad.postcode like '@{postcode}%'
    group by
        gp.practiceid
"))

# Less copy and paste:
# How do we make this generic so that we can use it with any specified
# postcode area?
# Use a function.
get_practice_date_range <- function(db_con, postcode){
    df <- dbGetQuery(db_con, paste("
        select  gp.practiceid as practice_id,
                min(gp.period) as earliest_date,
                max(gp.period) as latest_date
        from    gp_data_up_to_2015 as gp
        inner join
            address as ad
        on  gp.practiceid = ad.practiceid
        where 
            ad.postcode like '", postcode, "%'
        group by
            gp.practiceid
    ", sep=""))
    return(df)
}

# Or:
get_practice_date_range_qq <- function(db_con, postcode){
    df <- dbGetQuery(db_con, qq("
        select  gp.practiceid as practice_id,
                min(gp.period) as earliest_date,
                max(gp.period) as latest_date
        from    gp_data_up_to_2015 as gp
        inner join
            address as ad
        on  gp.practiceid = ad.practiceid
        where
            ad.postcode like '@{postcode}%'
        group by
            gp.practiceid
    "))
    return(df)
}

# And then we can write clearer, more concise code:
result_paste <- get_practice_date_range(con, 'CF')
result_qq <- get_practice_date_range(con, 'CF')
swansea_practices <- get_practice_date_range(con, 'SA')
llanelli_practices <- get_practice_date_range(con, 'LL')
invalid_practices <- get_practice_date_range(con, 'XX')

# And we can add some error handling:
get_practice_date_range <- function(db_con, postcode){
    df <- dbGetQuery(db_con, qq("
        select  gp.practiceid as practice_id,
                min(gp.period) as earliest_date,
                max(gp.period) as latest_date
        from    gp_data_up_to_2015 as gp
        inner join
            address as ad
        on  gp.practiceid = ad.practiceid
        where 
            ad.postcode like '@{postcode}%'
        group by
            gp.practiceid
    "))
    if (nrow(df) == 0){
        warning(paste('No practices found for ', postcode, '.\n', sep=''))
    }
    return(df)
}

# ------------------------------------------------------------------------------







