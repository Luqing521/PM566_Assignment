Lab 10 - SQL
================

# Setup

``` r
# install.package(c("RSQLite","DBI"))
library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
actor <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/actor.csv")
rental <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/rental.csv")
customer <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/customer.csv")
payment <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/payment_p2007_01.csv")

# Copy data.frames to database
dbWriteTable(con, "actor", actor)
dbWriteTable(con, "rental", rental)
dbWriteTable(con, "customer", customer)
dbWriteTable(con, "payment", payment)
```

``` r
dbListTables(con)
```

    ## [1] "actor"    "customer" "payment"  "rental"

TIP: Use can use the following QUERY to see the structure of a table

``` sql
PRAGMA table_info(payment)
```

<div class="knitsql-table">

| cid | name          | type    | notnull | dflt\_value | pk |
| :-- | :------------ | :------ | ------: | :---------- | -: |
| 0   | payment\_id   | INTEGER |       0 | NA          |  0 |
| 1   | customer\_id  | INTEGER |       0 | NA          |  0 |
| 2   | staff\_id     | INTEGER |       0 | NA          |  0 |
| 3   | rental\_id    | INTEGER |       0 | NA          |  0 |
| 4   | amount        | REAL    |       0 | NA          |  0 |
| 5   | payment\_date | TEXT    |       0 | NA          |  0 |

6 records

</div>

SQL references:

<https://www.w3schools.com/sql/>

# Exercise 1

Retrive the actor ID, first name and last name for all actors using the
`actor` table. Sort by last name and then by first name.

``` sql
SELECT actor_id, first_name, last_name
FROM actor
ORDER by last_name,first_name
```

<div class="knitsql-table">

| actor\_id | first\_name | last\_name |
| --------: | :---------- | :--------- |
|        58 | CHRISTIAN   | AKROYD     |
|       182 | DEBBIE      | AKROYD     |
|        92 | KIRSTEN     | AKROYD     |
|       118 | CUBA        | ALLEN      |
|       145 | KIM         | ALLEN      |
|       194 | MERYL       | ALLEN      |
|        76 | ANGELINA    | ASTAIRE    |
|       112 | RUSSELL     | BACALL     |
|       190 | AUDREY      | BAILEY     |
|        67 | JESSICA     | BAILEY     |

Displaying records 1 - 10

</div>

# Exercise 2

Retrive the actor ID, first name, and last name for actors whose last
name equals ‘WILLIAMS’ or ‘DAVIS’.

``` sql
SELECT actor_id,first_name,last_name
FROM actor
WHERE last_name IN ('WILLIAMS','DAVIS') 
```

<div class="knitsql-table">

| actor\_id | first\_name | last\_name |
| --------: | :---------- | :--------- |
|         4 | JENNIFER    | DAVIS      |
|        72 | SEAN        | WILLIAMS   |
|       101 | SUSAN       | DAVIS      |
|       110 | SUSAN       | DAVIS      |
|       137 | MORGAN      | WILLIAMS   |
|       172 | GROUCHO     | WILLIAMS   |

6 records

</div>

# Exercise 3

Write a query against the `rental` table that returns the IDs of the
customers who rented a film on July 5, 2005 (use the rental.rental\_date
column, and you can use the date() function to ignore the time
component). Include a single row for each distinct customer ID.

``` sql
SELECT DISTINCT customer_id
FROM rental
WHERE date(rental_date) = '2005-07-05'
```

# Exercise 4

## Exercise 4.1

Construct a query that retrives all rows from the `payment` table where
the amount is either 1.99, 7.99, 9.99.

``` sql
SELECT *
FROM payment
WHERE amount IN (1.99, 7.99, 9.99)
```

<div class="knitsql-table">

| payment\_id | customer\_id | staff\_id | rental\_id | amount | payment\_date              |
| ----------: | -----------: | --------: | ---------: | -----: | :------------------------- |
|       16050 |          269 |         2 |          7 |   1.99 | 2007-01-24 21:40:19.996577 |
|       16056 |          270 |         1 |        193 |   1.99 | 2007-01-26 05:10:14.996577 |
|       16081 |          282 |         2 |         48 |   1.99 | 2007-01-25 04:49:12.996577 |
|       16103 |          294 |         1 |        595 |   1.99 | 2007-01-28 12:28:20.996577 |
|       16133 |          307 |         1 |        614 |   1.99 | 2007-01-28 14:01:54.996577 |
|       16158 |          316 |         1 |       1065 |   1.99 | 2007-01-31 07:23:22.996577 |
|       16160 |          318 |         1 |        224 |   9.99 | 2007-01-26 08:46:53.996577 |
|       16161 |          319 |         1 |         15 |   9.99 | 2007-01-24 23:07:48.996577 |
|       16180 |          330 |         2 |        967 |   7.99 | 2007-01-30 17:40:32.996577 |
|       16206 |          351 |         1 |       1137 |   1.99 | 2007-01-31 17:48:40.996577 |

Displaying records 1 - 10

</div>

## Exercise 4.2

Construct a query that retrives all rows from the `payment` table where
the amount is greater then 5

``` sql
SELECT *
FROM payment
WHERE amount >5
```

<div class="knitsql-table">

| payment\_id | customer\_id | staff\_id | rental\_id | amount | payment\_date              |
| ----------: | -----------: | --------: | ---------: | -----: | :------------------------- |
|       16052 |          269 |         2 |        678 |   6.99 | 2007-01-28 21:44:14.996577 |
|       16058 |          271 |         1 |       1096 |   8.99 | 2007-01-31 11:59:15.996577 |
|       16060 |          272 |         1 |        405 |   6.99 | 2007-01-27 12:01:05.996577 |
|       16061 |          272 |         1 |       1041 |   6.99 | 2007-01-31 04:14:49.996577 |
|       16068 |          274 |         1 |        394 |   5.99 | 2007-01-27 09:54:37.996577 |
|       16073 |          276 |         1 |        860 |  10.99 | 2007-01-30 01:13:42.996577 |
|       16074 |          277 |         2 |        308 |   6.99 | 2007-01-26 20:30:05.996577 |
|       16082 |          282 |         2 |        282 |   6.99 | 2007-01-26 17:24:52.996577 |
|       16086 |          284 |         1 |       1145 |   6.99 | 2007-01-31 18:42:11.996577 |
|       16087 |          286 |         2 |         81 |   6.99 | 2007-01-25 10:43:45.996577 |

Displaying records 1 - 10

</div>

## Exercise 4.3

Construct a query that retrives all rows from the `payment` table where
the amount is greater then 5 and less then 8

``` sql
SELECT *
FROM payment
WHERE (amount >5) AND (amount <8)
```

<div class="knitsql-table">

| payment\_id | customer\_id | staff\_id | rental\_id | amount | payment\_date              |
| ----------: | -----------: | --------: | ---------: | -----: | :------------------------- |
|       16052 |          269 |         2 |        678 |   6.99 | 2007-01-28 21:44:14.996577 |
|       16060 |          272 |         1 |        405 |   6.99 | 2007-01-27 12:01:05.996577 |
|       16061 |          272 |         1 |       1041 |   6.99 | 2007-01-31 04:14:49.996577 |
|       16068 |          274 |         1 |        394 |   5.99 | 2007-01-27 09:54:37.996577 |
|       16074 |          277 |         2 |        308 |   6.99 | 2007-01-26 20:30:05.996577 |
|       16082 |          282 |         2 |        282 |   6.99 | 2007-01-26 17:24:52.996577 |
|       16086 |          284 |         1 |       1145 |   6.99 | 2007-01-31 18:42:11.996577 |
|       16087 |          286 |         2 |         81 |   6.99 | 2007-01-25 10:43:45.996577 |
|       16092 |          288 |         2 |        427 |   6.99 | 2007-01-27 14:38:30.996577 |
|       16094 |          288 |         2 |        565 |   5.99 | 2007-01-28 07:54:57.996577 |

Displaying records 1 - 10

</div>

# Exercise 5

Retrive all the payment IDs and their amount from the customers whose
last name is ‘DAVIS’.

``` sql
SELECT p.payment_id, p.amount, c.last_name
FROM customer AS c
  INNER JOIN payment AS p
ON c.customer_id = p.customer_id
WHERE c.last_name = 'DAVIS'
```

<div class="knitsql-table">

| payment\_id | amount | last\_name |
| :---------- | -----: | :--------- |
| 16685       |   4.99 | DAVIS      |
| 16686       |   2.99 | DAVIS      |
| 16687       |   0.99 | DAVIS      |

3 records

</div>

# Exercise 6

## Exercise 6.1

Use `COUNT(*)` to count the number of rows in `rental`

``` sql
SELECT COUNT(*) AS n_rows
FROM rental
```

<div class="knitsql-table">

| n\_rows |
| ------: |
|   16044 |

1 records

</div>

## Exercise 6.2

Use `COUNT(*)` and `GROUP BY` to count the number of rentals for each
`customer_id`

``` sql
SELECT COUNT(*) AS n_rows,customer_id
FROM rental
GROUP BY customer_id
```

<div class="knitsql-table">

| n\_rows | customer\_id |
| ------: | -----------: |
|      32 |            1 |
|      27 |            2 |
|      26 |            3 |
|      22 |            4 |
|      38 |            5 |
|      28 |            6 |
|      33 |            7 |
|      24 |            8 |
|      23 |            9 |
|      25 |           10 |

Displaying records 1 - 10

</div>

## Exercise 6.3

Repeat the previous query and sort by the count in descending order

``` sql
SELECT COUNT(*) AS n_rows,customer_id
FROM rental
GROUP BY customer_id
ORDER BY  n_rows DESC
```

<div class="knitsql-table">

| n\_rows | customer\_id |
| ------: | -----------: |
|      46 |          148 |
|      45 |          526 |
|      42 |          236 |
|      42 |          144 |
|      41 |           75 |
|      40 |          469 |
|      40 |          197 |
|      39 |          468 |
|      39 |          178 |
|      39 |          137 |

Displaying records 1 - 10

</div>

## Exercise 6.4

Repeat the previous query but use `HAVING` to only keep the groups with
40 or more.

``` sql
SELECT COUNT(*) AS n_rows,customer_id
FROM rental
GROUP BY customer_id
HAVING COUNT(*) >= 40
ORDER BY n_rows DESC
```

<div class="knitsql-table">

| n\_rows | customer\_id |
| ------: | -----------: |
|      46 |          148 |
|      45 |          526 |
|      42 |          236 |
|      42 |          144 |
|      41 |           75 |
|      40 |          469 |
|      40 |          197 |

7 records

</div>

# Exercise 7

The following query calculates a number of summary statistics for the
payment table using `MAX`, `MIN`, `AVG` and `SUM`

``` sql
SELECT 
   MAX(amount) AS max_amount,
   MIN(amount) AS min_amount,
   AVG(amount) AS avg_amount,
   SUM(amount) AS sum_amount
FROM payment
```

<div class="knitsql-table">

| max\_amount | min\_amount | avg\_amount | sum\_amount |
| ----------: | ----------: | ----------: | ----------: |
|       11.99 |        0.99 |    4.169775 |     4824.43 |

1 records

</div>

## Exercise 7.1

Modify the above query to do those calculations for each `customer_id`

``` sql
SELECT customer_id,
   MAX(amount) AS max_amount,
   MIN(amount) AS min_amount,
   AVG(amount) AS avg_amount,
   SUM(amount) AS sum_amount
FROM payment
GROUP BY customer_id
```

<div class="knitsql-table">

| customer\_id | max\_amount | min\_amount | avg\_amount | sum\_amount |
| -----------: | ----------: | ----------: | ----------: | ----------: |
|            1 |        2.99 |        0.99 |    1.990000 |        3.98 |
|            2 |        4.99 |        4.99 |    4.990000 |        4.99 |
|            3 |        2.99 |        1.99 |    2.490000 |        4.98 |
|            5 |        6.99 |        0.99 |    3.323333 |        9.97 |
|            6 |        4.99 |        0.99 |    2.990000 |        8.97 |
|            7 |        5.99 |        0.99 |    4.190000 |       20.95 |
|            8 |        6.99 |        6.99 |    6.990000 |        6.99 |
|            9 |        4.99 |        0.99 |    3.656667 |       10.97 |
|           10 |        4.99 |        4.99 |    4.990000 |        4.99 |
|           11 |        6.99 |        6.99 |    6.990000 |        6.99 |

Displaying records 1 - 10

</div>

## Exercise 7.2

Modify the above query to only keep the `customer_id`s that have more
then 5 payments

``` sql
SELECT customer_id,
   MAX(amount) AS max_amount,
   MIN(amount) AS min_amount,
   AVG(amount) AS avg_amount,
   SUM(amount) AS sum_amount,
   COUNT(*) AS count
FROM payment
GROUP BY customer_id
HAVING count >5
```

<div class="knitsql-table">

| customer\_id | max\_amount | min\_amount | avg\_amount | sum\_amount | count |
| -----------: | ----------: | ----------: | ----------: | ----------: | ----: |
|           19 |        9.99 |        0.99 |    4.490000 |       26.94 |     6 |
|           53 |        9.99 |        0.99 |    4.490000 |       26.94 |     6 |
|          109 |        7.99 |        0.99 |    3.990000 |       27.93 |     7 |
|          161 |        5.99 |        0.99 |    2.990000 |       17.94 |     6 |
|          197 |        3.99 |        0.99 |    2.615000 |       20.92 |     8 |
|          207 |        6.99 |        0.99 |    2.990000 |       17.94 |     6 |
|          239 |        7.99 |        2.99 |    5.656667 |       33.94 |     6 |
|          245 |        8.99 |        0.99 |    4.823333 |       28.94 |     6 |
|          251 |        4.99 |        1.99 |    3.323333 |       19.94 |     6 |
|          269 |        6.99 |        0.99 |    3.156667 |       18.94 |     6 |

Displaying records 1 - 10

</div>

# Cleanup

Run the following chunk to disconnect from the connection.

``` r
# clean up
dbDisconnect(con)
```
