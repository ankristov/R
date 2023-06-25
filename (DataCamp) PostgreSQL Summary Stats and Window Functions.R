#
# Window functions vs GROUP BY
# Which of the following is FALSE?
# Answer the question
# Possible Answers
# - Unlike GROUP BY results, window functions don't reduce the number of rows in the output.
# - Window functions can fetch values from other rows into the table, whereas GROUP BY functions cannot.
# - Window functions can open a "window" to another table, whereas GROUP BY functions cannot. +
# - Window functions can calculate running totals and moving averages, whereas GROUP BY functions cannot.
# Correct! This choice is false because window functions take as input the rows of the table you're querying.

# Numbering rows
# The simplest application for window functions is numbering rows. Numbering rows allows you to easily fetch the nth row. For example, it would be very difficult to get the 35th row in any given table if you didn't have a column with each row's number.
# summer_medals
# year	city	  sport	    discipline	athlete	              country	gender	event	                      medal
# 1896	Athens	Aquatics	Swimming	  HAJOS Alfred	        HUN	    Men	    100M Freestyle	            Gold
# 1896	Athens	Aquatics	Swimming	  HERSCHMANN Otto	      AUT	    Men	    100M Freestyle	            Silver
# 1896	Athens	Aquatics	Swimming	  DRIVAS Dimitrios	    GRE	    Men	    100M Freestyle For Sailors	Bronze
# 1896	Athens	Aquatics	Swimming	  MALOKINIS Ioannis	    GRE	    Men	    100M Freestyle For Sailors	Gold
# 1896	Athens	Aquatics	Swimming	  CHASAPIS Spiridon	    GRE	    Men	    100M Freestyle For Sailors	Silver
# 1896	Athens	Aquatics	Swimming	  CHOROPHAS Efstathios	GRE	    Men	    1200M Freestyle	            Bronze
# Instructions
# - Number each row in the dataset.
SELECT
*,
-- Assign numbers to each row
ROW_Number() OVER() AS Row_N
FROM Summer_Medals
ORDER BY Row_N ASC;

# Numbering Olympic games in ascending order
# The Summer Olympics dataset contains the results of the games between 1896 and 2012. The first Summer Olympics were held in 1896, the second in 1900, and so on. What if you want to easily query the table to see in which year the 13th Summer Olympics were held? You'd need to number the rows for that.
# Instructions
# - Assign a number to each year in which Summer Olympic games were held.
SELECT
Year,
-- Assign numbers to each year
ROW_NUMBER() OVER() AS Row_N
FROM (
  SELECT DISTINCT year
  FROM Summer_Medals
  ORDER BY Year ASC
) AS Years
ORDER BY Year ASC;
# year	row_n
# 1896	1
# 1900	2
# 1904	3
# 1908	4
# 1912	5
# 1920	6
# 1924	7

#
# Numbering Olympic games in descending order
# You've already numbered the rows in the Summer Medals dataset. What if you need to reverse the row numbers so that the most recent Olympic games' rows have a lower number?
# Instructions
# - Assign a number to each year in which Summer Olympic games were held so that rows with the most recent years have lower row numbers.
SELECT
Year,
-- Assign the lowest numbers to the most recent years
ROW_NUMBER() OVER (ORDER BY Year DESC) AS Row_N
FROM (
  SELECT DISTINCT Year
  FROM Summer_Medals
) AS Years
ORDER BY Year;
# year	row_n
# 1896	27
# 1900	26
# 1904	25
# 1908	24
# 1912	23
# 1920	22
# Great! You've written your first ORDER BY subclause and changed the behavior of the ROW_NUMBER() window function.

# Numbering Olympic athletes by medals earned
# Row numbering can also be used for ranking. For example, numbering rows and ordering by the count of medals each athlete earned in the OVER clause will assign 1 to the highest-earning medalist, 2 to the second highest-earning medalist, and so on.
# Instructions
# - For each athlete, count the number of medals he or she has earned.
SELECT
-- Count the number of medals each athlete has earned
Athlete,
COUNT(medal) AS Medals
FROM Summer_Medals
GROUP BY Athlete
ORDER BY Medals DESC;
# athlete	            medals
# PHELPS Michael	    22
# LATYNINA Larisa	    18
# ANDRIANOV Nikolay	  15
# MANGIAROTTI Edoardo	13
# ONO Takashi	        13
# - Having wrapped the previous query in the Athlete_Medals CTE, rank each athlete by the number of medals they've earned.
WITH Athlete_Medals AS (
  SELECT
  -- Count the number of medals each athlete has earned
  Athlete,
  COUNT(*) AS medals
  FROM Summer_Medals
  GROUP BY athlete)
SELECT
-- Number each athlete by how many medals they ve earned
  athlete, medals,
  ROW_NUMBER() OVER (ORDER BY medals DESC) AS Row_N
FROM Athlete_Medals
ORDER BY medals DESC;
# athlete	            medals  row_n
# PHELPS Michael	    22	    1
# LATYNINA Larisa	    18	    2
# ANDRIANOV Nikolay	  15	    3
# MANGIAROTTI Edoardo	13	    4
# ONO Takashi	        13	    5
# SHAKHLIN Boris	    13	    6

# Reigning weightlifting champions
# A reigning champion is a champion who's won both the previous and current years' competitions. To determine if a champion is reigning, the previous and current years' results need to be in the same row, in two different columns.
# Instructions
# - Return each year's gold medalists in the Men's 69KG weightlifting competition.
SELECT 
-- Return each year's champions' countries
year,
country AS champion
FROM Summer_Medals
WHERE
Discipline = 'Weightlifting' AND
Event = '69KG' AND
Gender = 'Men' AND
Medal = 'Gold';
# year	champion
# 2000	BUL
# 2004	CHN
# 2008	CHN
# 2012	CHN
# AK: subquery
SELECT *
  -- Return each year's champions' countries
--year,
--country AS champion
FROM Summer_Medals
WHERE
Discipline = 'Weightlifting' AND
Event = '69KG' AND
Gender = 'Men' AND
Medal = 'Gold';
# year	city	  sport	        discipline	  athlete	        country	gender	event	medal
# 2000	Sydney	Weightlifting	Weightlifting	BOEVSKI Galabin	BUL	    Men	    69KG	Gold
# 2004	Athens	Weightlifting	Weightlifting	ZHANG Guozheng	CHN	    Men	    69KG	Gold
# 2008	Beijing	Weightlifting	Weightlifting	LIAO Hui	      CHN	    Men	    69KG	Gold
# 2012	London	Weightlifting	Weightlifting	LIN Qingfeng	  CHN	    Men	    69KG	Gold
# - Having wrapped the previous query in the Weightlifting_Gold CTE, get the previous year's champion for each year.
WITH Weightlifting_Gold AS (
  SELECT
    -- Return each year's champions' countries
    Year,
    Country AS champion
  FROM Summer_Medals
  WHERE
    Discipline = 'Weightlifting' AND
    Event = '69KG' AND
    Gender = 'Men' AND
    Medal = 'Gold')
SELECT
  Year, Champion,
  -- Fetch the previous year s champion
  LAG(Champion, 1) OVER (ORDER BY year ASC) AS Last_Champion
FROM Weightlifting_Gold
ORDER BY Year ASC;
# year	champion	last_champion
# 2000	BUL	      null
# 2004	CHN	      BUL
# 2008	CHN	      CHN
# 2012	CHN	      CHN
# Well done! You can now compare the two champion columns to see if a champion is reigning.

# 3 PARTITION BY.mp4
# Reigning champions by gender
# You've already fetched the previous year's champion for one event. However, if you have multiple events, genders, or other metrics as columns, you'll need to split your table into partitions to avoid having a champion from one event or gender appear as the previous champion of another event or gender.
# Instructions
# - Return the previous champions of each year's event by gender.
WITH Tennis_Gold AS (
  SELECT DISTINCT
  Gender, Year, Country
  FROM Summer_Medals
  WHERE
  Year >= 2000 AND
  Event = 'Javelin Throw' AND
  Medal = 'Gold')
SELECT
  Gender, Year,
  Country AS Champion,
  -- Fetch the previous year s champion by gender
  LAG(country) OVER (PARTITION BY gender ORDER BY gender, year ASC) AS Last_Champion
FROM Tennis_Gold
ORDER BY Gender ASC, Year ASC;
# gender year	champion	last_champion
# Men	   2000	CZE	      null
# Men	   2004	NOR	      CZE
# Men	   2008	NOR	      NOR
# Men	   2012	TTO	      NOR
# Women	 2000	NOR	      null
# Women	 2004	CUB	      NOR
# Women	 2008	CZE	      CUB
# Women	 2012	CZE	      CZE
# AK: subquery
SELECT DISTINCT
  Gender, Year, Country
FROM Summer_Medals
WHERE
  Year >= 2000 AND
  Event = 'Javelin Throw' AND
  Medal = 'Gold';
# gender	year	country
# Men	    2000	CZE
# Men	    2004	NOR
# Men	    2008	NOR
# Men	    2012	TTO
# Women	  2000	NOR
# Women	  2004	CUB
# Women	  2008	CZE
# Women	  2012	CZE
# Great! Partitioning correctly split the champions by gender, so that data on champions of one gender does not get mixed into the other gender's results.
  
# Reigning champions by gender and event
# In the previous exercise, you partitioned by gender to ensure that data about one gender doesn't get 
  # mixed into data about the other gender. If you have multiple columns, however, partitioning by only 
  # one of them will still mix the results of the other columns.
# Instructions
# - Return the previous champions of each year's events by gender and event.
  WITH Athletics_Gold AS (
    SELECT DISTINCT
    Gender, Year, Event, Country
    FROM Summer_Medals
    WHERE
    Year >= 2000 AND
    Discipline = 'Athletics' AND
    Event IN ('100M', '10000M') AND
    Medal = 'Gold')
  
  SELECT
  Gender, Year, Event,
  Country AS Champion,
  -- Fetch the previous year s champion by gender and event
  LAG(Country) OVER (PARTITION BY Gender, Event
            ORDER BY Year ASC) AS Last_Champion
FROM Athletics_Gold
ORDER BY Event ASC, Gender ASC, Year ASC;
# gender	year	event	  champion	last_champion
# Men	    2000	10000M	ETH	      null
# Men	    2004	10000M	ETH	      ETH
# Men	    2008	10000M	ETH	      ETH
# Men	    2012	10000M	GBR	      ETH
# Women	  2000	10000M	ETH	      null
# Women	  2004	10000M	CHN	      ETH
# Women	  2008	10000M	ETH	      CHN
# Women	  2012	10000M	ETH	      ETH
# Men	    2000	100M	  USA	      null
# Men	    2004	100M	  USA	      USA
# Men	    2008	100M	  JAM	      USA
# Men	    2012	100M	  JAM	      JAM
# Women	  2004	100M	  BLR	      null
# Women	  2008	100M	  JAM	      BLR
# Women	  2012	100M	  JAM	      JAM
# Good job! You can partition by more than one column in case your groups are spread over several columns.

# Row numbers with partitioning
# If you run ROW_NUMBER() OVER (PARTITION BY Year ORDER BY Medals DESC) on the following table, what row number would the 2008 Iranian record have?
#   | Year | Country | Medals |
#   |------|---------|--------|
#   | 2004 | IRN     | 32     |
#   | 2004 | LBN     | 17     |
#   | 2004 | KSA     | 4      |
#   | 2008 | IRQ     | 29     |
#   | 2008 | IRN     | 27     |
#   | 2008 | UAE     | 12     |
# Possible Answers
# - 5
# - 1
# - 2 +

# 4 Fetching.mp4
# Future gold medalists
# Fetching functions allow you to get values from different parts of the table into one row. If you have time-ordered data, you can "peek into the future" with the LEAD fetching function. This is especially useful if you want to compare a current value to a future value.
# Instructions
# - For each year, fetch the current gold medalist and the gold medalist 3 competitions ahead of the current row.
WITH Discus_Medalists AS (
  SELECT DISTINCT
  Year,
  Athlete
  FROM Summer_Medals
  WHERE Medal = 'Gold'
  AND Event = 'Discus Throw'
  AND Gender = 'Women'
  AND Year >= 2000)
SELECT
-- For each year, fetch the current and future medalists
year,
athlete,
LEAD(athlete,3) OVER (ORDER BY year ASC) AS Future_Champion
FROM Discus_Medalists
ORDER BY Year ASC;
# year	athlete	        future_champion
# 2000	ZVEREVA Ellina	PERKOVIC Sandra
# 2004	SADOVA Natalya	null
# 2008	BROWN TRAFTON   Stephanie	null
# 2012	PERKOVIC Sandra	null
# AK: subquery
WITH Discus_Medalists AS (
  SELECT DISTINCT
  Year,
  Athlete
  FROM Summer_Medals
  WHERE Medal = 'Gold'
  AND Event = 'Discus Throw'
  AND Gender = 'Women'
  AND Year >= 2000)
SELECT *
FROM Discus_Medalists
ORDER BY Year ASC;
# year	athlete
# 2000	ZVEREVA Ellina
# 2004	SADOVA Natalya
# 2008	BROWN TRAFTON Stephanie
# 2012	PERKOVIC Sandra
# Good job! You fetched future competitions' results with LEAD().

# First athlete by name
# It's often useful to get the first or last value in a dataset to compare all other values to it. With absolute fetching functions like FIRST_VALUE, you can fetch a value at an absolute position in the table, like its beginning or end.
# Instructions
# - Return all athletes and the first athlete ordered by alphabetical order.
WITH All_Male_Medalists AS (
  SELECT DISTINCT
  Athlete
  FROM Summer_Medals
  WHERE Medal = 'Gold'
  AND Gender = 'Men')
SELECT
-- Fetch all athletes and the first athlete alphabetically
Athlete,
FIRST_VALUE(Athlete) OVER (
  ORDER BY Athlete ASC
) AS First_Athlete
FROM All_Male_Medalists;
# athlete	                first_athlete
# AABYE Edgar	            AABYE Edgar
# AALTONEN Paavo Johannes	AABYE Edgar
# AAS Thomas Valentin	    AABYE Edgar
# ABALMASAU Aliaksei	    AABYE Edgar
# ABALO Luc	              AABYE Edgar
# ABANDA ETONG Patrice	  AABYE Edgar
# Great! You can use absolute position fetching functions to fetch values at fixed positions in your table or partition.

# Last country by name
# Just like you can get the first row's value in a dataset, you can get the last row's value. This is often useful when you want to compare the most recent value to previous values.
# Instructions
# - Return the year and the city in which each Olympic games were held.
# - Fetch the last city in which the Olympic games were held.
WITH Hosts AS (
  SELECT DISTINCT Year, City
  FROM Summer_Medals)
SELECT
Year,
City,
-- Get the last city in which the Olympic games were held
LAST_VALUE(City) OVER (
  ORDER BY Year ASC
  RANGE BETWEEN
  UNBOUNDED PRECEDING AND
  UNBOUNDED FOLLOWING
) AS Last_City
FROM Hosts
ORDER BY Year ASC;
# year	city	    last_city
# 1896	Athens	  London
# 1900	Paris	    London
# 1904	St Louis	London
# 1908	London	  London
# 1912	Stockholm	London
# Well done! Now you can get the values of the rows at the beginning and end of your table.

# 5 Ranking.mp4
# Ranking athletes by medals earned
# In chapter 1, you used ROW_NUMBER to rank athletes by awarded medals. However, ROW_NUMBER assigns different numbers to athletes with the same count of awarded medals, so it's not a useful ranking function; if two athletes earned the same number of medals, they should have the same rank.
# Instructions
# - Rank each athlete by the number of medals they've earned -- the higher the count, the higher the rank -- with identical numbers in case of identical values.
WITH Athlete_Medals AS (
  SELECT
  Athlete,
  COUNT(*) AS Medals
  FROM Summer_Medals
  GROUP BY Athlete)

SELECT
Athlete,
Medals,
-- Rank athletes by the medals they ve won
  RANK() OVER (ORDER BY Medals DESC) AS Rank_N
FROM Athlete_Medals
ORDER BY Medals DESC;
# athlete	            medals	rank_n
# PHELPS Michael	    22	    1
# LATYNINA Larisa	    18	    2
# ANDRIANOV Nikolay	  15	    3
# MANGIAROTTI Edoardo	13	    4
# ONO Takashi	        13	    4
# SHAKHLIN Boris	    13	    4
# Well done! RANK's output corresponds to the actual Olympics' ranking system.

# Ranking athletes from multiple countries
# In the previous exercise, you used RANK to assign rankings to one group of athletes. In real-world data, however, you'll often find numerous groups within your data. Without partitioning your data, one group's values will influence the rankings of the others.
# Also, while RANK skips numbers in case of identical values, the most natural way to assign rankings is not to skip numbers. If two countries are tied for second place, the country after them is considered to be third by most people.
# Instructions
# - Rank each country's athletes by the count of medals they've earned -- the higher the count, the higher the rank -- without skipping numbers in case of identical values.
WITH Athlete_Medals AS (
  SELECT
  Country, Athlete, COUNT(*) AS Medals
  FROM Summer_Medals
  WHERE
  Country IN ('JPN', 'KOR')
  AND Year >= 2000
  GROUP BY Country, Athlete
  HAVING COUNT(*) > 1)

SELECT 
Country,
-- Rank athletes in each country by the medals they ve won
  Athlete, Medals,
  DENSE_RANK() OVER (PARTITION BY Country
                ORDER BY Medals DESC) AS Rank_N
FROM Athlete_Medals
ORDER BY Country ASC, RANK_N ASC;
# country	athlete	      medals	rank_n
# JPN	KITAJIMA Kosuke	  7	1
# JPN	UCHIMURA Kohei	  5	2
# JPN	TAKEDA Miho	      4	3
# JPN	TACHIBANA Miya	  4	3
# JPN	IRIE Ryosuke	    3	4
# JPN	TOMITA Hiroyuki	  3	4
# JPN	SUZUKI Satomi	    3	4
# JPN	TANI Ryoko      	3	4
# JPN	KASHIMA Takehiro	3	4
# JPN	YOSHIDA Saori	    3	4
# JPN	ICHO Kaori	      3	4
# JPN	MATSUDA Takeshi	  3	4
# JPN	UTSUGI Reika	    2	5
# JPN	YONEDA Isao	      2	5
# JPN	SATO Rie	        2	5
# JPN	MUROFUSHI Koji  	2	5
# AK: subquery
WITH Athlete_Medals AS (
SELECT
Country, Athlete, COUNT(*) AS Medals
FROM Summer_Medals
WHERE
Country IN ('JPN', 'KOR')
AND Year >= 2000
GROUP BY Country, Athlete
HAVING COUNT(*) > 1)
SELECT *
FROM Athlete_Medals
--ORDER BY Country ASC, RANK_N ASC;
# country	athlete	        medals
# JPN	    YONEDA Yoko	    2
# KOR	    KIM Soo-Nyung	  2
# JPN   	YONEDA Isao	    2
# JPN	    SATO Rie      	2
# JPN	    MUROFUSHI Koji	2
# Good job! DENSE_RANK's way of ranking is how we'd typically assign ranks in real life.

# DENSE_RANK's output
# You have the following table:
#  | Country | Medals |
#  |---------|--------|
#  | IRN     | 23     |
#  | IRQ     | 19     |
#  | LBN     | 19     |
#  | SYR     | 19     |
#  | BHR     | 7      |
#  | KSA     | 3      |
#  If you were to use DENSE_RANK to order the Medals column in descending order, what rank would BHR be assigned?
# Answer the question
# Possible Answers
# - 5
# - 3 +
# - 6
# Correct! DENSE_RANK would rank BHR as 3rd.

# 6 Paging.mp4
# Paging events
# There are exactly 666 unique events in the Summer Medals Olympics dataset. If you want to chunk them up to analyze them piece by piece, you'll need to split the events into groups of approximately equal size.
# Instructions
# - Split the distinct events into exactly 111 groups, ordered by event in alphabetical order.
WITH Events AS (
  SELECT DISTINCT Event
  FROM Summer_Medals)

SELECT
--- Split up the distinct events into 111 unique groups
Event,
NTILE(111) OVER (ORDER BY Event ASC) AS Page
FROM Events
ORDER BY Event ASC;
# event	                            page
# + 100KG	                          1
# + 100KG (Heavyweight)           	1
# + 100KG (Super Heavyweight)     	1
# + 105KG                         	1
# + 108KG Total (Super Heavyweight)	1
# + 110KG Total (Super Heavyweight)	1
# + 67 KG	                          2
# + 71.67KG (Heavyweight)	          2
# + 72KG (Heavyweight)	            2
# + 73KG (Heavyweight)            	2
# + 75KG                          	2
# + 78KG                          	2
# + 78KG (Heavyweight)            	3
# + 79.38KG (Heavyweight)	          3
# Good! NTILE() allows you to make the size of the dataset you're working with more manageable.

# Top, middle, and bottom thirds
# Splitting your data into thirds or quartiles is often useful to understand how the values in your dataset are spread. Getting summary statistics (averages, sums, standard deviations, etc.) of the top, middle, and bottom thirds can help you determine what distribution your values follow.
# Instructions 1/2
# - Split the athletes into top, middle, and bottom thirds based on their count of medals.
WITH Athlete_Medals AS (
  SELECT Athlete, COUNT(*) AS Medals
  FROM Summer_Medals
  GROUP BY Athlete
  HAVING COUNT(*) > 1)

SELECT
Athlete,
Medals,
-- Split athletes into thirds by their earned medals
NTILE(3) OVER (ORDER BY Medals DESC) AS Third
FROM Athlete_Medals
ORDER BY Medals DESC, Athlete ASC;
# athlete	            medals	    third
# PHELPS Michael	    22	        1
# LATYNINA Larisa	    18	        1
# ANDRIANOV Nikolay	  15	        1
# MANGIAROTTI Edoardo	13	        1
# ONO Takashi	        13	        1
# SHAKHLIN Boris	    13	        1
# COUGHLIN Natalie  	12	        1
# - Return the average of each third.
WITH Athlete_Medals AS (
  SELECT Athlete, COUNT(*) AS Medals
  FROM Summer_Medals
  GROUP BY Athlete
  HAVING COUNT(*) > 1),
Thirds AS (
  SELECT
  Athlete,
  Medals,
  NTILE(3) OVER (ORDER BY Medals DESC) AS Third
  FROM Athlete_Medals)

SELECT
-- Get the average medals earned in each third
third,
ROUND(AVG(medals), 2) AS Avg_Medals
FROM Thirds
GROUP BY Third
ORDER BY Third ASC;
# third	avg_medals
# 1	3.7864464692482916
# 2	2.0000000000000000
# 3	2.0000000000000000
# Great! Using NTILE() and summary statistic functions, you could see the differences in the top, middle, and bottom thirds.

# 7 Aggregate window functions.mp4
# Running totals of athlete medals
# The running total (or cumulative sum) of a column helps you determine what each row's contribution is to the total sum.
# Instructions
# - Return the athletes, the number of medals they earned, and the medals running total, ordered by the athletes' names in alphabetical order.
WITH Athlete_Medals AS (
  SELECT
  Athlete, COUNT(*) AS Medals
  FROM Summer_Medals
  WHERE
  Country = 'USA' AND Medal = 'Gold'
  AND Year >= 2000
  GROUP BY Athlete)

SELECT
-- Calculate the running total of athlete medals
Athlete,
Medals,
SUM(Medals) OVER (ORDER BY Athlete ASC) AS Max_Medals
FROM Athlete_Medals
ORDER BY Athlete ASC;
# athlete	            medals	max_medals
# ABDUR-RAHIM Shareef	1     	1
# ABERNATHY Brent   	1	      2
# ADRIAN Nathan	      3	      5
# AHRENS Chris	      1	      6
# AINSWORTH Kurt    	1	      7
# ALLEN Ray	          1	      8
# Great job! You've used your first aggregate window function.

# Maximum country medals by year
# Getting the maximum of a country's earned medals so far helps you determine whether a country has broken its medals record by comparing the current year's earned medals and the maximum so far.
# Instructions
# - Return the year, country, medals, and the maximum medals earned so far for each country, ordered by year in ascending order.
# WITH Country_Medals AS (
SELECT
Year, Country, COUNT(*) AS Medals
FROM Summer_Medals
WHERE
Country IN ('CHN', 'KOR', 'JPN')
AND Medal = 'Gold' AND Year >= 2000
GROUP BY Year, Country)
SELECT
-- Return the max medals earned so far per country
Year,
Country,
Medals,
MAX(Medals) OVER (PARTITION BY Country
                  ORDER BY Year ASC) AS Max_Medals
FROM Country_Medals
ORDER BY Country ASC, Year ASC;
# year	country	medals	max_medals
# 2000	CHN	39	39
# 2004	CHN	52	52
# 2008	CHN	74	74
# 2012	CHN	56	74
# 2000	JPN	5	  5
# 2004	JPN	21	21
# 2008	JPN	23	23
# 2012	JPN	7	  23
# 2000	KOR	12	12
# 2004	KOR	14	14
# 2008	KOR	41	41
# 2012	KOR	18	41
# Good work! As with other window functions, you can use partioning with aggregate window functions.

# Minimum country medals by year
# So far, you've seen MAX and SUM, aggregate functions normally used with GROUP BY, being used as window functions. You can also use the other aggregate functions, like MIN, as window functions.
# Instructions
# - Return the year, medals earned, and minimum medals earned so far.
WITH France_Medals AS (
  SELECT
  Year, COUNT(*) AS Medals
  FROM Summer_Medals
  WHERE
  Country = 'FRA'
  AND Medal = 'Gold' AND Year >= 2000
  GROUP BY Year)
SELECT
Year,
Medals,
MIN(Medals) OVER (ORDER BY Year ASC) AS Min_Medals
FROM France_Medals
ORDER BY Year ASC;
# year	medals	min_medals
# 2000	22	    22
# 2004	21	    21
# 2008	25	    21
# 2012	30	    21
# AK: min from alls
SELECT
Year,
Medals,
MIN(Medals) OVER (ORDER BY Year ASC RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS Min_Medals
FROM France_Medals
ORDER BY Year ASC;
# year	medals	min_medals
# 2000	22    	21
# 2004	21	    21
# 2008	25    	21
# 2012	30    	21
# Well done! All aggregate functions can be used as window functions.

# 8 Frames.mp4
# Number of rows in a frame
# How many rows does the following frame span?
# ROWS BETWEEN 3 PRECEDING AND 2 FOLLOWING
# Answer the question
# Possible Answers
# - 5
# - 6 +
# - 4
# Correct! This frame spans the 3 rows preceding the current row, the current row itself, and the two rows after it.

# Moving maximum of Scandinavian athletes' medals
# Frames allow you to restrict the rows passed as input to your window function to a sliding window for you to define the start and finish.
# Adding a frame to your window function allows you to calculate "moving" metrics, inputs of which slide from row to row.
# Instructions
# - Return the year, medals earned, and the maximum medals earned, comparing only the current year and the next year.
WITH Scandinavian_Medals AS (
  SELECT
  Year, COUNT(*) AS Medals
  FROM Summer_Medals
  WHERE
  Country IN ('DEN', 'NOR', 'FIN', 'SWE', 'ISL')
  AND Medal = 'Gold'
  GROUP BY Year)
SELECT
-- Select each year's medals
  Year,
  Medals,
  -- Get the max of the current and next years'  medals
MAX(Medals) OVER (ORDER BY Year ASC
                  ROWS BETWEEN CURRENT ROW
                  AND 1 FOLLOWING) AS Max_Medals
FROM Scandinavian_Medals
ORDER BY Year ASC;
# year	medals	max_medals
# 1896	1	1
# 1900	1	77
# 1908	77	141
# 1912	141	159
# 1920	159	159
# 1924	48	48
# 1928	24	24
# 1932	17	17
# 1936	15	54
# 1948	54	54
# 1952	39	39
# 1956	19	19
# 1960	7	13
# 1964	13	13
# 1968	13	13
# 1972	10	18
# 1976	18	18
# 1980	10	10
# 1984	7	7
# 1988	6	7
# 1992	7	28
# 1996	28	46
# 2000	46	46
# 2004	29	29
# 2008	23	23
# 2012	20	20
# Good job! You've defined your first frame and restricted the window function's input rows.

# Moving maximum of Chinese athletes' medals
# Frames allow you to "peek" forwards or backward without first using the relative fetching functions, LAG and LEAD, to fetch previous rows' values into the current row.
# Instructions
# - Return the athletes, medals earned, and the maximum medals earned, comparing only the last two and current athletes, ordering by athletes' names in alphabetical order.
WITH Chinese_Medals AS (
  SELECT
  Athlete, COUNT(*) AS Medals
  FROM Summer_Medals
  WHERE
  Country = 'CHN' AND Medal = 'Gold'
  AND Year >= 2000
  GROUP BY Athlete)
SELECT
-- Select the athletes and the medals they've earned
  Athlete,
  Medals,
  -- Get the max of the last two and current rows' medals 
MAX(Medals) OVER (ORDER BY Athlete ASC
                  ROWS BETWEEN 2 PRECEDING
                  AND CURRENT ROW) AS Max_Medals
FROM Chinese_Medals
ORDER BY Athlete ASC;
# athlete	medals	max_medals
# CAI Yalin	    1	1
# CAI Yun	      1	1
# CAO Lei	      1	1
# CAO Yuan    	1	1
# CHEN Ding   	1	1
# CHEN Jing	    1	1
# CHEN Qi	      1	1
# CHEN Ruolin 	4	4
# CHEN Xiaomin	1	4
# CHEN Xiexia	  1	4
# CHEN Yanqing	2	2
# CHEN Yibing	  3	3
# CHEN Ying	    1	3
# CHEN Zhong  	2	3
# Great! You've used all three clauses used to define a frame's start and end.

# 9 Moving averages and totals.mp4
# Moving average's frame
# If you want your moving average to cover the last 3 and current Olympic games, how would you define its frame?
# Answer the question
# Possible Answers
# - ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING
# - ROWS BETWEEN 3 PRECEDING AND 1 PRECEDING
# - ROWS BETWEEN 3 PRECEDING AND CURRENT ROW
# Correct! This covers the current Olympic games, as well as the three preceding sets of games.

# Moving average of Russian medals
# Using frames with aggregate window functions allow you to calculate many common metrics, including moving averages and totals. These metrics track the change in performance over time.
# Instructions
# - Calculate the 3-year moving average of medals earned.
WITH Russian_Medals AS (
  SELECT
  Year, COUNT(*) AS Medals
  FROM Summer_Medals
  WHERE
  Country = 'RUS'
  AND Medal = 'Gold'
  AND Year >= 1980
  GROUP BY Year)

SELECT
Year, Medals,
--- Calculate the 3-year moving average of medals earned
AVG(Medals) OVER
(ORDER BY Year ASC
  ROWS BETWEEN
  2 PRECEDING AND CURRENT ROW) AS Medals_MA
FROM Russian_Medals
ORDER BY Year ASC;
# year	medals	medals_ma
# 1996	36	    36.0000000000000000
# 2000	66	    51.0000000000000000
# 2004	47	    49.6666666666666667
# 2008	43	    52.0000000000000000
# 2012	47	4   5.6666666666666667
# Great! You've used all three clauses used to define a frame's start and end and calculate a moving average.

# Moving total of countries' medals
# What if your data is split into multiple groups spread over one or more columns in the table? Even with a defined frame, if you can't somehow separate the groups' data, one group's values will affect the average of another group's values.
# Instructions
# - Calculate the 3-year moving sum of medals earned per country.
WITH Country_Medals AS (
  SELECT
  Year, Country, COUNT(*) AS Medals
  FROM Summer_Medals
  GROUP BY Year, Country)

SELECT
Year, Country, Medals,
-- Calculate each country s 3-game moving total
  SUM(Medals) OVER
    (PARTITION BY Country
     ORDER BY Year ASC
     ROWS BETWEEN
     2 PRECEDING AND CURRENT ROW) AS Medals_MA
FROM Country_Medals
ORDER BY Country ASC, Year ASC;
# year	country	medals	medals_ma
# 2008	AFG	1	1
# 2012	AFG	1	2
# 1988	AHO	1	1
# 1984	ALG	2	2
# 1992	ALG	2	4
# 1996	ALG	3	7
# 2000	ALG	5	10
# 2008	ALG	2	10
# 2012	ALG	1	8
# Good job! You can also use partitioning with frames.

# 10 Pivoting.mp4
# A basic pivot
# You have the following table of Pole Vault gold medalist countries by gender in 2008 and 2012.
# | Gender | Year | Country |
#  |--------|------|---------|
#  | Men    | 2008 | AUS     |
#  | Men    | 2012 | FRA     |
#  | Women  | 2008 | RUS     |
#  | Women  | 2012 | USA     |
#  Pivot it by Year to get the following reshaped, cleaner table.
#| Gender | 2008 | 2012 |
#  |--------|------|------|
#  | Men    | AUS  | FRA  |
#  | Women  | RUS  | USA  |
# Instructions
# - Create the correct extension.
# - Fill in the column names of the pivoted table.
-- Create the correct extention to enable CROSSTAB
CREATE EXTENSION IF NOT EXISTS tablefunc;

SELECT * FROM CROSSTAB($$
  SELECT
    Gender, Year, Country
  FROM Summer_Medals
  WHERE
    Year IN (2008, 2012)
    AND Medal = 'Gold'
    AND Event = 'Pole Vault'
  ORDER By Gender ASC, Year ASC;
-- Fill in the correct column names for the pivoted table
$$) AS ct (Gender VARCHAR,
          "2008" VARCHAR,
          "2012" VARCHAR)
ORDER BY Gender ASC;
# gender	2008	2012
# Men	    AUS	  FRA
# Women	  RUS	  USA
# Good job! You've pivoted a simple table by a column. Try a more complex table in the next exercise.

# Pivoting with ranking
# You want to produce an easy scannable table of the rankings of the three most populous EU countries by how many gold medals they've earned in the 2004 through 2012 Olympic games. The table needs to be in this format:
# | Country | 2004 | 2008 | 2012 |
# |---------|------|------|------|
# | FRA     | ...  | ...  | ...  |
# | GBR     | ...  | ...  | ...  |
# | GER     | ...  | ...  | ...  |
# You'll need to count the gold medals each country has earned, produce the ranks of each country by medals earned, then pivot the table to this shape.
# Instructions 1/3
# - Count the gold medals that France (FRA), the UK (GBR), and Germany (GER) have earned per country and year.
-- Count the gold medals per country and year
SELECT
Country,
Year,
COUNT(*) AS Awards
FROM Summer_Medals
WHERE
Country IN ('FRA', 'GBR', 'GER')
AND Year IN (2004, 2008, 2012)
AND Medal = 'Gold'
GROUP BY Year, Country
ORDER BY Country ASC, Year ASC;
# country	year	awards
# FRA	2004	21
# FRA	2008	25
# FRA	2012	30
# GBR	2004	17
# GBR	2008	31
# GBR	2012	48
# GER	2004	41
# GER	2008	42
# GER	2012	45
# - Select the country and year columns, then rank the three countries by how many gold medals they earned per year.
WITH Country_Awards AS (
  SELECT
  Country,
  Year,
  COUNT(*) AS Awards
  FROM Summer_Medals
  WHERE
  Country IN ('FRA', 'GBR', 'GER')
  AND Year IN (2004, 2008, 2012)
  AND Medal = 'Gold'
  GROUP BY Country, Year)

SELECT
-- Select Country and Year
Country,
Year, Awards,
-- Rank by gold medals earned per year
RANK() OVER (PARTITION BY Country ORDER BY Awards DESC) :: INTEGER AS rank # BY Country not correct in this case
FROM Country_Awards
ORDER BY Country ASC, Year ASC;
# country	year	awards	rank
# FRA	    2004	21	    3
# FRA	    2008	25	    2
# FRA	    2012	30	    1
# GBR	    2004	17	    3
# GBR	    2008	31	    2
# GBR	    2012	48	    1
# GER	    2004	41	    3
# GER	    2008	42	    2
# GER	    2012	45	    1
WITH Country_Award AS (...)
SELECT
-- Select Country and Year
Country,
Year, Awards,
-- Rank by gold medals earned per year
RANK() OVER (PARTITION BY Year ORDER BY Awards DESC) :: INTEGER AS rank
FROM Country_Awards
ORDER BY Country ASC, Year ASC;
# country	year	awards	rank
# FRA	    2004	21    	2
# FRA	    2008	25    	3
# FRA	    2012	30    	3
# GBR	    2004	17    	3
# GBR	    2008	31    	2
# GBR	    2012	48    	1
# GER	    2004	41    	1
# GER	    2008	42    	1
# GER	    2012	45    	2
# - Pivot the query's results by Year by filling in the new table's correct column names.
CREATE EXTENSION IF NOT EXISTS tablefunc;

SELECT * FROM CROSSTAB($$
  WITH Country_Awards AS (
    SELECT
      Country,
      Year,
      COUNT(*) AS Awards
    FROM Summer_Medals
    WHERE
      Country IN ('FRA', 'GBR', 'GER')
      AND Year IN (2004, 2008, 2012)
      AND Medal = 'Gold'
    GROUP BY Country, Year)
  SELECT
    Country,
    Year,
    RANK() OVER
      (PARTITION BY Year ORDER BY Awards DESC) :: INTEGER AS rank
  FROM Country_Awards
  ORDER BY Country ASC, Year ASC;
-- Fill in the correct column names for the pivoted table
$$) AS ct (Country VARCHAR,
          "2004" INTEGER,
          "2008" INTEGER,
          "2012" INTEGER)
Order by Country ASC;
# country	2004	2008	2012
# FRA	    2	    3	    3
# GBR	    3	    2	    1
# GER	    1	    1	    2
# AK: pivot for medals qquantity
...
SELECT
  Country,
  Year, Awards :: INTEGER
  --RANK() OVER
  --  (PARTITION BY Year
  --   ORDER BY Awards DESC) :: INTEGER AS rank
FROM Country_Awards
ORDER BY Country ASC, Year ASC;
-- Fill in the correct column names for the pivoted table
$$) AS ct (Country VARCHAR,
           "2004" INTEGER,
           "2008" INTEGER,
           "2012" INTEGER)
Order by Country ASC;
# country	2004	2008	2012
# FRA	    21	  25	  30
# GBR	    17	  31	  48
# GER	    41	  42	  45
# Well done! The pivoted rankings table is very easy to scan.

# 11 ROLLUP and CUBE.mp4
# Country-level subtotals
# You want to look at three Scandinavian countries' earned gold medals per country and gender in the year 2004. You're also interested in Country-level subtotals to get the total medals earned for each country, but Gender-level subtotals don't make much sense in this case, so disregard them.
# Instructions
# - Count the gold medals awarded per country and gender.
# - Generate Country-level gold award counts.
-- Count the gold medals per country and gender
SELECT
Country,
Gender,
COUNT(*) AS Gold_Awards
FROM Summer_Medals
WHERE
Year = 2004
AND Medal = 'Gold'
AND Country IN ('DEN', 'NOR', 'SWE')
-- Generate Country-level subtotals
GROUP BY Country, ROLLUP(Gender)
ORDER BY Country ASC, Gender ASC;
# country	gender	gold_awards
# DEN	    Men	    4
# DEN	    Women	  15
# DEN	    null	  19
# NOR	    Men	    3
# NOR	    Women	  2
# NOR	    null	  5
# SWE	    Men	    4
# SWE	    Women	  1
# SWE	    null	  5

# All group-level subtotals
# You want to break down all medals awarded to Russia in the 2012 Olympic games per gender and medal type. Since the medals all belong to one country, Russia, it makes sense to generate all possible subtotals (Gender- and Medal-level subtotals), as well as a grand total.
# Generate a breakdown of the medals awarded to Russia per country and medal type, including all group-level subtotals and a grand total.
# Instructions
# - Count the medals awarded per gender and medal type.
# - Generate all possible group-level counts (per gender and medal type subtotals and the grand total).
-- Count the medals per gender and medal type
SELECT
Gender,
Medal,
COUNT(*) AS Awards
FROM Summer_Medals
WHERE
Year = 2012
AND Country = 'RUS'
-- Get all possible group-level subtotals
GROUP BY CUBE(Gender, Medal)
ORDER BY Gender ASC, Medal ASC;
# gender	medal	  awards
# Men	    Bronze	34
# Men	    Gold	  23
# Men	    Silver	7
# Men	    null  	64
# Women	  Bronze	17
# Women	  Gold	  24
# Women	  Silver	25
# Women	  null	  66
# null	  Bronze	51
# null	  Gold	  47
# null	  Silver	32
# null	  null	  130
# Excellent! You now know when to use ROLLUP and when to use CUBE.

# 12 A survey of useful functions.mp4
# Cleaning up results
# Returning to the breakdown of Scandinavian awards you previously made, you want to clean up the results by replacing the nulls with meaningful text.
# Instructions
# - Turn the nulls in the Country column to All countries, and the nulls in the Gender column to All genders.
SELECT
-- Replace the nulls in the columns with meaningful text
COALESCE(Country, 'All countries') AS Country,
COALESCE(Gender, 'All genders') AS Gender,
COUNT(*) AS Awards
FROM Summer_Medals
WHERE
Year = 2004
AND Medal = 'Gold'
AND Country IN ('DEN', 'NOR', 'SWE')
GROUP BY ROLLUP(Country, Gender)
ORDER BY Country ASC, Gender ASC;
# country	      gender	    awards
# All countries	All genders	29
# DEN	          All genders	19
# DEN	          Men	        4
# DEN         	Women	      15
# NOR	          All genders	5
# NOR	          Men	        3
# NOR	          Women	      2
# SWE	          All genders	5
# SWE	          Men	        4
# SWE	          Women	      1
# Good job! You've made ROLLUP's output become clean and readable.
# AK: pivot this
CREATE EXTENSION IF NOT EXISTS tablefunc;
SELECT * from CROSSTAB ($$
                          SELECT
                        -- Replace the nulls in the columns with meaningful text
                        COALESCE(Country, 'All countries') :: VARCHAR AS Country,
                        COALESCE(Gender, 'All genders') :: VARCHAR AS Gender,
                        COUNT(*) :: INTEGER AS Awards
                        FROM Summer_Medals
                        WHERE
                        Year = 2004
                        AND Medal = 'Gold'
                        AND Country IN ('DEN', 'NOR', 'SWE')
                        GROUP BY CUBE(Country, Gender)
                        ORDER BY Country ASC, Gender ASC;
                        $$) AS ct (Country VARCHAR,
                                   "All genders" INTEGER,
                                   "Men" INTEGER,
                                   "Women" INTEGER);
# country	      All genders	Men	Women
# All countries	29	        11	18
# DEN	          19	        4	  15
# NOR	          5	          3	  2
# SWE	          5	          4	  1

# Summarizing results
# After ranking each country in the 2000 Olympics by gold medals awarded, you want to return the top 3 countries in one row, as a comma-separated string. In other words, turn this:
# | Country | Rank |
# |---------|------|
# | USA     | 1    |
# | RUS     | 2    |
# | AUS     | 3    |
# | ...     | ...  |
# into this:
# USA, RUS, AUS
# Instructions 1/2
# - Rank countries by the medals they've been awarded.
WITH Country_Medals AS (
  SELECT
  Country,
  COUNT(*) AS Medals
  FROM Summer_Medals
  WHERE Year = 2000
  AND Medal = 'Gold'
  GROUP BY Country)
SELECT
Country, Medals,
-- Rank countries by the medals awarded
RANK() OVER (ORDER BY Medals DESC) AS Rank
FROM Country_Medals
ORDER BY Rank ASC;
# country	medals	rank
# USA	    130   	1
# RUS	    66    	2
# AUS	    60    	3
# CHN	    39    	4
# - Return the top 3 countries by medals awarded as one comma-separated string.
WITH Country_Medals AS (
  SELECT
  Country,
  COUNT(*) AS Medals
  FROM Summer_Medals
  WHERE Year = 2000
  AND Medal = 'Gold'
  GROUP BY Country),

Country_Ranks AS (
  SELECT
  Country,
  RANK() OVER (ORDER BY Medals DESC) AS Rank
  FROM Country_Medals
  ORDER BY Rank ASC)

-- Compress the countries column
SELECT STRING_AGG(Country, ', ')
FROM Country_Ranks
-- Select only the top three ranks
WHERE rank <= 3;
# string_agg
# USA, RUS, AUS
# Great! You've compressed three rows into one, losing no information in the process.

























































































































  
  
  


  
  
  







































