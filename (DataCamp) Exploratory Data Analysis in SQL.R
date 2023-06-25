
# fortune500
# rank	title	              name	                  ticker	url	hq	                          sector	                  industry	                                employees	revenues	revenues_change	profits	profits_change	assets	equity
# 1	    Walmart	            Wal-Mart Stores, Inc.	  WMT  	  http://www.walmart.com	          Bentonville, AR	Retailing	General Merchandisers	                    2300000	  485873	  0.8	            13643	  -7.2	          198825	77798
# 2	    Berkshire Hathaway	Berkshire Hathaway Inc.	BRKA  	http://www.berkshirehathaway.com	Omaha, NE	Financials	    Insurance: Property and Casualty (Stock)	367700	  223604	  6.1	            24074	  0	              620854	283001
# 3	    Apple	              Apple, Inc.	            AAPL  	http://www.apple.com	            Cupertino, CA	Technology	Computers, Office Equipment	              116000	  215639	  -7.7	          45687	  -14.4	          321686	128249
# 4	    Exxon Mobil	        Exxon Mobil Corporation	XOM   	http://www.exxonmobil.com	        Irving, TX	              Energy	Petroleum Refining	              72700	    205004	  -16.7	          7840	  -51.5	          330314	167325
# 5	    McKesson	          McKesson Corporation	  MCK   	http://www.mckesson.com	          San Francisco, CA	        Wholesalers	Wholesalers: Health Care	    68000	    192487	  6.2	            2258	  53	            56563	  8924
# company
# id	exchange	ticker	name	parent_id
# 1	nasdaq	PYPL 	PayPal Holdings Incorporated	null
# 2	nasdaq	AMZN 	Amazon.com Inc	null
# 3	nasdaq	MSFT 	Microsoft Corp.	null
# 4	nasdaq	MDB  	MongoDB	null
# tag_type
# id                    	tag	type
# 1	amazon-cloudformation	cloud
# 2	amazon-cloudfront	    cloud
# 3	amazon-cloudsearch  	cloud
# 4	amazon-cloudwatch	    cloud
# 5	amazon-cognito	      cloud
# 6	amazon-cognito	      identity
# 7	amazon-data-pipeline	cloud
# 8	amazon-dynamodb	      cloud
# 9	amazon-dynamodb	      database
# stackoverflow
# id	tag	        date	      question_count	question_pct	unanswered_count	unanswered_pct
# 1	  paypal	    2018-09-25	18050	          0.001093757	  8402	            0.001751857
# 2	  amazon-elb	2018-09-25	1452	          0.000088	    561	              0.000116972
# 3	  amazon-mws	2018-09-25	706	            0.0000428	    278	              0.000058
# 4	  amazon-swf	2018-09-25	232	            0.0000141	    77	              0.0000161
# 5	  amazon-sns	2018-09-25	1400	          0.0000848	    601	              0.000125312
# tag_company
# tag	                  company_id
# actionscript	        10
# actionscript-3	      10
# amazon	              2
# amazon-api	          2
# amazon-appstore	      2
# amazon-cloudformation	12

# Count missing values
# Which column of fortune500 has the most missing values? To find out, you'll need to check each column individually, although here we'll check just three.
# Course Note: While you're unlikely to encounter this issue during this exercise, note that if you run a query that takes more than a few seconds to execute, your session may expire or you may be disconnected from the server. You will not have this issue with any of the exercise solutions, so if your session expires or disconnects, there's an error with your query.
# Instructions 1/4
# - First, figure out how many rows are in fortune500 by counting them.
-- Select the count of the number of rows
SELECT COUNT(*)
FROM fortune500;
# count
# 500
# - Subtract the count of the non-NULL ticker values from the total number of rows; alias the difference as missing.
-- Select the count of ticker, 
-- subtract from the total number of rows, 
-- and alias as missing
SELECT count(*) - COUNT(ticker) AS missing
FROM fortune500;
# missing
# 32
# AK
with a as (select * from fortune500 limit 50)
select * from a where ticker is null;
# rank	title	name	                                      ticker	url	hq	sector	industry	employees	revenues	revenues_change	profits	profits_change	assets	equity
# 33	State Farm Insurance Cos.	State Farm Insurance Cos.	null	http://www.statefarm.com	Bloomington, IL	Financials	Insurance: Property and Casualty (Mutual)	68234	76132	0.6	350.3	-94.4	256030	87592
# 41	Dell Technologies	Dell Technologies Inc.          	null	http://www.delltechnologies.com	Round Rock, TX	Technology	Computers, Office Equipment	138000	64806	18.1	-1672	null	118206	13243
# 49	Albertsons Cos.	Albertsons Companies, Inc.        	null	http://www.albertsons.com
# AK
with a as (select * from fortune500 limit 50)
select COUNT(*), COUNT(ticker), COUNT(*) - COUNT(ticker) from a;
# count	count	?column?
# 50  	47	  3
# - Repeat for the profits_change column.
-- Select the count of profits_change, 
-- subtract from total number of rows, and alias as missing
select count(*) - count(profits_change) as missing
from fortune500;
# missing
# 63
# - Repeat for the industry column.
-- Select the count of industry, 
-- subtract from total number of rows, and alias as missing
select count(*) - count(industry) as missing
from fortune500;
# missing
# 13
# Good work! Note that the result of a call to count(*) is labelled count in the result. You can refer
# to this column with count in ORDER BY clauses without explicitly aliasing count(*) AS count.

# Join tables
# Part of exploring a database is figuring out how tables relate to each other. The company and fortune500 tables don't have a formal relationship between them in the database, but this 
# doesn't prevent you from joining them.
# To join the tables, you need to find a column that they have in common where the values are consistent across the tables. Remember: just because two tables have a column with the same name, it doesn't mean those columns necessarily contain compatible data. If you find more than one pair of columns with similar data, you may need to try joining with each in turn to see if you get the same number of results.
# Reference the entity relationship diagram if needed.
# Instructions
# - Look at the contents of the company and fortune500 tables. Find a column that they have in common where the values for each company are the same in both tables.
# - Join the company and fortune500 tables with an INNER JOIN.
# - Select only company.name for companies that appear in both tables.
SELECT company.name
-- Table(s) to select from
FROM company
INNER JOIN fortune500
ON fortune500.ticker = company.ticker;
# name
# Apple Incorporated
# Amazon.com Inc
# Alphabet
# Microsoft Corp.
# International Business Machines Corporation
# PayPal Holdings Incorporated
# Adobe Systems Incorporated
# You got it! You can join tables when they share a column with consistent data values.

# 2 The keys to the database.mp4
# Foreign keys
# Recall that foreign keys reference another row in the database via a unique ID. Values in a foreign key column are restricted to values in the referenced column OR NULL.
# Using what you know about foreign keys, why can't the tag column in the tag_type table be a foreign key that references the tag column in the stackoverflow table?
# Remember, you can reference the slides using the icon in the upper right of the screen to review the requirements for a foreign key.
# Instructions
# Possible Answers
# - stackoverflow.tag is not a primary key
#   Incorrect Submission
#   Foreign keys often reference primary keys, but it isn't a requirement. What feature of primary keys is a requirement?
# - tag_type.tag contains NULL values
#   Incorrect Submission
#   Foreign key column can contain NULL to indicate no relationship.
# - stackoverflow.tag contains duplicate values +
#   Correct
#   Brillant! Foreign keys must reference a column with unique values for each row so the referenced row can be identified.
# - tag_type.tag does not contain all the values in stackoverflow.tag
#   Incorrect Submission
#   A foreign key column can only contain values in the referenced column, but it does not need to contain all of them.

# Read an entity relationship diagram
# The information you need is sometimes split across multiple tables in the database.
# What is the most common stackoverflow tag_type? What companies have a tag of that type?
# To generate a list of such companies, you'll need to join three tables together.
# Reference the entity relationship diagram as needed when determining which columns to use when joining tables.
# Instructions 1/2
# - First, using the tag_type table, count the number of tags with each type.
# - Order the results to find the most common tag type.
-- Count the number of tags with each type
SELECT type, COUNT(*) AS count
FROM tag_type
-- To get the count for each type, what do you need to do?
  GROUP BY type
-- Order the results with the most common
-- tag types listed first
ORDER BY count desc;
# type	      count
# cloud	      31
# database	  6
# payment 	  5
# mobile-os	  4
# api	        4
# company 	  4
# storage 	  2
# os	        2
# spreadsheet	2
# identity	  1
# - Join the tag_company, company, and tag_type tables, keeping only mutually occurring records.
# - Select company.name, tag_type.tag, and tag_type.type for tags with the most common type from the previous step.
-- Select the 3 columns desired
SELECT company.name, tag_type.tag, tag_type.type
FROM company
-- Join to the tag_company table
INNER JOIN tag_company 
ON company.id = tag_company.company_id
-- Join to the tag_type table
INNER JOIN tag_type
ON tag_company.tag = tag_type.tag
-- Filter to most common type
WHERE type='cloud';
# name	              tag	                        type
# Amazon Web Services	amazon-cloudformation	      cloud
# Amazon Web Services	amazon-cloudfront	          cloud
# Amazon Web Services	amazon-cloudsearch	        cloud
# Amazon Web Services	amazon-cloudwatch	          cloud
# Amazon Web Services	amazon-cognito	            cloud
# Amazon Web Services	amazon-data-pipeline	      cloud
# Amazon Web Services	amazon-dynamodb	            cloud
# Amazon Web Services	amazon-ebs	                cloud
# Amazon Web Services	amazon-ec2	                cloud
# Amazon Web Services	amazon-ecs	                cloud
# Amazon Web Services	amazon-elastic-beanstalk	  cloud
# Amazon Web Services	amazon-elasticache	        cloud
# Amazon Web Services	amazon-elb	                cloud
# Amazon Web Services	amazon-emr	                cloud
# Amazon Web Services	amazon-glacier	            cloud
# Amazon Web Services	amazon-kinesis	            cloud
# Amazon Web Services	amazon-lambda	              cloud
# Amazon Web Services	amazon-rds-aurora	          cloud
# Amazon Web Services	amazon-rds	                cloud
# Amazon Web Services	amazon-redshift	            cloud
# Amazon Web Services	amazon-route53	            cloud
# Amazon Web Services	amazon-s3	                  cloud
# Amazon Web Services	amazon-ses	                cloud
# Amazon Web Services	amazon-simpledb	            cloud
# Amazon Web Services	amazon-sns	                cloud
# Amazon Web Services	amazon-sqs	                cloud
# Amazon Web Services	amazon-swf	                cloud
# Amazon Web Services	amazon-vpc	                cloud
# Amazon Web Services	amazon-web-services	        cloud
# Microsoft Corp.	    azure	                      cloud
# Dropbox	            dropbox	                    cloud
# AK: add each tags count
-- Select the 3 columns desired
SELECT company.name, tag_type.tag, tag_type.type, 
(select count(*) from stackoverflow where tag = tag_type.tag) as N
FROM company
-- Join to the tag_company table
INNER JOIN tag_company 
ON company.id = tag_company.company_id
-- Join to the tag_type table
INNER JOIN tag_type
ON tag_company.tag = tag_type.tag
-- Filter to most common type
WHERE type='cloud'
ORDER BY N desc;
# name	              tag	                  type	n
# Amazon Web Services	amazon-cloudsearch	  cloud	987
# Amazon Web Services	amazon-cloudfront	    cloud	987
# Amazon Web Services	amazon-route53	      cloud	987
# Amazon Web Services	amazon-s3	            cloud	987
# Amazon Web Services	amazon-ec2	          cloud	987
# Amazon Web Services	amazon-web-services	  cloud	987
# Microsoft Corp.	    azure	                cloud	985
# Dropbox	            dropbox	              cloud	983
# Amazon Web Services	amazon-elasticache	  cloud	748
# Amazon Web Services	amazon-elb	          cloud	748
# Amazon Web Services	amazon-emr	          cloud	748
# Amazon Web Services	amazon-glacier	      cloud	748
# Amazon Web Services	amazon-cloudformation	cloud	748
# Amazon Web Services	amazon-lambda	        cloud	748
# Superb! You could combine these steps in a single query by using a subquery in the WHERE clause instead of the value 'cloud'.

# Coalesce
# The coalesce() function can be useful for specifying a default or backup value when a column contains NULL values.
# coalesce() checks arguments in order and returns the first non-NULL value, if one exists.
# coalesce(NULL, 1, 2) = 1
# coalesce(NULL, NULL) = NULL
# coalesce(2, 3, NULL) = 2
# In the fortune500 data, industry contains some missing values. Use coalesce() to use the value of sector as the industry when industry is NULL. Then find the most common industry.
# Instructions
# - Use coalesce() to select the first non-NULL value from industry, sector, or 'Unknown' as a fallback value.
# - Alias the result of the call to coalesce() as industry2.
# - Count the number of rows with each industry2 value.
# - Find the most common value of industry2.
-- Use coalesce
SELECT COALESCE(industry, sector, 'Unknown') AS industry2,
-- Don t forget to count!
       COUNT(*) 
  FROM fortune500 
-- Group by what? (What are you counting by?)
 GROUP BY industry2
-- Order results to see most common first
 ORDER BY count desc
-- Limit results to get just the one value you want
limit 1;
# industry2	                                count
# Utilities: Gas and Electric	              22
# AK: all, without limit
# industry2	                                count
# Utilities: Gas and Electric	              22
# Specialty Retailers: Other	              21
# Insurance: Property and Casualty (Stock)	19
# Commercial Banks	                        19
# Chemicals	                                14
# Food Consumer Products	                  13
# Aerospace and Defense	                    12
# Diversified Financials                  	12
# Petroleum Refining	                      10
# Telecommunications	                      10
# Energy	                                  10
# Pharmaceuticals	                          10
# Terrific! coalesce is essential when the value you need could be in more than one column. In the next exercise, you'll use coalesce as part of a self join.

# Coalesce with a self-join
# You previously joined the company and fortune500 tables to find out which companies are in both tables. Now, also include companies from company that are subsidiaries of Fortune 500 
# companies as well.
# To include subsidiaries, you will need to join company to itself to associate a subsidiary with its parent company's information. To do this self-join, use two different aliases for company.
# coalesce will help you combine the two ticker columns in the result of the self-join to join to fortune500.
# Instructions
# - Join company to itself to add information about a company's parent to the original company's information.
# - Use coalesce to get the parent company ticker if available and the original company ticker otherwise.
# - INNER JOIN to fortune500 using the ticker.
# - Select original company name, fortune500 title and rank.
SELECT company_original.name, fortune500.title, fortune500.rank
-- Start with original company information
FROM company AS company_original
-- Join to another copy of company with parent
-- company information
LEFT JOIN company AS company_parent
ON company_original.parent_id = company_parent.id
-- Join to fortune500, only keep rows that match
INNER JOIN fortune500 
-- Use parent ticker if there is one, 
-- otherwise original ticker
ON coalesce(company_original.ticker, 
            company_parent.ticker) = 
  fortune500.ticker
-- For clarity, order by rank
ORDER BY rank;
# name	                                      title	          rank
# Apple Incorporated	                        Apple	          3
# Amazon.com Inc	                            Amazon.com	    12
# Amazon Web Services	                        Amazon.com	    12
# Alphabet	                                  Alphabet	      27
# Google LLC	                                Alphabet	      27
# Microsoft Corp.	                            Microsoft	      28
# International Business Machines Corporation	IBM	            32
# PayPal Holdings Incorporated	              PayPal Holdings	264
# eBay, Inc.	                                eBay	          310
# Adobe Systems Incorporated	                Adobe Systems	  443
# AK: this is ^ a list of companies from stackoverflow (including parent) which are in the list of fortune500
# AK: subquery
SELECT * --company_original.___, ___, ___
-- Start with original company information
FROM company AS company_original
-- Join to another copy of company with parent
-- company information
LEFT JOIN company AS company_parent
ON company_original.parent_id = company_parent.id;
# id	exchange	ticker	name	                                      parent_id	id	exchange	ticker	name	parent_id
# 1	  nasdaq	  PYPL 	  PayPal Holdings Incorporated	              null	    null	null	null	null	null
# 2	  nasdaq  	AMZN 	  Amazon.com Inc                            	null	    null	null	null	null	null
# 3	  nasdaq	  MSFT 	  Microsoft Corp.	                            null	    null	null	null	null	null
# 4	  nasdaq	  MDB  	  MongoDB	                                    null	    null	null	null	null	null
# 5	  nasdaq	  DBX  	  Dropbox	                                    null	    null	null	null	null	null
# 6	  nasdaq	  AAPL 	  Apple Incorporated	                        null	    null	null	null	null	null
# 7	  nasdaq	  CTXS 	  Citrix Systems	                            null	    null	null	null	null	null
# 8	  nasdaq	  GOOGL	  Alphabet	                                  null	    null	null	null	null	null
# 9	  nyse	    IBM  	  International Business Machines Corporation	null	    null	null	null	null	null
# 10	nasdaq	  ADBE 	  Adobe Systems Incorporated	                null	    null	null	null	null	null
# 11	null	    null	  Stripe	                                    null	    null	null	null	null	null
# 12	null	    null	  Amazon Web Services	                        2	        2	nasdaq	AMZN 	Amazon.com Inc	null
# 13	null	    null	  Google LLC	                                8	        8	nasdaq	GOOGL	Alphabet	null
# 14	nasdaq	  EBAY 	  eBay, Inc.	                                null	    null	null	null	null	null
# Awesome! Self-joins can get confusing. Use meaningful aliases to help keep everything straight.

# 3 Column types and constraints.mp4
# Effects of casting
# When you cast data from one type to another, information can be lost or changed. See how the casting changes values and practice casting data using the CAST() function and the :: syntax.
# SELECT CAST(value AS new_type);
# SELECT value::new_type;
# Instructions 1/3
# - Select profits_change and profits_change cast as integer from fortune500.
# - Look at how the values were converted.
-- Select the original value
SELECT profits_change, 
-- Cast profits_change
CAST(profits_change AS INTEGER) AS profits_change_int
FROM fortune500;
# profits_change	profits_change_int
# -7.2	          -7
# 0	              0
# -14.4	          -14
# -51.5	          -52
# 53	            53
# 20.7	          21
# 1.5	            2
# -2.7	          -3
# -2.8	          -3
# -37.7	          -38
# null	          null
# 297.8	          298
# - Compare the results of casting of dividing the integer value 10 by 3 to the result of dividing the numeric value 10 by 3.
-- Divide 10 by 3
SELECT 10::INTEGER/3, 
-- Cast 10 as numeric and divide by 3
10::NUMERIC/3;
# ?column?	?column?
# 3	3.3333333333333333
# - Now cast numbers that appear as text as numeric.
# - Note: 1e3 is scientific notation.
SELECT '3.2'::NUMERIC,
'-123'::NUMERIC,
'1e3'::NUMERIC,
'1e-3'::NUMERIC,
'02314'::NUMERIC,
'0002'::NUMERIC;
# numeric	numeric	numeric	numeric	numeric	numeric
# 3.2	    -123	  1000	  0.001	  2314	  2
# Good job! Note that numbers cast as integer are rounded to the nearest whole number and division produces different results for integer values than for numeric values.

# Summarize the distribution of numeric values
# Was 2017 a good or bad year for revenue of Fortune 500 companies? Examine how revenue changed from 2016 to 2017 by first looking at the distribution of revenues_change and then counting companies whose revenue increased.
# Instructions 1/3
# - Use GROUP BY and count() to examine the values of revenues_change.
# - Order the results by revenues_change to see the distribution.
-- Select the count of each value of revenues_change
SELECT revenues_change, COUNT(*)
FROM fortune500
GROUP BY revenues_change
-- order by the values of revenues_change
ORDER BY revenues_change;
# revenues_change	count
# -57.5	1
# -53.3	1
# -51.4	1
# -50.9	1
# -45	  1
# -41.7	1
# - Repeat step 1, but this time, cast revenues_change as an integer to reduce the number of different values.
# -- Select the count of each revenues_change integer value
SELECT revenues_change::INTEGER, COUNT(*)
FROM fortune500
GROUP BY revenues_change::INTEGER
-- order by the values of revenues_change
ORDER BY revenues_change;
# revenues_change	count
# -58	1
# -53	1
# -51	2
# -45	1
# -42	1
# -39	1
# -38	2
# -33	1
# -30	1
# -27	1
# -23	1
# -21	2
# -20	5
# -18	4
# -17	3
# -16	1
# -15	3
# -14	4
# -13	3
# -12	6
# -11	11
# -10	8
# -9	7
# -8	11
# -7	9
# -6	17
# -5	10
# -4	17
# -3	15
# -2	20
# -1	21
# 0	25
# 1	19
# 2	41
# 3	25
# 4	24
# - How many of the Fortune 500 companies had revenues increase in 2017 compared to 2016? To find out, count the rows of fortune500 where revenues_change indicates an increase.
-- Count rows 
SELECT COUNT(*), (SELECT COUNT(*) FROM fortune500) AS N
FROM fortune500
-- Where...
WHERE revenues_change > 0;
# count	n
# 298	  500
# You got it. Examining distributions and counting observations of interest are two first steps in exploring data. In the next chapter, we'll learn other functions and approaches for summarizing numeric data.

# 4 Numeric data types and summary functions.mp4
# Division
# Compute the average revenue per employee for Fortune 500 companies by sector.
# Instructions
# - Compute revenue per employee by dividing revenues by employees; use casting to produce a numeric result.
# - Take the average of revenue per employee with avg(); alias this as avg_rev_employee.
# - Group by sector.
# - Order by the average revenue per employee.
-- Select average revenue per employee by sector
SELECT sector, 
AVG(revenues::NUMERIC/employees) AS avg_rev_employee
FROM fortune500
GROUP BY sector
-- Use the column alias to order the results
ORDER BY avg_rev_employee;
# sector	                      avg_rev_employee
# Hotels, Restaurants & Leisure	0.09498718151056814829
# Apparel	                      0.27865942976680063809
# Food & Drug Stores	          0.30799950410060207070
# Motor Vehicles & Parts	      0.34252712424659522769
# Household Products	          0.35557338969595350494
# Retailing	                    0.36019456092078082945
# Industrials	                  0.36148543376146347043
# Aerospace & Defense	          0.36671499248628270960
# Transportation	              0.40365352477329589012
# Business Services	            0.42010994210166630178
# Technology	                  0.57311847693359764307
# Chemicals	                    0.59549976658074458280
# Telecommunications	          0.62958997279187493604
# Health Care	                  0.79053286919681465178
# Media	                        0.79561866565463184108
# Food, Beverages & Tobacco	    0.83088475939249850372
# Engineering & Construction	  0.86116376673874818555
# Wholesalers	                  1.41324581189371202678
# Financials	                  1.72638470140972568700
# Energy	                      1.82601423325242362195
# Materials	                    4.75758351554488369167
# Sensational! You know to watch out for integer division problems, and that ordering your query results by the value of interest will help you make sense of the results.

# Explore with division
# In exploring a new database, it can be unclear what the data means and how columns are related to each other.
# What information does the unanswered_pct column in the stackoverflow table contain? Is it the percent of questions with the tag that are unanswered (unanswered ?s with tag/all ?s with tag)? Or is it something else, such as the percent of all unanswered questions on the site with the tag (unanswered ?s with tag/all unanswered ?s)?
# Divide unanswered_count (unanswered ?s with tag) by question_count (all ?s with tag) to see if the value matches that of unanswered_pct to determine the answer.
# stackoverflow
# id	tag	      date	      question_count	question_pct	unanswered_count	unanswered_pct
# 1	paypal	    2018-09-25	18050	          0.001093757	  8402	            0.001751857
# 2	amazon-elb	2018-09-25	1452	          0.000088	    561	              0.000116972
# 3	amazon-mws	2018-09-25	706	            0.0000428	    278	              0.000058
# 4	amazon-swf	2018-09-25	232	            0.0000141	    77	              0.0000161
# 5	amazon-sns	2018-09-25	1400	          0.0000848	    601	              0.000125312
# 6	excel	      2018-09-25	177603	        0.010762031	  61804	            0.012886449
# Instructions
# - Exclude rows where question_count is 0 to avoid a divide by zero error.
# - Limit the result to 10 rows.
-- Divide unanswered_count by question_count
SELECT unanswered_count/question_count::NUMERIC AS computed_pct, 
-- What are you comparing the above quantity to?
  unanswered_pct
FROM stackoverflow
-- Select rows where question_count is not 0
WHERE question_count > 0
LIMIT 10;
# computed_pct	          unanswered_pct
# 0.46548476454293628809	0.001751857
# 0.38636363636363636364	0.000116972
# 0.39376770538243626062	0.000058
# 0.33189655172413793103	0.0000161
# 0.42928571428571428571	0.000125312
# 0.34798961729250068974	0.012886449
# 0.35083862172255878033	0.007619406
# 0.30729166666666666667	0.0000123
# 0.35428051001821493625	0.0000811
# 0.38065776619993487463	0.000243743
# Super! The values don't match. unanswered_pct is the percent of unanswered questions on Stack 
# Overflow with the tag, not the percent of questions with the tag that are unanswered.
# AK: check ^
-- Divide unanswered_count by question_count
SELECT unanswered_count/question_count::NUMERIC AS computed_pct, 
*,
unanswered_count/(SELECT SUM(unanswered_count) FROM stackoverflow s2 WHERE s2.tag = s1.tag)::NUMERIC AS computed_all_pct,
(SELECT SUM(unanswered_count) FROM stackoverflow) as total_unanswered,
(SELECT SUM(unanswered_count) FROM stackoverflow s2 WHERE s2.tag = s1.tag) as total_tag_anunswered,
-- What are you comparing the above quantity to?
  unanswered_pct
FROM stackoverflow s1
-- Select rows where question_count is not 0
WHERE  question_count > 0 and tag = 'paypal'
LIMIT 20;
# computed_pct	          id	tag	    date	      question_count	question_pct	unanswered_count	unanswered_pct	computed_all_pct	      total_unanswered	total_tag_anunswered	unanswered_pct
# 0.46548476454293628809	1	  paypal	2018-09-25	18050	          0.001093757	  8402	            0.001751857	    0.00110671035024445232	796612018	        7591869	              0.001751857
# 0.46530318146546946015	54	paypal	2018-09-24	18042	          0.001093744	  8395	            0.00175155	    0.00110578831115236577	796612018	        7591869	              0.00175155
# 0.46524390243902439024	135	paypal	2018-09-23	18040	          0.001093869	  8393	            0.001751701	    0.00110552487141176962	796612018	        7591869	              0.001751701
# 0.46521425799656300238	212	paypal	2018-09-22	18039	          0.001094045	  8392	            0.001752108	    0.00110539315154147154	796612018	        7591869	              0.001752108
# AK: do not see confirmation to conclusion ^

# Summarize numeric columns
# Summarize the profit column in the fortune500 table using the functions you've learned.
# You can access the course slides for reference using the PDF icon in the upper right corner of the screen.
# Instructions 1/2
# - Compute the min(), avg(), max(), and stddev() of profits.
-- Select min, avg, max, and stddev of fortune500 profits
SELECT MIN(profits),
AVG(profits),
MAX(profits),
stddev(profits)
FROM fortune500;
# min	  avg	                  max	  stddev
# -6177	1783.4753507014028056	45687	3940.495363490788
# - Now repeat step 1, but summarize profits by sector.
# - Order the results by the average profits for each sector.
-- Select sector and summary measures of fortune500 profits
SELECT sector,
MIN(profits),
AVG(profits),
MAX(profits),
stddev(profits)
FROM fortune500
-- What to group by?
  GROUP BY sector
-- Order by the average profits
ORDER BY avg DESC;
# sector	                      min	    avg	                  max	  stddev
# Technology	                  -1672	  4137.2418604651162791	45687	8042.983363606666
# Telecommunications	          -383.5	4127.2800000000000000	13127	5400.731732686270
# Health Care	                  -1721	  2773.2605263157894737	16540	3751.818796086771
# Financials	                  -1128	  2719.7761904761904762	24733	5064.764070852874
# Food, Beverages & Tobacco	    -677	  2346.1833333333333333	14239	3412.352156334481
# Aerospace & Defense	          -941	  2093.3083333333333333	5302	2064.779951937795
# Motor Vehicles & Parts	      -674.9	1919.5333333333333333	9427	3176.300731983670
# Media	                        -495.9	1821.3363636363636364	9391	2839.299478136369
# Industrials	                  -176.1	1727.6894736842105263	8831	2326.018251073599
# Transportation	              69	    1670.2941176470588235	4373	1373.013160657332
# Household Products	          -198.8	1650.3083333333333333	10508	2900.116805001398
# Hotels, Restaurants & Leisure	348	    1451.0600000000000000	4686.5	1372.975732730432
# Apparel	                      396	    1263.7000000000000000	3760	1419.134570786013
# Food & Drug Stores	          -502.2	1217.4285714285714286	4173	1613.041448851915
# Business Services	            57.2	  1155.3550000000000000	5991	1454.360686992199
# Chemicals	                    -3.9	  1137.0214285714285714	4318	1129.752304492226
# Retailing	                    -2221	  991.7851063829787234	13643	2348.342559077222
# Wholesalers	                  -199.4	391.2793103448275862	2258	532.171183776766
# Engineering & Construction	  15	    390.1692307692307692	911.8	277.665120197620
# Materials	                    -440	  272.4684210526315789	1027	406.632781447055
# Energy	                      -6177	  10.4446428571428571	  7840	2264.572142925951

# Summarize group statistics
# Sometimes you want to understand how a value varies across groups. For example, how does the maximum value per group vary across groups?
# To find out, first summarize by group, and then compute summary statistics of the group results. One way to do this is to compute group values in a subquery, and then summarize the results of the subquery.
# For this exercise, what is the standard deviation across tags in the maximum number of Stack Overflow questions per day? What about the mean, min, and max of the maximums as well?
# Instructions
# - Start by writing a subquery to compute the max() of question_count per tag; alias the subquery result as maxval.
# - Then compute the standard deviation of maxval with stddev().
# - Compute the min(), max(), and avg() of maxval too.
-- Compute standard deviation of maximum values
SELECT STDDEV(maxval),
-- min
MIN(maxval),
-- max
MAX(maxval),
-- avg
AVG(maxval)
-- Subquery to compute max of question_count by tag
FROM (SELECT MAX(question_count) AS maxval
      FROM stackoverflow
      -- Compute max by...
      GROUP BY tag) AS max_results; -- alias for subquery
# stddev	        min	max	    avg
# 176458.37952720	30	1138658	52652.433962264151
# Great job summarizing! A subquery was necessary here because the tag maximums must be computed before you can summarize them.

# 5 Exploring distributions.mp4
# Truncate
# Use trunc() to examine the distributions of attributes of the Fortune 500 companies.
# Remember that trunc() truncates numbers by replacing lower place value digits with zeros:
# trunc(value_to_truncate, places_to_truncate)
# Negative values for places_to_truncate indicate digits to the left of the decimal to replace, while positive values indicate digits to the right of the decimal to keep.
# Instructions 1/2
# - Use trunc() to truncate employees to the 100,000s (5 zeros).
# - Count the number of observations with each truncated value.
-- Truncate employees
SELECT TRUNC(employees, -5) AS employee_bin,
-- Count number of companies with each truncated value
COUNT(*)
FROM fortune500
-- Use alias to group
GROUP BY employee_bin
-- Use alias to order
ORDER BY employee_bin;
# employee_bin	count
# 0	            433
# 100000	      35
# 200000	      20
# 300000	      7
# 400000	      4
# 2300000	      1
# - Repeat step 1 for companies with < 100,000 employees (most common).
# - This time, truncate employees to the 10,000s place.
-- Truncate employees
SELECT TRUNC(employees, -4) AS employee_bin,
-- Count number of companies with each truncated value
COUNT(*)
FROM fortune500
-- Limit to which companies?
  WHERE employees < 100000
-- Use alias to group
GROUP BY employee_bin
-- Use alias to order
ORDER BY employee_bin;
# employee_bin	count
# 0	            102
# 10000	        108
# 20000	        63
# 30000	        42
# 40000	        35
# 50000	        31
# 60000	        18
# 70000	        18
# 80000	        6
# 90000	        10
# Awesome! Start exploring a distribution by grouping values into large bins, then refine as needed for ranges of values where there are a lot of observations.

# Generate series
# Summarize the distribution of the number of questions with the tag "dropbox" on Stack Overflow per day by binning the data.
# Recall:
# generate_series(from, to, step)
# You can reference the slides using the PDF icon in the upper right corner of the screen.
# Instructions 1/3
# - Start by selecting the minimum and maximum of the question_count column for the tag 'dropbox' so you know the range of values to cover with the bins.
-- Select the min and max of question_count
SELECT MIN(question_count), 
MAX(question_count)
-- From what table?
  FROM stackoverflow
-- For tag dropbox
WHERE tag = 'dropbox';
# min 	max
# 2315	3072
# Next, use generate_series() to create bins of size 50 from 2200 to 3100.
# - To do this, you need an upper and lower bound to define a bin.
# - This will require you to modify the stopping value of the lower bound and the starting value of the upper bound by the bin width.
-- Create lower and upper bounds of bins
SELECT generate_series(2200, 3050, 50) AS lower,
generate_series(2250, 3100, 50) AS upper;
# lower	upper
# 2200	2250
# 2250	2300
# 2300	2350
# 2350	2400
# - Select lower and upper from bins, along with the count of values within each bin bounds.
# - To do this, you'll need to join 'dropbox', which contains the question_count for tag "dropbox", to the bins created by generate_series().
# - The join should occur where the count is greater than or equal to the lower bound, and strictly less than the upper bound.
-- Bins created in Step 2
WITH bins AS (
  SELECT generate_series(2200, 3050, 50) AS lower,
  generate_series(2250, 3100, 50) AS upper),
-- Subset stackoverflow to just tag dropbox (Step 1)
dropbox AS (
  SELECT question_count 
  FROM stackoverflow
  WHERE tag='dropbox') 
-- Select columns for result
-- What column are you counting to summarize?
  SELECT lower, upper, count(dropbox.question_count) by_not_null_values, count(*) as by_row_number
FROM bins  -- Created above
-- Join to dropbox (created above), 
-- keeping all rows from the bins table in the join
LEFT JOIN dropbox
-- Compare question_count to lower and upper
ON dropbox.question_count >= lower 
AND dropbox.question_count < upper
-- Group by lower and upper to count values in each bin
GROUP BY lower, upper
-- Order by lower to put bins in order
ORDER BY lower;
# lower	upper	by_not_null_values	by_row_number
# 2200	2250	0	                  1
# 2250	2300	0	                  1
# 2300	2350	22	                22
# 2350	2400	39	                39
# 2400	2450	54	                54
# 2450	2500	53	                53
# 2500	2550	45	                45
# 2550	2600	41	                41
# 2600	2650	46	                46
# 2650	2700	57	                57
# 2700	2750	44	                44
# 2750	2800	50	                50
# 2800	2850	62	                62
# 2850	2900	61	                61
# 2900	2950	108	                108
# 2950	3000	159	                159
# 3000	3050	98	                98
# 3050	3100	44	                44
# Awesome! generate_series allows you to group values into any size interval and also include ranges with zero values.

# 6 More summary functions.mp4
# Correlation
# What's the relationship between a company's revenue and its other financial attributes? Compute the correlation between revenues and other financial variables with the corr() function.
# Instructions
# - Compute the correlation between revenues and profits.
# - Compute the correlation between revenues and assets.
# - Compute the correlation between revenues and equity.
-- Correlation between revenues and profit
SELECT corr(revenues, profits) AS rev_profits,
-- Correlation between revenues and assets
corr(revenues, assets) AS rev_assets,
-- Correlation between revenues and equity
corr(revenues, equity) AS rev_equity 
FROM fortune500;
# rev_profits	        rev_assets	        rev_equity
# 0.5999935815724783	0.3294995213185059	0.5465709997184311
# Well done. Profits, assets, and equity are all positvely correlated with revenue for Fortune 500 companies.

# Mean and Median
# Compute the mean (avg()) and median assets of Fortune 500 companies by sector.
# Use the percentile_disc() function to compute the median:
# percentile_disc(0.5) 
# WITHIN GROUP (ORDER BY column_name)
# Instructions
# - Select the mean and median of assets.
# - Group by sector.
# - Order the results by the mean.
-- What groups are you computing statistics by?
  SELECT sector,
-- Select the mean of assets with the avg function
AVG(assets) AS mean,
-- Select the median
percentile_disc(0.5) WITHIN GROUP (ORDER BY assets) AS median
FROM fortune500
-- Computing statistics for each what?
  GROUP BY sector
-- Order results by a value of interest
ORDER BY mean;
# sector	                    mean	                  median
# Engineering & Construction	8199.2307692307692308	  8709
# Wholesalers	                9362.5862068965517241	  5390
# Materials	                  10833.263157894737	    7741
# Apparel	                    11064.8000000000000000	9739
# Phenomenal! The mean and median can differ significantly for skewed distributions that have a few extreme values.

# +++
# 7 Creating temporary tables.mp4
# Create a temp table. Find the Fortune 500 companies that have profits in the top 20% for their sector (compared to other Fortune 500 companies).
# To do this, first, find the 80th percentile of profit for each sector with percentile_disc(fraction) 
# WITHIN GROUP (ORDER BY sort_expression)
# and save the results in a temporary table.
# Then join fortune500 to the temporary table to select companies with profits greater than the 80th percentile cut-off.
# Instructions 1/2
# - Create a temporary table called profit80 containing the sector and 80th percentile of profits for each sector.
# - Alias the percentile column as pct80.
-- To clear table if it already exists;
-- fill in name of temp table
DROP TABLE IF EXISTS profit80;

-- Create the temporary table
CREATE TEMP TABLE profit80 AS 
-- Select the two columns you need; alias as needed
SELECT sector, 
PERCENTILE_DISC(0.8) WITHIN GROUP (ORDER BY profits) AS pct80
-- What table are you getting the data from?
  FROM fortune500
-- What do you need to group by?
  GROUP BY sector;

-- See what you created: select all columns and rows 
-- from the table you created
SELECT * 
  FROM profit80;
# sector	                    pct80
# Aerospace & Defense	        4895
# Apparel	                    1074.1
# Business Services	          1401
# Chemicals	                  1500
# Energy	                    1311
# Engineering & Construction	602.7
# - Using the profit80 table you created in step 1, select companies that have profits greater than pct80.
# - Select the title, sector, profits from fortune500, as well as the ratio of the company's profits to the 80th percentile profit.
-- Code from previous step
DROP TABLE IF EXISTS profit80;

CREATE TEMP TABLE profit80 AS
SELECT sector, 
percentile_disc(0.8) WITHIN GROUP (ORDER BY profits) AS pct80
FROM fortune500 
GROUP BY sector;

-- Select columns, aliasing as needed
SELECT fortune500.title, fortune500.sector, 
fortune500.profits, profit80.pct80, fortune500.profits/profit80.pct80 AS ratio
-- What tables do you need to join?  
FROM fortune500 
LEFT JOIN profit80
-- How are the tables joined?
  ON profit80.sector = fortune500.sector
-- What rows do you want to select?
  WHERE fortune500.profits > profit80.pct80;
# title	              sector	    profits	pct80	ratio
# Walmart	            Retailing	  13643	  1228	11.1099348534201954
# Berkshire Hathaway	Financials	24074	  3014	7.9873921698739217
# Apple	              Technology	45687	  7266	6.2877786952931462
# Exxon Mobil	        Energy	    7840	  1311	5.9801678108314264
# McKesson	          Wholesalers	2258	  605.9	3.7266875722066348
# Thumbs up! Instead of creating a temporary table, you could do this in one step with a subquery. 
# But if you'll use the same subquery multiple times, a temporary table can be a good option.

# Create a temp table to simplify a query
# The Stack Overflow data contains daily question counts through 2018-09-25 for all tags, but each tag has a different starting date in the data.
# Find out how many questions had each tag on the first date for which data for the tag is available, as well as how many questions had the tag on the last day. Also, compute the difference between these two values.
# To do this, first compute the minimum date for each tag.
# Then use the minimum dates to select the question_count on both the first and last day. To do this, join the temp table startdates to two different copies of the stackoverflow table: one for each column - first day and last day - aliased with different names.
# Instructions 1/2
# - First, create a temporary table called startdates with each tag and the min() date for the tag in stackoverflow.
-- To clear table if it already exists
DROP TABLE IF EXISTS startdates;

-- Create temp table syntax
CREATE TEMP TABLE startdates AS
-- Compute the minimum date for each what?
  SELECT tag,
MIN(date) AS mindate
FROM stackoverflow
-- What do you need to compute the min date for each tag?
  GROUP BY tag;

-- Look at the table you created
SELECT * 
  FROM startdates;
# tag	mindate
# amazon-route53	      2016-01-01
# google-spreadsheet	  2016-01-01
# dropbox	              2016-01-01
# amazon-data-pipeline	2016-09-01
# amazon	              2016-01-01
# amazon-sns	          2016-09-01
# - Join startdates to stackoverflow twice using different table aliases.
# - For each tag, select mindate, question_count on the mindate, and question_count on 2018-09-25 (the max date).
# - Compute the change in question_count over time.
-- To clear table if it already exists
DROP TABLE IF EXISTS startdates;

CREATE TEMP TABLE startdates AS
SELECT tag, min(date) AS mindate
FROM stackoverflow
GROUP BY tag;

-- Select tag (Remember the table name!) and mindate
SELECT startdates.tag, 
startdates.mindate, 
-- Select question count on the min and max days
so_min.question_count AS min_date_question_count,
so_max.question_count AS max_date_question_count,
-- Compute the change in question_count (max- min)
so_max.question_count - so_min.question_count AS change
FROM startdates
-- Join startdates to stackoverflow with alias so_min
INNER JOIN stackoverflow AS so_min
-- What needs to match between tables?
  ON startdates.tag = so_min.tag
AND startdates.mindate = so_min.date
-- Join to stackoverflow again with alias so_max
INNER JOIN stackoverflow AS so_max
-- Again, what needs to match between tables?
  ON startdates.tag = so_max.tag
AND so_max.date = '2018-09-25';
# tag	                      mindate	    min_date_question_count	max_date_question_count	change
# paypal	                  2016-01-01	13296	                  18050	                  4754
# amazon-elb              	2016-09-01	576	1452	876
# amazon-mws	              2016-09-01	367	706	339
# amazon-swf	              2016-09-01	167	232	65
# amazon-sns	              2016-09-01	690	1400	710
# excel	                    2016-01-01	81384	177603	96219
# mongodb	                  2016-01-01	55510	104159	48649
# amazon-glacier	          2016-09-01	118	192	74
# amazon-route53	          2016-01-01	369	1098	729
# dropbox	                  2016-01-01	2319	3071	752
# azure	                    2016-01-01	25859	61259	35400
# sql-server	              2016-01-01	151267	242484	91217
# amazon-cloudwatch	        2016-09-01	334	1165	831
# amazon-redshift	          2016-09-01	1448	3502	2054
# amazon-cognito	          2016-09-01	547	2830	2283
# amazon-dynamodb	          2016-09-01	2687	5985	3298
# ios9	                    2016-01-01	3147	4183	1036
# amazon-sqs	              2016-09-01	855	1584	729
# dropbox-api	              2016-01-01	1437	2072	635
# amazon-ses	              2016-09-01	481	934	453
# amazon-emr	              2016-09-01	557	3046	2489
# citrix	                  2016-01-01	465	597	132
# amazon-elastic-beanstalk	2016-09-01	281	5490	5209
# amazon-rds	              2016-09-01	1156	2537	1381
# amazon-rds-aurora	        2016-09-01	60	323	263
# android	                  2016-01-01	770606	1138658	368052
# amazon-cloudformation	    2016-09-01	592	2516	1924
# ios	                      2016-01-01	397485	577421	179936
# android-pay	              2017-03-17	444	490	46
# applepay	                2017-03-18	222	357	135
# windows	                  2016-01-01	88874	124751	35877
# amazon-web-services	      2016-01-01	22077	63939	41862
# google-spreadsheet	      2016-01-01	6531	15685	9154
# amazon-cloudfront	        2016-01-01	1099	2264	1165
# amazon-lambda	            2016-09-01	158	8032	7874
# amazon-cloudsearch	      2016-01-01	176	318	142
# amazon-ecs	              2016-09-01	145	1074	929
# amazon-s3	                2016-01-01	12015	25443	13428
# amazon-elasticache	      2016-09-01	257	434	177
# amazon-simpledb	          2016-09-01	374	392	18
# amazon-data-pipeline	    2016-09-01	195	399	204
# amazon-ec2	              2016-01-01	11632	22755	11123
# amazon-ebs	              2016-09-01	257	338	81
# ios8	                    2016-01-01	9854	9399	-455
# amazon-kinesis	          2016-09-01	259	766	507
# cognos	                  2016-01-01	616	848	232
# actionscript	            2016-01-01	8761	9101	340
# amazon	                  2016-01-01	2994	3636	642
# osx	                      2016-01-01	62614	87485	24871
# stripe-payments	          2016-01-01	1736	4964	3228
# actionscript-3	          2016-01-01	39025	41149	2124
# amazon-vpc	              2016-09-01	339	719	380
# applepayjs	              2017-03-18	11	30	19
# Sensational! The main query here was already complicated, so creating the temporary table first helped simplify the analysis.

# Insert into a temp table
# While you can join the results of multiple similar queries together with UNION, sometimes it's easier to break a query down into steps. You can do this by creating a temporary table and inserting rows into it.
# Compute the correlations between each pair of profits, profits_change, and revenues_change from the Fortune 500 data.
# The resulting temporary table should have the following structure:
#   measure	        profits	profits_change	revenues_change
#   profits	        1.00	  #	              #
#   profits_change	#	      1.00	          #
#   revenues_change	#	      #	              1.00
# Recall the round() function to make the results more readable:
#   round(column_name::numeric, decimal_places)
# Note that Steps 1 and 2 do not produce output. It is normal for the query result pane to say "Your query did not generate any results."
# Instructions 1/3
# - Create a temp table correlations.
# - Compute the correlation between profits and each of the three variables (i.e. correlate profits with profits, profits with profits_change, etc).
# - Alias columns by the name of the variable for which the correlation with profits is being computed.
DROP TABLE IF EXISTS correlations;

-- Create temp table 
CREATE TEMP TABLE correlations AS
-- Select each correlation
SELECT 'profits'::varchar AS measure,
-- Compute correlations
corr(profits, profits) AS profits,
corr(profits, profits_change) AS profits_change,
corr(profits, revenues_change) AS revenues_change
FROM fortune500;
# - Insert rows into the correlations table for profits_change and revenues_change.
DROP TABLE IF EXISTS correlations;

CREATE TEMP TABLE correlations AS
SELECT 'profits'::varchar AS measure,
corr(profits, profits) AS profits,
corr(profits, profits_change) AS profits_change,
corr(profits, revenues_change) AS revenues_change
FROM fortune500;
--  select * from correlations;

-- Add a row for profits_change
-- Insert into what table?
  INSERT INTO correlations
-- Follow the pattern of the select statement above
-- Using profits_change instead of profits
SELECT 'profits_change'::varchar AS measure,
corr(profits_change, profits) AS profits,
corr(profits_change, profits_change) AS profits_change,
corr(profits_change, revenues_change) AS revenues_change
FROM fortune500;

-- Repeat the above, but for revenues_change
INSERT INTO correlations
SELECT 'revenues_change'::varchar AS measure,
corr(revenues_change, profits) AS profits,
corr(revenues_change, profits_change) AS profits_change,
corr(revenues_change, revenues_change) AS revenues_change
FROM fortune500;
# - Select all rows and columns from the correlations table to view the correlation matrix.
# - First, you will need to round each correlation to 2 decimal places.
# - The output of corr() is of type double precision, so you will need to also cast columns to numeric.
DROP TABLE IF EXISTS correlations;

CREATE TEMP TABLE correlations AS
SELECT 'profits'::varchar AS measure,
corr(profits, profits) AS profits,
corr(profits, profits_change) AS profits_change,
corr(profits, revenues_change) AS revenues_change
FROM fortune500;

INSERT INTO correlations
SELECT 'profits_change'::varchar AS measure,
corr(profits_change, profits) AS profits,
corr(profits_change, profits_change) AS profits_change,
corr(profits_change, revenues_change) AS revenues_change
FROM fortune500;

INSERT INTO correlations
SELECT 'revenues_change'::varchar AS measure,
corr(revenues_change, profits) AS profits,
corr(revenues_change, profits_change) AS profits_change,
corr(revenues_change, revenues_change) AS revenues_change
FROM fortune500;

-- Select each column, rounding the correlations
SELECT measure, 
ROUND(profits::NUMERIC, 2) AS profits,
ROUND(profits_change::NUMERIC, 2) AS profits_change,
ROUND(revenues_change::NUMERIC, 2) AS revenues_change
FROM correlations;
# measure	        profits	profits_change	revenues_change
# profits	        1.00	  0.02	          0.02
# profits_change	0.02	  1.00	          -0.09
# revenues_change	0.02	  -0.09	          1.00
# Fantastic work! When specifying the number of decimal places with the round or trunc functions, 
# the first value must be of type numeric. The correlations were double precision before being cast to numeric.

# 8 Character data types and common issues.mp4
# Count the categories
# In this chapter, we'll be working mostly with the Evanston 311 data in table evanston311. This is data on help requests submitted to the city of Evanston, IL.
# This data has several character columns. Start by examining the most frequent values in some of these columns to get familiar with the common categories.
# evanston311
# id	    priority	source	            category	                                            date_created	            date_completed	          street	       house_num	zip	  description
# 1340563	NONE	    gov.publicstuff.com	Fire Prevention - Inspection of a Commercial Property	2016-01-13 16:03:18+01:00	2016-01-19 17:51:26+01:00	Sheridan Road	 606-612	  60202	Please contact Debbie at Ext. 222
# 1826017	MEDIUM	  Iframe	            Water Service - Question or Concern	                  2016-08-12 16:35:12+02:00	2016-08-27 09:00:27+02:00	Washington St	 930	      null	Last spring we called you to report that our sump pump that in the past 50 years has been used to eject laundry water from the basement, was running continuously since February.  You came twice to check on it including taking a water sample and 'listening' at the street shut off valve.  You did not detect a leak.   Since then we have had three plumbers in to look at the problem.  We scoped the sewer line, one listened at the interior shut off, and we turned off the building water to see if it affected the pumping.  All negative.  The sump pump continues to run every 90 seconds 24/7, and we have one flood when the pump was accidentally turned off.  This current drought has not affected it either.   We are not sure what you can do but  we know that we have a constant source of water entering the sump, which one of the plumbers said would probably rule out a sewer line leak.  We are a 20 unit condo building.  This water is coming from somewhere, but our water bill suggests it is not an internal leak, as well as the other tests.  We thought you should know.
# 1849204	MEDIUM	  Iframe	            Trees-Fallen limb or tree	                            2016-08-22 11:07:45+02:00	2016-08-24 09:05:32+02:00	Lincoln St	   1183-1223	null	This isn't about a fallen tree or tree limb but I didn't know how else to categorize it this.  The sidewalk on the north side of Lincoln Street, east and the North Shore Sanitary Canal and west of Chandler Newberger is becoming difficult to use because of the overgrowth bushes and weeds on the edge of the Canal Shores golf course.  The vegetation and branches are growing out over the sidewalk.  Please have a crew (from the City or the golf course) trim there to make the sidewalk more useable, especially now that kids are back in school, walking on this sidewalk to/from Orrington and Haven.  Thanks!
# 1880254	MEDIUM	  iOS	                Ask A Question / Send A Message	                      2016-09-01 11:03:54+02:00	2016-09-01 18:52:40+02:00	Callan Ave	   1–111	    60202	Actually on back side of 621 Howard, growth in rain gutter
# 1972582	MEDIUM	  Iframe	            Dead Animal on Public Property	                      2016-09-19 03:46:41+02:00	2016-09-27 13:28:50+02:00	Crain St	     1524	      null	This is not public property but I need help.   There's a raccoon in my back yard that I'm pretty sure has distemper.  It was staggering around and when I went out with a flashlight to see what was going on, it went under bushes on right side of my garage and I need help.   I cannot afford to hire a commercial wild animal removal service.   They charge over $100 and I simply don't have it.   If that raccoon has distemper, which I'm sure it has ( or maybe it has rabies -both distemper and rabies make animals stagger), it's a public health concern.   I live in a two flat and my upstairs neighbor has a child and I have cats, and there are lots of pets in this area.   Although pets should be vaccinated for distemper, not everyone does that, which means this sick raccoon could be a risk to people's pets, given that distemper is  an air borne disease.   I need help.   My phone number is 847-693-0038.   Again, I cannot afford to hire a service to remove it.   I called a few and they charge over $100 to remove a raccoon.  I don't have that and neither does my upstairs neighbor.   Please help me.  The raccoon is inside my back yard on the side of the garage where there are a lot of bushes, near the side door of the garage.   I will be home until 10am.   You can go into my back yard without my permission, please just get that poor, very sick raccoon.
# 1840025	HIGH	    gov.publicstuff.com	Traffic Signal/Traffic Signal Back Plate	            2016-08-17 22:15:03+02:00	2016-08-31 10:45:14+02:00	Central Street 2830	      60201	Some type of flashing signal of exit fire truck. proper background color of signage (white, not yellow) for not blocking fire exit drive. Police can not enforce due to wrong background color. Flashing signal needed for safety reasons, numerous close calls with on coming traffic while exiting the station with emergency lights on. Repainting of street stripping to light post east of Reese (where signage currently is) possible a highlighting color (Red, red &white). please contact Capt. Taylor for questions or clarification at ext. 5945 would like to meet on 8/23 in AM to discuss some solutions or possible ideas.
# Instructions 1/4
# - How many rows does each priority level have?
-- Select the count of each level of priority
SELECT priority, COUNT(*) as n_rows, COUNT(priority) as n_not_null_values
FROM evanston311
GROUP BY priority;
# priority	n_rows	n_not_null_values
# MEDIUM	  5745	  5745
# NONE	    30081	  30081
# HIGH	    88	    88
# LOW	      517	    517
# - How many distinct values of zip appear in at least 100 rows?
-- Find values of zip that appear in at least 100 rows
-- Also get the count of each value
SELECT zip, COUNT(*)
FROM evanston311
GROUP BY zip
HAVING COUNT(*) >= 100; 
# zip	  count
# 60208	255
# null	5528
# 60201	19054
# 60202	11165
# - How many distinct values of source appear in at least 100 rows?
-- Find values of source that appear in at least 100 rows
-- Also get the count of each value
SELECT source, COUNT(*)
FROM evanston311
GROUP BY source
HAVING COUNT(*) >= 100;
# source	            count
# gov.publicstuff.com	30985
# Android	            444
# Iframe	            3670
# iOS	                1199
# - Select the five most common values of street and the count of each.
-- Find the 5 most common values of street and the count of each
SELECT street, COUNT(*)
FROM evanston311
GROUP BY street
ORDER BY count DESC
limit 5;
# street	        count
# null	          1699
# Chicago Avenue	1440
# Sherman Avenue	1276
# Central Street	1211
# Davis Street	  1154
# Fabulous! Becoming familiar with common categorical values helps you learn what to expect from your data.

# Spotting character data problems
# Explore the distinct values of the street column. Select each street value and the count of the number of rows with that value. Sort the results by street to see similar values near each other.
# Look at the results.
# Which of the following is NOT an issue you see with the values of street?
# Instructions
# Possible Answers
# - The street suffix (e.g. Street, Avenue) is sometimes abbreviated
#   Incorrect Submission
#   This problem exists. Example: Ashland Ave and Ashland Avenue.
# - There are sometimes extra spaces at the beginning and end of values +
#   Correct! street values do not have extra spaces. You could verify this with a LIKE query.
# - House/street numbers sometimes appear in the column
#   Incorrect Submission
#   House/street numbers that contain letters or punctuation have been erronously included in the street values.
# - Capitalization is not consistent across values
#   Incorrect Submission
#   The capitalization is fairly consistent, but there are values or partial values in all lowercase.
# - All of the above are potential problems
#   Incorrect Submission
#   Nope. One item in this list is NOT a problem with street values.
# Correct! street values do not have extra spaces. You could verify this with a LIKE query.
SELECT street, COUNT(*)
FROM evanston311
GROUP BY street
ORDER BY street;
# street	count
# 1/2 Chicago Ave	1
# 1047B Chicago Ave	1
# 13th Street	1
# 141A Callan Ave	2
# 141b Callan Ave	1
# 1624B Central St	1
# 217A Dodge Ave	1
# 221c Dodge Ave	1
# 300c Dodge Ave	1
# 3314A Central St	1
# 36th Street	1
# 600A South Blvd	7
# 606B South Blvd	3
# 612C South Blvd	2
# 613B Custer Ave	1
# 618B South Blvd	1
# 6th Street	2
# Arlington Boulevard	1
# Arnold Pl	1
# Arts Cir Dr	1
# Arts Circle Drive	3
# Asbury Ave	93

# 9 Cases and spaces.mp4
# Trimming
# Some of the street values in evanston311 include house numbers with # or / in them. In addition, some street values end in a ..
# Remove the house numbers, extra punctuation, and any spaces from the beginning and end of the street values as a first attempt at cleaning up the values.
# Instructions
# - Trim digits 0-9, #, /, ., and spaces from the beginning and end of street.
# - Select distinct original street value and the corrected street value.
# - Order the results by the original street value.
SELECT distinct street,
-- Trim off unwanted characters from street
trim(street, '0123456789 #/.') AS cleaned_street,
CASE WHEN trim(street, '0123456789 #/.') = street THEN 'Yes' ELSE 'No' END
FROM evanston311
ORDER BY street;
# street	              cleaned_street	        case
# 1/2 Chicago Ave	      Chicago Ave	            No
# 1047B Chicago Ave	    B Chicago Ave	          No
# 13th Street	          th Street	              No
# 141A Callan Ave	      A Callan Ave	          No
# 141b Callan Ave	      b Callan Ave	          No
# 1624B Central St	    B Central St	          No
# 217A Dodge Ave	      A Dodge Ave	            No
# 221c Dodge Ave	      c Dodge Ave	            No
# 300c Dodge Ave	      c Dodge Ave	            No
# 3314A Central St	    A Central St	          No
# 36th Street	          th Street	              No
# 600A South Blvd	      A South Blvd          	No
# 606B South Blvd	      B South Blvd          	No
# 612C South Blvd	      C South Blvd	          No
# 613B Custer Ave	      B Custer Ave	          No
# 618B South Blvd	      B South Blvd	          No
# 6th Street	          th Street	              No
# Arlington Boulevard	  Arlington Boulevard	    Yes
# Arnold Pl	            Arnold Pl	              Yes
# Arts Cir Dr	          Arts Cir Dr	            Yes
# Arts Circle Drive	    Arts Circle Drive	      Yes
# Asbury Ave	          Asbury Ave	            Yes
# Asbury Ave & Davis St	Asbury Ave & Davis St	  Yes
# Good job! Note that the "cleaned" values still include letters from house numbers, and trim() stripped off some numbers that belong as part of road names. It can take several tries to find the right combination of functions to clean up messy values.

# Exploring unstructured text
# The description column of evanston311 has the details of the inquiry, while the category column groups inquiries into different types. How well does the category capture what's in the description?
# LIKE and ILIKE queries will help you find relevant descriptions and categories. Remember that with LIKE queries, you can include a % on each side of a word to find values that contain the word. For example:
#   SELECT category
#   FROM evanston311
#   WHERE category LIKE '%Taxi%';
# % matches 0 or more characters.
# Building up the query through the steps below, find inquires that mention trash or garbage in the description without trash or garbage being in the category. What are the most frequent categories for such inquiries?
# Instructions 1/4
# - Use ILIKE to count rows in evanston311 where the description contains 'trash' or 'garbage' regardless of case.
-- Count rows
SELECT COUNT(*)
FROM evanston311
-- Where description includes trash or garbage
WHERE description ILIKE '%trash%' or description ILIKE '%garbage%';
# count
# 2551
# AK: data
-- Count rows
SELECT *
  FROM evanston311
-- Where description includes trash or garbage
WHERE description ILIKE '%trash%' or description ILIKE '%garbage%';
# id	priority	  source	              category	                                            date_created	date_completed	street	house_num	zip	description
# 2552787	MEDIUM	Iframe              	Trash, Recycling, Yard Waste Cart- Repair/Replacement	2017-04-20 20:27:40+02:00	2017-05-02 16:40:34+02:00	Grant St	2225	null	Large hole in base due to animals.  They are now pulling the trash from the bottom of the can and littering the alley area.
# 2554596	NONE	  gov.publicstuff.com	  Trash, Recycling, Yard Waste Cart- Repair/Replacement	2017-04-21 13:12:04+02:00	2017-05-03 16:57:48+02:00	Foster Street	2121	60201	3 trash carts need to be repaired/replaced.
# - category values are in title case. Use LIKE to find category values with 'Trash' or 'Garbage' in them.
-- Select categories containing Trash or Garbage
SELECT category
FROM evanston311
-- Use LIKE
WHERE category LIKE '%Trash%'
OR category LIKE '%Garbage%';
# category
# THIS REQUEST IS INACTIVE...Trash Cart - Compost Bin
# Trash - Tire Pickup
# Trash - Special Pickup - Resident Use
# Trash, Recycling, Yard Waste Cart- Repair/Replacement
# Trash, Recycling, Yard Waste Cart- Repair/Replacement
# Trash - Missed Garbage Pickup
# - Count rows where the description includes 'trash' or 'garbage' but the category does not.
-- Count rows
SELECT COUNT(*)
FROM evanston311 
-- description contains trash or garbage (any case)
WHERE (description ILIKE '%trash%'
       OR description ILIKE '%garbage%') 
-- category does not contain Trash or Garbage
AND category NOT LIKE '%Trash%'
AND category NOT LIKE '%Garbage%';
# count
# 570
# AK: data
-- Count rows
SELECT *
  FROM evanston311 
-- description contains trash or garbage (any case)
WHERE (description ILIKE '%trash%'
       OR description ILIKE '%garbage%') 
-- category does not contain Trash or Garbage
AND category NOT LIKE '%Trash%'
AND category NOT LIKE '%Garbage%';
# id	priority	source	category	date_created	date_completed	street	house_num	zip	description
# 2529319	NONE	gov.publicstuff.com	Rodents- Rats	2017-04-13 12:56:13+02:00	2017-08-01 16:29:41+02:00	Darrow Avenue	1810	60201	The resident is requesting treatment on her property. There are large rats. There is trash at 1813 Dodge which is to the West of her property across the alley.
# 3465201	NONE	gov.publicstuff.com	Ask A Question / Send A Message	2017-11-21 16:15:18+01:00	2017-11-27 11:35:32+01:00	Dempster Street	810	60202	Caller having trouble with Groot picking up her garbage.  Caller says that they have missed pick ups because she has trouble with one of the drivers.  She says that she is wondering if she can choose a different company other than Groot.  Caller advised that the fact that Groot is doing such a poor job is causing a rat issue in the alley.
# 2593734	NONE	gov.publicstuff.com	Ask A Question / Send A Message	2017-05-02 17:06:56+02:00	2017-05-08 17:20:17+02:00	Orrington Avenue	2636	60201	Complaint about Groot Trucking/Garbage service - caller states trucks travel at high speed in the school zone. She is fearful of possible accident.
# 2655826	NONE	Iframe	Sanitation Billing Questions	2017-05-18 15:22:39+02:00	2017-05-31 16:24:32+02:00	Dodge Ave	2233	null	Hello, I have a question about the sanitation charges on our Evanston Water Bill.    For your information, we live at 2233 Dodge Ave and our account # is 06622250-02.    I've always paid out bill without looking at the calculations since we moved in 2011, but recently I noticed the cost break-out of sanitation charges on the back of my bill.  The information indicates that a large black trash cart (95 gallon) is a charge of $17.95 per month and the blue recycling cart (65 gallon) is a charge of $7.95 per month.   These are the only two carts that we have at our house which is a single family home.    The total for these two carts should be $25.90 per month,  which would make our sanitation service charge $51.80 for a two month period.  However, the charge on our bill has alway been $71.80, which seems like it may be $20 too high.    Perhaps there are additional charges that I am not taking into account?   When you have a moment, I would appreciate your help to make sure the sanitation charges are correct.  Thanks in advance for your assistance.    Warm Regards,  Laura Gilbert 312-972-0505
# - Find the most common categories for rows with a description about trash that don't have a trash-related category.
-- Count rows with each category
SELECT category, COUNT(*)
FROM evanston311 
WHERE (description ILIKE '%trash%'
       OR description ILIKE '%garbage%') 
AND category NOT LIKE '%Trash%'
AND category NOT LIKE '%Garbage%'
-- What are you counting?
  GROUP BY category
--- order by most frequent values
ORDER BY count DESC
LIMIT 10;
# category	                                  count
# Ask A Question / Send A Message	            273
# Rodents- Rats	                              77
# Recycling - Missed Pickup	                  28
# Dead Animal on Public Property	            16
# Graffiti	                                  15
# Yard Waste - Missed Pickup	                14
# Public Transit Agency Issue	                13
# Food Establishment - Unsanitary Conditions	13
# Exterior Conditions	                        10
# Street Sweeping	                            9

# 10 Splitting and concatenating text.mp4
# Concatenate strings
# House number (house_num) and street are in two separate columns in evanston311. Concatenate them together with concat() with a space in between the values.
# Instructions
# - Concatenate house_num, a space ' ', and street into a single value using the concat().
# - Use a trim function to remove any spaces from the start of the concatenated value.
-- Concatenate house_num, a space, and street
-- and trim spaces from the start of the result
SELECT house_num, street, LTRIM(CONCAT(house_num, ' ', street)) AS address
FROM evanston311;
# house_num	street	      address
# 606-612	  Sheridan Road	606-612 Sheridan Road
# 930	      Washington St	930 Washington St
# 1183-1223	Lincoln St	  1183-1223 Lincoln St
# 1–111	    Callan Ave	  1–111 Callan Ave
# 1524	    Crain St	    1524 Crain St
# Great! When joining values that might be NULL with a separator between them, consider using the concat_ws() function, which you can read about in the PostgreSQL documentation, to avoid duplicate or unnecessary separators in the result.

# Split strings on a delimiter
# The street suffix is the part of the street name that gives the type of street, such as Avenue, Road, or Street. In the Evanston 311 data, sometimes the street suffix is the full word, while other times it is the abbreviation.
# Extract just the first word of each street value to find the most common streets regardless of the suffix.
# To do this, use
# split_part(string_to_split, delimiter, part_number)
# Instructions
# - Use split_part() to select the first word in street; alias the result as street_name.
# - Also select the count of each value of street_name.
-- Select the first word of the street value
SELECT split_part(street, ' ', 1) AS street_name, 
count(*)
FROM evanston311
GROUP BY street_name
ORDER BY count DESC
LIMIT 20;
# street_name	count
# null	1699
# Chicago	1569
# Central	1529
# Sherman	1479
# Davis	1248
# Church	1225
# Main	880
# Sheridan	842
# Ridge	823
# Dodge	816
# Maple	778
# Asbury	675
# Hinman	586
# West	578
# Orrington	561
# Emerson	513
# Grove	498
# Darrow	489
# Custer	464
# Lake	444

# Shorten long strings
# The description column of evanston311 can be very long. You can get the length of a string with the length() function.
# For displaying or quickly reviewing the data, you might want to only display the first few characters. You can use the left() function to get a specified number of characters at the start of each value.
# To indicate that more data is available, concatenate '...' to the end of any shortened description. To do this, you can use a CASE WHEN statement to add '...' only when the string length is greater than 50.
# Select the first 50 characters of description when description starts with the word "I".
# Instructions
# - Select the first 50 characters of description with '...' concatenated on the end where the length() of the description is greater than 50 characters. Otherwise just select the description as is.
# - Select only descriptions that begin with the word 'I' and not the letter 'I'.
# - For example, you would want to select "I like using SQL!", but would not want to select "In this course we use SQL!".
-- Select the first 50 chars when length is greater than 50
SELECT CASE WHEN length(description) > 50
THEN left(description, 50) || '...'
-- otherwise just select description
ELSE description
END
FROM evanston311
-- limit to descriptions that start with the word I
WHERE description LIKE 'I %'
ORDER BY description;
# description
# I  work for Schermerhorn & Co. and manage this con...
# I Live in a townhouse with garbage cans in back, i...
# I Put In For Reserve Disabled Parking, A Week Ago ...
# I SDO GOWANS #1258 RECEIVED A TELEPHONE CALL ON 3/...
# I accidentally mistyped my license plate number - ...
# I accidentally sent the wrong cover letter on my a...
# Well done! Shortening long fields can help you scan the values quickly and spot patterns you might not otherwise see.

# 11 Strategies for multiple transformations.mp4
# Create an "other" category
# If we want to summarize Evanston 311 requests by zip code, it would be useful to group all of the low frequency zip codes together in an "other" category.
# Which of the following values, when substituted for ??? in the query, would give the result below?
# Query:
# SELECT CASE WHEN zipcount < ??? THEN 'other'
# ELSE zip
# END AS zip_recoded,
# sum(zipcount) AS zipsum
# FROM (SELECT zip, count(*) AS zipcount
#       FROM evanston311
#       GROUP BY zip) AS fullcounts
# GROUP BY zip_recoded
# ORDER BY zipsum DESC;
# Result:
# zip_recoded    zipsum
# 60201          19054
# 60202          11165
# null           5528
# other          429
# 60208          255
# Instructions
# Possible Answers
# - 255 +
#   Incorrect Submission
#   Using 255 as a cut-off would exclude 60208 since the query uses < instead of <=. (AK: ???)
# - 1000
#   Incorrect Submission
#   A cutoff of 1000 would include the 60208 zip code in the 'other' category instead of leaving it as a separate value.
# - 100
#   Correct! All of the zip codes with fewer observations than 60208 have less than 100 rows in the table.
# - 60201
#  Incorrect Submission
#  ??? should be a value of count, not a zip code.
SELECT CASE WHEN zipcount < 255 THEN 'other'
ELSE zip
END AS zip_recoded,
sum(zipcount) AS zipsum
FROM (SELECT zip, count(*) AS zipcount
      FROM evanston311
      GROUP BY zip) AS fullcounts
GROUP BY zip_recoded
ORDER BY zipsum DESC;
# zip_recoded	zipsum
# 60201	      19054
# 60202	      11165
# null	      5528
# other	      429
# 60208	      255
SELECT CASE WHEN zipcount < 100 THEN 'other'
ELSE zip
END AS zip_recoded,
sum(zipcount) AS zipsum
FROM (SELECT zip, count(*) AS zipcount
      FROM evanston311
      GROUP BY zip) AS fullcounts
GROUP BY zip_recoded
ORDER BY zipsum DESC;
# zip_recoded	zipsum
# 60201	      19054
# 60202	      11165
# null	      5528
# other	      429
# 60208	      255

# Group and recode values
# There are almost 150 distinct values of evanston311.category. But some of these categories are similar, with the form "Main Category - Details". We can get a better sense of what requests are common if we aggregate by the main category.
# To do this, create a temporary table recode mapping distinct category values to new, standardized values. Make the standardized values the part of the category before a dash ('-'). Extract this value with the split_part() function:
# split_part(string text, delimiter text, field int)
# You'll also need to do some additional cleanup of a few cases that don't fit this pattern.
# Then the evanston311 table can be joined to recode to group requests by the new standardized category values.
# Instructions 1/4
# - Create recode with a standardized column; use split_part() and then rtrim() to remove any remaining whitespace on the result of split_part().
-- Fill in the command below with the name of the temp table
DROP TABLE IF EXISTS recode;

-- Create and name the temporary table
CREATE TEMP TABLE recode AS
-- Write the select query to generate the table 
-- with distinct values of category and standardized values
SELECT DISTINCT category, 
rtrim(split_part(category, '-', 1)) AS standardized
-- What table are you selecting the above values from?
  FROM evanston311;

-- Look at a few values before the next step
SELECT DISTINCT standardized 
FROM recode
WHERE standardized LIKE 'Trash%Cart'
OR standardized LIKE 'Snow%Removal%';
# standardized
# Snow Removal
# Snow Removal/Concerns
# Snow/Ice/Hazard Removal
# Trash Cart
# Trash Cart, Recycling Cart
# - UPDATE standardized values LIKE 'Trash%Cart' to 'Trash Cart'.
# - UPDATE standardized values of 'Snow Removal/Concerns' and 'Snow/Ice/Hazard Removal' to 'Snow Removal'.
-- Code from previous step
DROP TABLE IF EXISTS recode;

CREATE TEMP TABLE recode AS
SELECT DISTINCT category, 
rtrim(split_part(category, '-', 1)) AS standardized
FROM evanston311;

-- Update to group trash cart values
UPDATE recode 
SET standardized='Trash Cart' 
WHERE category LIKE 'Trash%Cart%';

-- Update to group snow removal values
UPDATE recode 
SET standardized='Snow Removal' 
WHERE category LIKE 'Snow%Removal%';

-- Examine effect of updates
SELECT DISTINCT standardized 
FROM recode
WHERE standardized LIKE 'Trash%Cart'
OR standardized LIKE 'Snow%Removal%';
# standardized
# Snow Removal
# Trash Cart
# - UPDATE recode by setting standardized values of 'THIS REQUEST IS INACTIVE…Trash Cart', '(DO NOT USE) Water Bill', 'DO NOT USE Trash', and 'NO LONGER IN USE' to 'UNUSED'.
-- Code from previous step
DROP TABLE IF EXISTS recode;

CREATE TEMP TABLE recode AS
SELECT DISTINCT category, 
rtrim(split_part(category, '-', 1)) AS standardized
FROM evanston311;

UPDATE recode SET standardized='Trash Cart' 
WHERE standardized LIKE 'Trash%Cart';

UPDATE recode SET standardized='Snow Removal' 
WHERE standardized LIKE 'Snow%Removal%';

-- Update to group unused/inactive values
UPDATE recode SET standardized='UNUSED' 
WHERE standardized IN ('THIS REQUEST IS INACTIVE...Trash Cart', 
                       '(DO NOT USE) Water Bill',
                       'DO NOT USE Trash', 
                       'NO LONGER IN USE');

-- Examine effect of updates
SELECT DISTINCT standardized 
FROM recode
ORDER BY standardized;
# standardized
# ADA/Inclusion Aids
# Abandoned Bicycle on City Property
# Abandoned Vehicle
# Accessibility
# - Now, join the evanston311 and recode tables to count the number of requests with each of the standardized values
# - List the most common standardized values first.
-- Code from previous step
DROP TABLE IF EXISTS recode;
CREATE TEMP TABLE recode AS
  SELECT 
    DISTINCT category, 
    rtrim(split_part(category, '-', 1)) AS standardized
  FROM evanston311;
  
UPDATE recode SET standardized='Trash Cart' 
  WHERE standardized LIKE 'Trash%Cart';
  
UPDATE recode SET standardized='Snow Removal' 
  WHERE standardized LIKE 'Snow%Removal%';
  
UPDATE recode SET standardized='UNUSED' 
  WHERE standardized IN ('THIS REQUEST IS INACTIVE...Trash Cart', 
                       '(DO NOT USE) Water Bill',
                       'DO NOT USE Trash', 'NO LONGER IN USE');

-- Select the recoded categories and the count of each
SELECT recode.standardized, COUNT(*)
-- From the original table and table with recoded values
FROM recode
INNER JOIN evanston311 
-- What column do they have in common?
  ON evanston311.category = recode.category 
-- What do you need to group by to count?
  GROUP BY recode.standardized
-- Display the most common val values first
ORDER BY count DESC;
# standardized	                  count
# Broken Parking Meter	          6092
# Trash	                          3699
# Ask A Question / Send A Message	2595
# Trash Cart	                    1902
# Tree Evaluation	                1879
# Rodents	                        1305
# Awesome! You now have a strategy for recoding the values of a variable. Next, you'll use a similar approach to create some new variables.

# Create a table with indicator variables
# Determine whether medium and high priority requests in the evanston311 data are more likely to contain requesters' contact information: an email address or phone number.
# Emails contain an @.
# Phone numbers have the pattern of three characters, dash, three characters, dash, four characters. For example: 555-555-1212.
# Use LIKE to match these patterns. Remember % matches any number of characters (even 0), and _ matches a single character. Enclosing a pattern in % (i.e. before and after your pattern) allows you to locate it within other text.
# For example, '%___.com%'would allow you to search for a reference to a website with the top-level domain '.com' and at least three characters preceding it.
# Create and store indicator variables for email and phone in a temporary table. LIKE produces True or False as a result, but casting a boolean (True or False) as an integer converts True to 1 and False to 0. This makes the values easier to summarize later.
# Instructions 1/2
# - Create a temp table indicators from evanston311 with three columns: id, email, and phone.
# - Use LIKE comparisons to detect the email and phone patterns that are in the description, and cast the result as an integer with CAST().
# - Your phone indicator should use a combination of underscores _ and dashes - to represent a standard 10-digit phone number format.
# - Remember to start and end your patterns with % so that you can locate the pattern within other text!
-- To clear table if it already exists
DROP TABLE IF EXISTS indicators;

-- Create the indicators temp table
CREATE TEMP TABLE indicators AS
-- Select id
SELECT id, 
-- Create the email indicator (find @)
CAST (description LIKE '%@%' AS integer) AS email,
-- Create the phone indicator
CAST (description LIKE '%___-___-____%' AS integer) AS phone
-- What table contains the data? 
  FROM evanston311;

-- Inspect the contents of the new temp table
SELECT *
  FROM indicators;
# id	email	phone
# 1340563	0	0
# 1826017	0	0
# 1849204	0	0
# 1880254	0	0
# 1972582	0	1
# AK: subquery
-- To clear table if it already exists
DROP TABLE IF EXISTS indicators;

-- Create the indicators temp table
CREATE TEMP TABLE indicators AS
-- Select id
SELECT id, 
-- Create the email indicator (find @)
CAST (description LIKE '%@%' AS integer) AS email,
-- Create the phone indicator
CAST (description LIKE '%___-___-____%' AS integer) AS phone,
description
-- What table contains the data? 
  FROM evanston311;

-- Inspect the contents of the new temp table
SELECT *
  FROM indicators
WHERE email * phone = 1;
# id	email	phone	description
# 2167024	1	1	I own one of ten two-flats on this street.  First, a question, then the background: what is height for a wrought iron decorative fence in front yard? For over two years kids going east via Mulford from school have stolen the corner stakes I had securing small corner decorative fencing, which I'd replace.  I removed one of the corners last week after the stake was stolen again, and two days ago they stole another stake (to use as swords? Who knows?) Yesterday I removed the remaining corner fencing.  I think I saw the thieves, running and whooping over my front yard after school (yay!  We won! She took down the corner fencing!)  This has been very disturbing. If I replace with a full front garden-type decorative fence, how high can I go (I'd secure it w. cement so these little thieves can't pull it up).  Or, is there another solution to this constant thievery? Darlene Nilges (40 yr. Evanston resident) dnilges@yahoo.com 847-732-4787
# 2336630	1	1	Even a very heavy  Divvy  bicycle, facing southbound on Sheridan (towards the alley) does not  trip  the in-pavement sensor, which needs re-tuning. At 10:15pm Thu 2/9/17, there was a lot of E-W auto traffic on Min, but none coming behind me southbound. So it was dangerous (plus illegal) for me to pedal forward against the red light, but it would not turn green.  (BTW, I no longer live in Evanston - I now live in W Rogers Park. But, in Evanston, I and my wife still - have friends; - shop; - visit our doctors, dentists & hospital; - eat at restaurants; - go to movies and musical events; - go to events at Northwestern U.; - are members of the Evanston Bicycle Club.)  Thank you, A.C. Wilson acwilson99@comcast.net 773-274-7663
# 3518565	1	1	Please send a copy of final reading by fax and/or email.  FAX: 312-750-1211 kjacobson@bussepc.com Request made by attorney's office.
# 2804628	1	1	Hello,  My car was damaged by city tree on 250 Ridge Ave. I do have a police report (17-006055) filed today 6/23/17. I went to three different body shops and they quote prices from $1357.11 to $550.   My car is the second car on that street that was damaged in the past three weeks. Since my deductible is more than the damages i am looking for the right procedure for damages reimbursement from the city of Evanston. Can you please help me. I am attaching a pictures of the accident.  My information:  Name: Lachezar Atanasov Address: 250 Ridge Ave apt 1E Phone: 773-398-5758 email: lacho78@gmail.com
# - Join the indicators table to evanston311, selecting the proportion of reports including an email or phone grouped by priority.
# - Include adjustments to account for issues arising from integer division.
-- To clear table if it already exists
DROP TABLE IF EXISTS indicators;

-- Create the temp table
CREATE TEMP TABLE indicators AS
SELECT id, 
CAST (description LIKE '%@%' AS integer) AS email,
CAST (description LIKE '%___-___-____%' AS integer) AS phone 
FROM evanston311;

-- Select the column you ll group by
SELECT priority,
       -- Compute the proportion of rows with each indicator
       SUM(email)/COUNT(*)::NUMERIC AS email_prop, 
       SUM(phone)/COUNT(*)::NUMERIC AS phone_prop
  -- Tables to select from
  FROM evanston311
       LEFT JOIN indicators
       -- Joining condition
       ON evanston311.id = indicators.id
 -- What are you grouping by?
 GROUP BY priority;
# priority	email_prop	            phone_prop
# MEDIUM	  0.01966927763272410792	0.01845082680591818973
# NONE	    0.00412220338419600412	0.00568465144110900568
# HIGH	    0.01136363636363636364	0.02272727272727272727
# LOW	      0.00580270793036750484	0.00193423597678916828
# You made it through! You not only created new variables but also used them successfully to learn about the 
# data. Medium and high priority requests do contain contact information more frequently.
# AK: ::INTEGER
-- To clear table if it already exists
DROP TABLE IF EXISTS indicators;

-- Create the temp table
CREATE TEMP TABLE indicators AS
SELECT id, 
CAST (description LIKE '%@%' AS integer) AS email,
CAST (description LIKE '%___-___-____%' AS integer) AS phone 
FROM evanston311;

-- Select the column you ll group by
SELECT priority,
       -- Compute the proportion of rows with each indicator
       SUM(email)/COUNT(*)::INTEGER AS email_prop, 
       SUM(phone)/COUNT(*)::INTEGER AS phone_prop
  -- Tables to select from
  FROM evanston311
       LEFT JOIN indicators
       -- Joining condition
       ON evanston311.id = indicators.id
 -- What are you grouping by?
 GROUP BY priority;
# priority	email_prop	phone_prop
# MEDIUM	  0	           0
# NONE	    0	           0
# HIGH	    0	           0
# LOW	      0	           0
       
# 12 Date/time types and formats.mp4
# ISO 8601
# Which date format below conforms to the ISO 8601 standard?
# Answer the question
# Possible Answers
# - June 15, 2018 3:30pm
# - 2018-06-15 15:30:00 +
# - 6/15/18 13:00
# - 2018-6-15 3:30:00
# Correct! The units are ordered from largest to smallest, and single digit values have a leading zero.

# Date comparisons
# When working with timestamps, sometimes you want to find all observations on a given day. However, if you specify only a date in a comparison, you may get unexpected results. This query:
# SELECT count(*) 
# FROM evanston311
# WHERE date_created = '2018-01-02';
# returns 0, even though there were 49 requests on January 2, 2018.
# This is because dates are automatically converted to timestamps when compared to a timestamp. The time fields are all set to zero:
# SELECT '2018-01-02'::timestamp;
# 2018-01-02 00:00:00
# When working with both timestamps and dates, you'll need to keep this in mind.
# Instructions 1/3
# - Count the number of Evanston 311 requests created on January 31, 2017 by casting date_created to a date.
-- Count requests created on January 31, 2017
SELECT count(*) 
FROM evanston311
WHERE date_created::DATE = '2017-01-31';
# count
# 45
# - Count the number of Evanston 311 requests created on February 29, 2016 by using >= and < operators.
-- Count requests created on February 29, 2016
SELECT count(*)
FROM evanston311 
WHERE date_created >= '2016-02-29' 
AND date_created < '2016-02-29'::DATE + 1;
# count
# 58
# - Count the number of requests created on March 13, 2017.
# - Specify the upper bound by adding 1 to the lower bound.
-- Count requests created on March 13, 2017
SELECT count(*)
FROM evanston311
WHERE date_created >= '2017-03-13'
AND date_created < '2017-03-13'::date + 1;
# count
# 33
# Good job! The strategy used in the first step is the simpliest and will often work, but the other approaches may be useful in different circumstances.

# Date arithmetic
# You can subtract dates or timestamps from each other.
# You can add time to dates or timestamps using intervals. An interval is specified with a number of units and the name of a datetime field. For example:
# '3 days'::interval
# '6 months'::interval
# '1 month 2 years'::interval
# '1 hour 30 minutes'::interval
# Practice date arithmetic with the Evanston 311 data and now().
# Instructions 1/4
# - Subtract the minimum date_created from the maximum date_created.
-- Subtract the min date_created from the max
SELECT MAX(date_created) - MIN(date_created)
FROM evanston311;
# ?column?
# 911 days, 16:33:39
# - Using now(), find out how old the most recent evanston311 request was created.
-- How old is the most recent request?
  SELECT now() - MAX(date_created), MAX(date_created), now()
FROM evanston311;
# ?column?	                  max	                      now
# 1467 days, 15:53:52.207619	2018-06-30 18:36:22+02:00	2022-07-07 10:30:14.207619+02:00
# - Add 100 days to the current timestamp.
-- Add 100 days to the current timestamp
SELECT now() + '100 days'::interval;
# ?column?
# 2022-10-15 10:31:45.300928+02:00
# - Select the current timestamp and the current timestamp plus 5 minutes.
-- Select the current timestamp, 
-- and the current timestamp + 5 minutes
SELECT now() + '5 minutes'::interval;
# ?column?
# 2022-07-07 10:38:33.625043+02:00
# Fantastic! The 's' at the of 'minutes' is optional. The query will work with or without it.

# Completion time by category
# The evanston311 data includes a date_created timestamp from when each request was created and a date_completed timestamp for when it was completed. The difference between these tells us how long a request was open.
# Which category of Evanston 311 requests takes the longest to complete?
# Instructions
# - Compute the average difference between the completion timestamp and the creation timestamp by category.
# - Order the results with the largest average time to complete the request first.
-- Select the category and the average completion time by category
SELECT category, 
AVG(date_completed - date_created) AS completion_time
FROM evanston311
GROUP by category
-- Order the results
ORDER BY completion_time DESC;
# category	                                          completion_time
# Rodents- Rats	                                      64 days, 10:58:23.000766
# Fire Prevention - Public Education	                34 days, 16:48:10
# Key Request - All  City Employees	                  32 days, 0:52:11
# Smoking	                                            27 days, 7:42:30.238095
# Notice of Violation	                                24 days, 5:41:12.666667
# Exterior Conditions	                                23 days, 22:18:31.087719
# General/Routine Maintenance - Facilities Management	23 days, 3:01:56.480000
# Trash - Special Pickup (STAFF ONLY)	                19 days, 6:04:52.096070
# Public Transit Agency Issue	                        19 days, 3:28:15.845070
# Private Utility Service Issue	                      17 days, 19:50:13.130000
# Nice going! The results of one query can help you generate further questions to answer: Why do rat 
# issues take so long to resolve? We'll investigate in an upcoming exercise.

# 13 Date/time components and aggregation.mp4
# Date parts
# The date_part() function is useful when you want to aggregate data by a unit of time across multiple larger units of time. For example, aggregating data by month across different years, or aggregating by hour across different days.
# Recall that you use date_part() as:
# SELECT date_part('field', timestamp);
# In this exercise, you'll use date_part() to gain insights about when Evanston 311 requests are submitted and completed.
# Instructions 1/3
# - How many requests are created in each of the 12 months during 2016-2017?
-- Extract the month from date_created and count requests
SELECT date_part('month', date_created) AS month, 
COUNT(*)
FROM evanston311
-- Limit the date range
WHERE date_part('year', date_created) IN (2016, 2017)
-- Group by what to get monthly counts?
  GROUP BY month
ORDER BY month ASC;
# month	count
# 1	1810
# 2	1775
# 3	2171
# 4	2385
# 5	2674
# 6	3403
# 7	3062
# 8	3110
# 9	2760
#10	2399
#11	2283
#12	2000
# - What is the most common hour of the day for requests to be created?
-- Get the hour and count requests
SELECT date_part('hour', date_created) AS hour,
count(*)
FROM evanston311
GROUP BY hour
-- Order results to select most common
ORDER BY count DESC
LIMIT 1;
# hour	count
# 11	  3960
-- Get the hour and count requests
SELECT date_part('hour', date_created) AS hour,
count(*)
FROM evanston311
GROUP BY hour
-- Order results to select most common
ORDER BY hour
--LIMIT 1;
# hour	count
# 0	    169
# 1	    88
# 2	    47
# 3	    29
# 4	    19
# 5	    9
# 6	    25
# 7	    145
# 8	    787
# 9	    2015
# 10	  3166
# 11	  3960
# 12	  3945
# 13	  3729
# 14	  3364
# 15	  3210
# 16	  3122
# 17	  2918
# 18	  2354
# 19	  1468
# 20	  993
# 21	  369
# 22	  268
# 23	  232
# - During what hours are requests usually completed? Count requests completed by hour.
# - Order the results by hour.
-- Count requests completed by hour
SELECT date_part('hour', date_completed) AS hour,
COUNT(*)
FROM evanston311
GROUP BY hour
ORDER BY hour;
# hour	count
# 0	59
# 1	49
# 2	10
# 3	17
# 4	13
# 5	9
# 6	37
# 7	450
# 8	1392
# 9	2143
#10	2830
#11	2849
#12	3199
#13	3302
#14	4028
#15	4734
#16	5157
#17	4013
#18	1267
#19	354
#20	186
#21	130
#22	87
#23	116
# Brilliant! You can also aggregate by day or week of the year, but make sure you read the documentation about how they are computed before using these units.

# Variation by day of week
# Does the time required to complete a request vary by the day of the week on which the request was created?
# We can get the name of the day of the week by converting a timestamp to character data:
# to_char(date_created, 'day') 
# But character names for the days of the week sort in alphabetical, not chronological, order. To get the chronological order of days of the week with an integer value for each day, we can use:
# EXTRACT(DOW FROM date_created)
# DOW stands for "day of week."
# Instructions
# - Select the name of the day of the week the request was created (date_created) as day.
# - Select the mean time between the request completion (date_completed) and request creation as duration.
# - Group by day (the name of the day of the week) and the integer value for the day of the week (use a function).
# - Order by the integer value of the day of the week using the same function used in GROUP BY.
-- Select name of the day of the week the request was created 
SELECT to_char(date_created, 'day') AS day,
-- Select avg time between request creation and completion
AVG(date_completed - date_created) AS duration
FROM evanston311 
-- Group by the name of the day of the week and 
-- integer value of day of week the request was created
GROUP BY day, EXTRACT(DOW FROM date_created)
-- Order by integer value of the day of the week 
-- the request was created
ORDER BY EXTRACT(DOW FROM date_created);
# day	duration
# sunday   	8 days, 23:02:01.677165
# monday   	7 days, 0:47:18.215322
# tuesday  	7 days, 2:36:24.417655
# wednesday	7 days, 12:30:37.884480
# thursday 	7 days, 10:32:22.104339
# friday   	8 days, 10:50:45.072001
# saturday 	7 days, 15:01:33.301878
# AK
-- Select name of the day of the week the request was created 
SELECT to_char(date_created, 'day') AS day, EXTRACT(DOW FROM date_created),
-- Select avg time between request creation and completion
AVG(date_completed - date_created) AS duration
FROM evanston311 
-- Group by the name of the day of the week and 
-- integer value of day of week the request was created
GROUP BY day, EXTRACT(DOW FROM date_created)
-- Order by integer value of the day of the week 
-- the request was created
ORDER BY EXTRACT(DOW FROM date_created);
# day	extract	duration
# sunday   	0	8 days, 23:02:01.677165
# monday   	1	7 days, 0:47:18.215322
# tuesday  	2	7 days, 2:36:24.417655
# wednesday	3	7 days, 12:30:37.884480
# thursday 	4	7 days, 10:32:22.104339
# friday   	5	8 days, 10:50:45.072001
# saturday 	6	7 days, 15:01:33.301878
# Terrific! Requests created at the beginning of the work week are closed sooner on average than those created at the end of the week or on the weekend.

# Date truncation
# Unlike date_part() or EXTRACT(), date_trunc() keeps date/time units larger than the field you specify as part of the date. So instead of just extracting one component of a timestamp, date_trunc() returns the specified unit and all larger ones as well.
# Recall the syntax:
# date_trunc('field', timestamp)
# Using date_trunc(), find the average number of Evanston 311 requests created per day for each month of the data. Ignore days with no requests when taking the average.
# Instructions
# - Write a subquery to count the number of requests created per day.
# - Select the month and average count per month from the daily_count subquery.
-- Aggregate daily counts by month
SELECT date_trunc('month', day)::DATE AS month,
ROUND(AVG(count), 2)
-- Subquery to compute daily counts
FROM (SELECT date_trunc('day', date_created) AS day,
      COUNT(*) AS count
      FROM evanston311
      GROUP BY day) AS daily_count
GROUP BY month
ORDER BY month;
# month	round
# 2016-01-01	23.48
# 2016-02-01	30.76
# 2016-03-01	35.55
# 2016-04-01	37.30
# 2016-05-01	40.77
# 2016-06-01	43.97
# 2016-07-01	41.52
# 2016-08-01	46.55
# 2016-09-01	47.33
# 2016-10-01	35.81
# 2016-11-01	36.62
# 2016-12-01	29.39
# 2017-01-01	34.90
# 2017-02-01	32.70
# 2017-03-01	35.63
# 2017-04-01	43.66
# 2017-05-01	46.81
# 2017-06-01	69.47
# 2017-07-01	57.26
# 2017-08-01	53.77
# 2017-09-01	44.67
# 2017-10-01	41.58
# 2017-11-01	40.70
# 2017-12-01	36.30
# 2018-01-01	35.48
# 2018-02-01	30.54
# 2018-03-01	29.71
# 2018-04-01	35.13
# 2018-05-01	45.32
# 2018-06-01	44.50
# You got it! This query ignores dates with no requests. You'll learn how to account for missing dates in an upcoming exercise.

# 14 Aggregating with date/time series.mp4
# Find missing dates
# The generate_series() function can be useful for identifying missing dates.
# Recall:
# generate_series(from, to, interval)
# where from and to are dates or timestamps, and interval can be specified as a string with a number and a unit of time, such as '1 month'.
# Are there any days in the Evanston 311 data where no requests were created?
# Instructions
# - Write a subquery using generate_series() to get all dates between the min() and max() date_created in evanston311.
# - Write another subquery to select all values of date_created as dates from evanston311.
# - Both subqueries should produce values of type date (look for the ::).
# - Select dates (day) from the first subquery that are NOT IN the results of the second subquery. This gives you days that are not in date_created.
SELECT day
-- 1) Subquery to generate all dates
-- from min to max date_created
FROM (SELECT generate_series(MIN(date_created),
                             MAX(date_created),
                             '1 day')::DATE AS day
      -- What table is date_created in?
        FROM evanston311) AS all_dates
-- 4) Select dates (day from above) that are NOT IN the subquery
WHERE day NOT IN 
-- 2) Subquery to select all date_created values as dates
(SELECT date_created::date
  FROM evanston311);
# day
# 2016-05-08
# 2016-11-06
# 2017-02-05
# 2017-03-12
# 2017-04-16
# 2017-12-25
# 2018-01-06
# 2018-01-14
# Fantastic! This approach works for finding missing values of other units of time, such as hours or months, as well.

# Custom aggregation periods
# Find the median number of Evanston 311 requests per day in each six month period from 2016-01-01 to 2018-06-30. Build the query following the three steps below.
# Recall that to aggregate data by non-standard date/time intervals, such as six months, you can use generate_series() to create bins with lower and upper bounds of time, and then summarize observations that fall in each bin.
# Remember: you can access the slides with an example of this type of query using the PDF icon link in the upper right corner of the screen.
# Instructions 1/3
# - Use generate_series() to create bins of 6 month intervals. Recall that the upper bin values are exclusive, so the values need to be one day greater than the last day to be included in the bin.
# - Notice how in the sample code, the first bin value of the upper bound is July 1st, and not June 30th.
# - Use the same approach when creating the last bin values of the lower and upper bounds (i.e. for 2018).
-- Generate 6 month bins covering 2016-01-01 to 2018-06-30

-- Create lower bounds of bins
SELECT generate_series('2016-01-01',  -- First bin lower value
                       '2018-01-01',  -- Last bin lower value
                       '6 month'::interval) AS lower,
-- Create upper bounds of bins
generate_series('2016-07-01',  -- First bin upper value
                '2018-07-01',  -- Last bin upper value
                '6 month'::interval) AS upper;
# lower	                    upper
# 2016-01-01 00:00:00+01:00	2016-07-01 00:00:00+02:00
# 2016-07-01 00:00:00+02:00	2017-01-01 00:00:00+01:00
# 2017-01-01 00:00:00+01:00	2017-07-01 00:00:00+02:00
# 2017-07-01 00:00:00+02:00	2018-01-01 00:00:00+01:00
# 2018-01-01 00:00:00+01:00	2018-07-01 00:00:00+02:00
# - Count the number of requests created per day. Remember to not count *, or you will risk counting NULL values.
# - Include days with no requests by joining evanston311 to a daily series from 2016-01-01 to 2018-06-30.
# - Note that because we are not generating bins, you can use June 30th as your series end date.
-- Count number of requests made per day
SELECT day, COUNT(evanston311.id) AS count # count not null after left join
-- Use a daily series from 2016-01-01 to 2018-06-30 
-- to include days with no requests
FROM (SELECT generate_series('2016-01-01',  -- series start date
                             '2018-06-30',  -- series end date
                             '1 day'::interval)::date AS day) AS daily_series
LEFT JOIN evanston311
-- match day from above (which is a date) to date_created
ON day = date_created::date
GROUP BY day;
# day	        count
# 2016-01-01	5
# 2016-01-02	27
# 2016-01-03	7
# 2016-01-04	56
# 2016-01-05	33
# 2016-01-06	44
# - Assign each daily count to a single 6 month bin by joining bins to daily_counts.
# - Compute the median value per bin using percentile_disc().
-- Bins from Step 1
WITH bins AS (
  SELECT generate_series('2016-01-01',
                         '2018-01-01',
                         '6 months'::interval) AS lower,
  generate_series('2016-07-01',
                  '2018-07-01',
                  '6 months'::interval) AS upper),
-- Daily counts from Step 2
daily_counts AS (
  SELECT day, count(date_created) AS count
  FROM (
      SELECT generate_series('2016-01-01',
                               '2018-06-30',
                               '1 day'::interval)::date AS day) AS daily_series
      LEFT JOIN evanston311
      ON day = date_created::date
      GROUP BY day)
-- Select bin bounds 
SELECT lower, 
upper, 
-- Compute median of count for each bin
percentile_disc(0.5) WITHIN GROUP (ORDER BY count) AS median
-- Join bins and daily_counts
FROM bins
LEFT JOIN daily_counts
-- Where the day is between the bin bounds
ON day >= lower
AND day < upper
-- Group by bin bounds
GROUP BY lower, upper
ORDER BY lower;
# lower	                    upper	                    median
# 2016-01-01 00:00:00+01:00	2016-07-01 00:00:00+02:00	37
# 2016-07-01 00:00:00+02:00	2017-01-01 00:00:00+01:00	41
# 2017-01-01 00:00:00+01:00	2017-07-01 00:00:00+02:00	44
# 2017-07-01 00:00:00+02:00	2018-01-01 00:00:00+01:00	51
# 2018-01-01 00:00:00+01:00	2018-07-01 00:00:00+02:00	40
# Well done! You might need to create custom bins to correspond to fiscal years, academic years, 
# 2-week periods, or other reporting periods for your organization.

# Monthly average with missing dates
# Find the average number of Evanston 311 requests created per day for each month of the data.
# This time, do not ignore dates with no requests.
# Instructions
# - Generate a series of dates from 2016-01-01 to 2018-06-30.
# - Join the series to a subquery to count the number of requests created per day.
# - Use date_trunc() to get months from date, which has all dates, NOT day.
# - Use coalesce() to replace NULL count values with 0. Compute the average of this value.
-- generate series with all days from 2016-01-01 to 2018-06-30
WITH all_days AS 
(SELECT generate_series('2016-01-01',
                        '2018-06-30',
                        '1 day'::interval) AS date),
-- Subquery to compute daily counts
daily_count AS 
(SELECT date_trunc('day', date_created) AS day,
  count(*) AS count
  FROM evanston311
  GROUP BY day)
-- Aggregate daily counts by month using date_trunc
SELECT date_trunc('month', all_days.date)::date AS month,
-- Use coalesce to replace NULL count values with 0
ROUND(avg(coalesce(count, 0)), 2) AS average
FROM all_days
LEFT JOIN daily_count
-- Joining condition
ON all_days.date=daily_count.day
GROUP BY month
ORDER BY month;  
# month	      average
# 2016-01-01	23.48
# 2016-02-01	30.76
# 2016-03-01	35.55
# 2016-04-01	37.30
# 2016-05-01	39.45
# 2016-06-01	43.97
# Well done. Because there are few days with no requests, including them doesn't change the averages 
# much. But, including them is always the right way to compute accurate averages!

# 15 Time between events.mp4
# Longest gap
# What is the longest time between Evanston 311 requests being submitted?
# Recall the syntax for lead() and lag():
# lag(column_to_adjust) OVER (ORDER BY ordering_column)
# lead(column_to_adjust) OVER (ORDER BY ordering_column)
# Instructions
# - Select date_created and the date_created of the previous request using lead() or lag() as appropriate.
# - Compute the gap between each request and the previous request.
# - Select the row with the maximum gap.
-- Compute the gaps
WITH request_gaps AS (
  SELECT date_created,
  -- lead or lag
  lag(date_created) OVER (ORDER BY date_created) AS previous,
  -- compute gap as date_created minus lead or lag
  date_created - lag(date_created) OVER (ORDER BY date_created) AS gap
  FROM evanston311)
-- Select the row with the maximum gap
SELECT *
  FROM request_gaps
-- Subquery to select maximum gap from request_gaps
WHERE gap = (SELECT MAX(gap)
             FROM request_gaps);
# date_created	            previous	                gap
# 2018-01-07 19:41:34+01:00	2018-01-05 19:04:09+01:00	2 days, 0:37:25
# Hooray! This query uses a WITH clause because we need to refer to request_gap twice to select the row with the maximum value.
# AK: subquery
-- Compute the gaps
WITH request_gaps AS (
  SELECT date_created,
  -- lead or lag
  lag(date_created) OVER (ORDER BY date_created) AS previous,
  -- compute gap as date_created minus lead or lag
  date_created - lag(date_created) OVER (ORDER BY date_created) AS gap
  FROM evanston311)
-- Select the row with the maximum gap
SELECT *
  FROM request_gaps
ORDER BY gap DESC;
# date_created	            previous	                gap
# 2016-01-01 01:02:43+01:00	null	                    null
# 2018-01-07 19:41:34+01:00	2018-01-05 19:04:09+01:00	2 days, 0:37:25
# 2016-11-07 09:39:25+01:00	2016-11-05 19:06:21+01:00	1 day, 14:33:04
# 2017-03-13 06:58:15+01:00	2017-03-11 17:16:20+01:00	1 day, 13:41:55
# 2018-01-15 08:13:05+01:00	2018-01-13 20:42:15+01:00	1 day, 11:30:50
# 2016-05-09 09:25:14+02:00	2016-05-07 22:04:50+02:00	1 day, 11:20:24

# Rats!
# Requests in category "Rodents- Rats" average over 64 days to resolve. Why?
# Investigate in 4 steps:
# Why is the average so high? Check the distribution of completion times. Hint: date_trunc() can be used on intervals.
# See how excluding outliers influences average completion times.
# Do requests made in busy months take longer to complete? Check the correlation between the average completion time and requests per month.
# Compare the number of requests created per month to the number completed.
# Remember: the time to resolve, or completion time, is date_completed - date_created.
# Instructions 1/4
# - Use date_trunc() to examine the distribution of rat request completion times by number of days.
-- Truncate the time to complete requests to the day
SELECT date_trunc('day', date_completed - date_created) AS completion_time,
-- Count requests with each truncated time
COUNT(*)
FROM evanston311
-- Where category is rats
WHERE category = 'Rodents- Rats'
-- Group and order by the variable of interest
GROUP BY completion_time
ORDER BY completion_time;
# completion_time	count
# 0:00:00	        73
# 1 day, 0:00:00	17
# 2 days, 0:00:00	23
# 3 days, 0:00:00	11
# 4 days, 0:00:00	6
# 5 days, 0:00:00	6
# 6 days, 0:00:00	5
# 7 days, 0:00:00	7
# 8 days, 0:00:00	6
# 9 days, 0:00:00	10
#10 days, 0:00:00	7
# AK: date difference calculation different
# -- Truncate the time to complete requests to the day
SELECT date_trunc('day', date_completed) - date_trunc('day', date_created) AS completion_time,
-- Count requests with each truncated time
COUNT(*)
FROM evanston311
-- Where category is rats
WHERE category = 'Rodents- Rats'
-- Group and order by the variable of interest
GROUP BY completion_time
ORDER BY completion_time;
# completion_time	count
# 0:00:00	42
# 1 day, 0:00:00	43
# 2 days, 0:00:00	10
# 3 days, 0:00:00	23
# 3 days, 1:00:00	1
# 4 days, 0:00:00	8
# 5 days, 0:00:00	5
# 6 days, 0:00:00	7
# 7 days, 0:00:00	5
# 8 days, 0:00:00	8
# 9 days, 0:00:00	3
#10 days, 0:00:00	12
#10 days, 1:00:00	2
# - Compute average completion time per category excluding the longest 5% of requests (outliers).
SELECT category, 
-- Compute average completion time per category
AVG(date_completed - date_created) AS avg_completion_time
FROM evanston311
-- Where completion time is less than the 95th percentile value
WHERE date_completed - date_created < 
  -- Compute the 95th percentile of completion time in a subquery
(SELECT percentile_disc(0.95) WITHIN GROUP (ORDER BY (date_completed - date_created))
  FROM evanston311)
GROUP BY category
-- Order the results
ORDER BY avg_completion_time DESC;
# category	                                            avg_completion_time
# Trash Cart - Downsize, Upsize or Remove	              12 days, 17:47:50.586912
# Sanitation Billing Questions	                        12 days, 11:13:25.888889
# THIS REQUEST IS INACTIVE...Trash Cart - Compost Bin	  12 days, 6:32:42.024390
# Trash, Recycling, Yard Waste Cart- Repair/Replacement	11 days, 18:48:27.488108
# Rodents- Rats	                                        11 days, 8:58:00.840849
# Landmark Building Plaque or Nomination	              11 days, 5:11:11.666667
# AK: with outliers
SELECT category, 
-- Compute average completion time per category
AVG(date_completed - date_created) AS avg_completion_time
FROM evanston311
-- Where completion time is less than the 95th percentile value
--WHERE date_completed - date_created < 
-- Compute the 95th percentile of completion time in a subquery
--         (SELECT percentile_disc(0.95) WITHIN GROUP (ORDER BY (date_completed - date_created))
--            FROM evanston311)
GROUP BY category
-- Order the results
ORDER BY avg_completion_time DESC;
# category	                                          avg_completion_time
# Rodents- Rats	                                      64 days, 10:58:23.000766
# Fire Prevention - Public Education	                34 days, 16:48:10
# Key Request - All  City Employees	                  32 days, 0:52:11
# Smoking	                                            27 days, 7:42:30.238095
# Notice of Violation	                                24 days, 5:41:12.666667
# Exterior Conditions	                                23 days, 22:18:31.087719
# General/Routine Maintenance - Facilities Management	23 days, 3:01:56.480000
# Trash - Special Pickup (STAFF ONLY)	                19 days, 6:04:52.096070
# Public Transit Agency Issue	                        19 days, 3:28:15.845070
# Private Utility Service Issue	                      17 days, 19:50:13.130000
# Accessibility	                                      15 days, 1:55:53.200000
# Trash Cart - Downsize, Upsize or Remove	            13 days, 21:49:58.179842
# AK: percentiles per categories (not overall)
select category,
  percentile_disc(0.95) WITHIN GROUP (ORDER BY (date_completed - date_created)) AS p95_category,
  percentile_disc(0.50) WITHIN GROUP (ORDER BY (date_completed - date_created)) AS p50_category,
  (SELECT percentile_disc(0.95) WITHIN GROUP (ORDER BY (date_completed - date_created)) FROM evanston311) as p95_overall
from evanston311
GROUP BY category
ORDER BY p95_category desc;
# category	                                          p95_category	      p50_category	    p95_overall
# Smoking	                                            173 days, 0:11:56	  9 days, 20:20:15	28 days, 0:21:37
# Rodents- Rats	                                      164 days, 21:04:56	51 days, 21:18:09	28 days, 0:21:37
# Public Transit Agency Issue	                        123 days, 3:41:11	  4 days, 19:34:51	28 days, 0:21:37
# Notice of Violation	                                111 days, 5:54:36	  14 days, 23:55:54	28 days, 0:21:37
# Fire Prevention - Public Education	                100 days, 4:31:48	  6 days, 1:06:04	  28 days, 0:21:37
# General/Routine Maintenance - Facilities Management	97 days, 21:00:37	  5 days, 16:43:44	28 days, 0:21:37
# - Get corr() between avg. completion time and monthly requests. EXTRACT(epoch FROM interval) returns seconds in interval.
-- Compute correlation (corr) between 
-- avg_completion time and count from the subquery
SELECT CORR(avg_completion, count)
-- Convert date_created to its month with date_trunc
FROM (SELECT date_trunc('month', date_created) AS month, 
      -- Compute average completion time in number of seconds           
      AVG(EXTRACT(epoch FROM date_completed - date_created)) AS avg_completion, 
      -- Count requests per month
      count(*) AS count
      FROM evanston311
      -- Limit to rodents
      WHERE category='Rodents- Rats' 
      -- Group by month, created above
      GROUP BY month) 
-- Required alias for subquery 
AS monthly_avgs;
# corr
# 0.23199855213424334
# AK: subquery
SELECT date_trunc('month', date_created) AS month, 
-- Compute average completion time in number of seconds           
AVG(EXTRACT(epoch FROM date_completed - date_created)) AS avg_completion, 
-- Count requests per month
count(*) AS count
FROM evanston311
-- Limit to rodents
WHERE category='Rodents- Rats' 
-- Group by month, created above
GROUP BY month;
# month	                    avg_completion	      count
# 2017-10-01 00:00:00+02:00	4259545.192307692308	78
# 2016-07-01 00:00:00+02:00	6074154.275000000000	80
# 2016-10-01 00:00:00+02:00	4843639.092105263158	76
# 2018-02-01 00:00:00+01:00	4421533.000000000000	10
# 2018-01-01 00:00:00+01:00	5101503.842105263158	19
# 2017-02-01 00:00:00+01:00	4730550.850000000000	20
# AK
SELECT EXTRACT(epoch FROM '2016-01-13 16:03:18+01:00'::timestamp -	'2016-01-19 17:51:26+01:00'::timestamp);
# extract
# -524888.000000
SELECT EXTRACT(epoch FROM '2016-01-13'::timestamp -	'2016-01-19'::timestamp);
# extract
# -518400.000000
# - Select the number of requests created and number of requests completed per month.
-- Compute monthly counts of requests created
WITH created AS (
  SELECT date_trunc('month', date_created) AS month,
  count(*) AS created_count
  FROM evanston311
  WHERE category='Rodents- Rats'
  GROUP BY month),
-- Compute monthly counts of requests completed
completed AS (
  SELECT date_trunc('month', date_completed) AS month,
  count(*) AS completed_count
  FROM evanston311
  WHERE category='Rodents- Rats'
  GROUP BY month)
-- Join monthly created and completed counts
SELECT created.month, 
created_count, 
completed_count
FROM created
INNER JOIN completed
ON created.month=completed.month
ORDER BY created.month;
# month	created_count	completed_count
# 2016-01-01 00:00:00+01:00	10	1
# 2016-02-01 00:00:00+01:00	22	11
# 2016-03-01 00:00:00+01:00	31	14
# 2016-04-01 00:00:00+02:00	36	16
# 2016-05-01 00:00:00+02:00	40	19
# 2016-06-01 00:00:00+02:00	41	49
# Outstanding! There is a slight correlation between completion times and the number of requests per month. But the bigger issue is the disproportionately large number of requests completed in November 2017.








































































































































 




















