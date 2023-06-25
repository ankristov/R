
# 1 We'll take the CASE.mp4
# Basic CASE statements
# What is your favorite team?
# The European Soccer Database contains data about 12,800 matches from 11 countries played between 2011-2015! Throughout this course, you will be shown filtered versions of the tables in this database in order to better explore their contents.
# In this exercise, you will identify matches played between FC Schalke 04 and FC Bayern Munich. There are 2 teams identified in each match in the hometeam_id and awayteam_id columns, available to you in the filtered matches_germany table. ID can join to the team_api_id column in the teams_germany table, but you cannot perform a join on both at the same time.
# However, you can perform this operation using a CASE statement once you've identified the team_api_id associated with each team!
# Instructions
# - Select the team's long name and API id from the teams_germany table.
# - Filter the query for FC Schalke 04 and FC Bayern Munich using IN, giving you the team_api_IDs needed for the next step.
select * from teams_germany limit 5;
# id	team_api_id	team_long_name	    team_short_name
# 15617	     9823	FC Bayern Munich	  BMU
# 15618  	   9790	Hamburger SV	      HAM
# 15619	     8178	Bayer 04 Leverkusen	LEV
# 15620	     9789	Borussia Dortmund	  DOR
select * from matches_germany limit 5;
# id	country_id	season	stage	date	hometeam_id	awayteam_id	home_goal	away_goal
# 8727	7809	2011/2012	1	2011-08-05	9789	9790	3	1
# 8728	7809	2011/2012	1	2011-08-06	8697	8350	2	0
# 8729	7809	2011/2012	1	2011-08-06	9904	8226	2	1
SELECT
-- Select the team long name and team API id
team_long_name,
team_api_id
FROM teams_germany
-- Only include FC Schalke 04 and FC Bayern Munich
WHERE team_long_name in ('FC Schalke 04', 'FC Bayern Munich');
# team_long_name	team_api_id
# FC Bayern Munich	9823
# FC Schalke 04	10189

# - Create a CASE statement that identifies whether a match in Germany included FC Bayern Munich, FC Schalke 04, or neither as the home team.
# - Group the query by the CASE statement alias, home_team.
-- Identify the home team as Bayern Munich, Schalke 04, or neither
SELECT 
CASE WHEN hometeam_id = 10189 THEN 'FC Schalke 04'
WHEN hometeam_id = 9823 THEN 'FC Bayern Munich'
ELSE 'Other' END AS home_team,
COUNT(id) AS total_matches
FROM matches_germany
-- Group by the CASE statement alias
GROUP BY home_team;
# home_team	        total_matches
# FC Bayern Munich	68
# Other	            1088
# FC Schalke 04	    68
# Great job! You were able to identify two teams using CASE statement instead of a join, which 
# could only identify one at a time. Let's learn some more about key teams in Europe!

# CASE statements comparing column values
# Barcelona is considered one of the strongest teams in Spain's soccer league.
# In this exercise, you will be creating a list of matches in the 2011/2012 season where Barcelona was the home team. You will do this using a CASE statement that compares the values of two columns to create a new group -- wins, losses, and ties.
# In 3 steps, you will build a query that identifies a match's winner, identifies the identity of the opponent, and finally filters for Barcelona as the home team. Completing a query in this order will allow you to watch your results take shape with each new piece of information.
# The matches_spain table currently contains Barcelona's matches from the 2011/2012 season, and has two key columns, hometeam_id and awayteam_id, that can be joined with the teams_spain table. However, you can only join teams_spain to one column at a time.
# id	country_id	season	stage	date	hometeam_id	awayteam_id	home_goal	away_goal
# matches_spain
# id	country_id	season	stage	date	hometeam_id	awayteam_id	home_goal	away_goal
# 22658	21518	2011/2012	1	2012-01-21	8560	9906	0	4
# 22659	21518	2011/2012	1	2012-01-22	8633	8315	4	1
# 22660	21518	2011/2012	1	2012-01-22	9864	8634	1	4
# teams_spain
# id	team_api_id	team_long_name	team_short_name
# 43035	10267	Valencia CF	   VAL
# 43036	8661	RCD Mallorca	 MAL
# 43037	8371	CA Osasuna	   OSA
# Instructions
# - Select the date of the match and create a CASE statement to identify matches as home wins, home losses, or ties.
SELECT 
-- Select the date of the match
date,
-- Identify home wins, losses, or ties
CASE WHEN home_goal > away_goal THEN 'Home win!'
WHEN home_goal < away_goal THEN 'Home loss :(' 
ELSE 'Tie' END as outcome
FROM matches_spain;
# date	      outcome
# 2012-01-21	Home loss :(
# 2012-01-22	Home win!
# 2012-01-22	Home loss :(
# 2012-01-23	Home win!
# - Left join the teams_spain table team_api_id column to the matches_spain table awayteam_id. This allows us to retrieve the away team's identity.
# - Select team_long_name from teams_spain as opponent and complete the CASE statement from Step 1.
SELECT 
m.date,
--Select the team long name column and call it 'opponent'
t.team_long_name AS opponent, 
-- Complete the CASE statement with an alias
CASE WHEN m.home_goal > m.away_goal THEN 'Home win!'
WHEN m.home_goal < m.away_goal THEN 'Home loss :('
ELSE 'Tie' END AS outcome
FROM matches_spain AS m
-- Left join teams_spain onto matches_spain
LEFT JOIN teams_spain AS t
ON m.awayteam_id = t.team_api_id;
# date	      opponent	  outcome
# 2011-10-15	Valencia CF	Tie
# 2011-09-24	Valencia CF	Home win!
# 2011-09-17	Valencia CF	Home loss :(
# - Complete the same CASE statement as the previous steps.
# - Filter for matches where the home team is FC Barcelona (id = 8634).
SELECT 
m.date,
t.team_long_name AS opponent,
-- Complete the CASE statement with an alias
CASE WHEN m.home_goal > m.away_goal THEN 'Barcelona win!'
WHEN m.home_goal < m.away_goal THEN 'Barcelona loss :(' 
ELSE 'Tie' END AS outcome 
FROM matches_spain AS m
LEFT JOIN teams_spain AS t 
ON m.awayteam_id = t.team_api_id
-- Filter for Barcelona as the home team
WHERE m.hometeam_id = 8634; 
# date	opponent	outcome
# 2011-10-29	RCD Mallorca	Barcelona win!
# 2011-11-19	Real Zaragoza	Barcelona win!
# 2011-12-03	Levante UD	Barcelona win!
# 2011-11-29	Rayo Vallecano	Barcelona win!
# Great job! Did you notice how often Barcelona tends to win matches?

# CASE statements comparing two column values part 2
# Similar to the previous exercise, you will construct a query to determine the outcome of Barcelona's matches where they played as the away team. You will learn how to combine these two queries in chapters 2 and 3.
# Did their performance differ from the matches where they were the home team?
# Instructions
# - Complete the CASE statement to identify Barcelona's away team games (id = 8634) as wins, losses, or ties.
# - Left join the teams_spain table team_api_id column on the matches_spain table hometeam_id column. This retrieves the identity of the home team opponent.
# - Filter the query to only include matches where Barcelona was the away team.
-- Select matches where Barcelona was the away team
SELECT  
m.date,
t.team_long_name AS opponent,
CASE WHEN m.home_goal < m.away_goal THEN 'Barcelona win!'
WHEN m.home_goal > m.away_goal THEN 'Barcelona loss :(' 
ELSE 'Tie' END AS outcome
FROM matches_spain AS m
-- Join teams_spain to matches_spain
LEFT JOIN teams_spain AS t 
ON m.hometeam_id = t.team_api_id
WHERE m.awayteam_id = 8634;
# date	      opponent	               outcome
# 2012-01-22	Málaga CF	               Barcelona win!
# 2011-10-25	Granada CF	             Barcelona win!
# 2011-11-06	Athletic Club de Bilbao	 Tie
# 2011-11-26	Getafe CF	               Barcelona loss :(
# Fantastic! Barcelona's performance seems to be worse when they are the away team. We'll explore 
# this further in the next lesson.

# 2 In CASE things get more complex.mp4
# In CASE of rivalry
# Barcelona and Real Madrid have been rival teams for more than 80 years. Matches between these two teams are given the name El Clásico (The Classic). In this exercise, you will query a list of matches played between these two rivals.
# You will notice in Step 2 that when you have multiple logical conditions in a CASE statement, you may quickly end up with a large number of WHEN clauses to logically test every outcome you are interested in. It's important to make sure you don't accidentally exclude key information in your ELSE clause.
# In this exercise, you will retrieve information about matches played between Barcelona (id = 8634) and Real Madrid (id = 8633). Note that the query you are provided with already identifies the Clásico matches using a filter in the WHERE clause.
# Instructions
# Complete the first CASE statement, identifying Barcelona or Real Madrid as the home team using the hometeam_id column.
# Complete the second CASE statement in the same way, using awayteam_id.
SELECT 
date,
-- Identify the home team as Barcelona or Real Madrid
CASE WHEN hometeam_id = 8634 THEN 'FC Barcelona' 
WHEN hometeam_id = 8633 THEN 'Real Madrid CF' END AS home,
-- Identify the away team as Barcelona or Real Madrid
CASE WHEN awayteam_id = 8634 THEN 'FC Barcelona' 
WHEN awayteam_id = 8633 THEN 'Real Madrid CF' END AS away
FROM matches_spain
WHERE (awayteam_id = 8634 OR hometeam_id = 8634)
AND (awayteam_id = 8633 OR hometeam_id = 8633);
# date	      home	             away
# 2011-12-10	Real Madrid CF	   FC Barcelona
# 2012-04-21	FC Barcelona	     Real Madrid CF
# 2013-03-02	Real Madrid CF	   FC Barcelona
# - Construct the final CASE statement identifying who won each match. Note there are 3 possible outcomes, but 5 conditions that you need to identify.
# - Fill in the logical operators to identify Barcelona or Real Madrid as the winner.
SELECT 
date,
CASE WHEN hometeam_id = 8634 THEN 'FC Barcelona' 
ELSE 'Real Madrid CF' END as home,
CASE WHEN awayteam_id = 8634 THEN 'FC Barcelona' 
ELSE 'Real Madrid CF' END as away,
-- Identify all possible match outcomes
CASE WHEN home_goal > away_goal AND hometeam_id = 8634 THEN 'Barcelona win!'
WHEN home_goal > away_goal AND hometeam_id = 8633 THEN 'Real Madrid win!'
WHEN home_goal < away_goal AND awayteam_id = 8634 THEN 'Barcelona win!'
WHEN home_goal < away_goal AND awayteam_id = 8633 THEN 'Real Madrid win!'
ELSE 'Tie!' END AS outcome
FROM matches_spain
WHERE (awayteam_id = 8634 OR hometeam_id = 8634)
AND (awayteam_id = 8633 OR hometeam_id = 8633);
# date	      home	              away	          outcome
# 2011-12-10	Real Madrid CF	    FC Barcelona  	Barcelona win!
# 2012-04-21	FC Barcelona	      Real Madrid CF	Real Madrid win!
# 2013-03-02	Real Madrid CF	    FC Barcelona	  Real Madrid win!
# 2012-10-07	FC Barcelona	      Real Madrid CF	Tie!
# Fantastic! These two teams are fairly evenly matched!

# Filtering your CASE statement
# Let's generate a list of matches won by Italy's Bologna team! There are quite a few additional teams in the two tables, so a key part of generating a usable query will be using your CASE statement as a filter in the WHERE clause.
# CASE statements allow you to categorize data that you're interested in -- and exclude data you're not interested in. In order to do this, you can use a CASE statement as a filter in the WHERE statement to remove output you don't want to see.
# Here is how you might set that up:
# SELECT *
# FROM table
# WHERE 
#     CASE WHEN a > 5 THEN 'Keep'
#          WHEN a <= 5 THEN 'Exclude' END = 'Keep';
# In essence, you can use the CASE statement as a filtering column like any other column in your database. The only difference is that you don't alias the statement in WHERE.
# matches_italy
# id	country_id	season	stage	date	hometeam_id	awayteam_id	home_goal	away_goal
# 11397	10257	2011/2012	1	2011-12-21	8524	9880	4	1
# 11398	10257	2011/2012	1	2011-12-21	9857	8686	0	2
# 11399	10257	2011/2012	1	2011-12-20	8529	8564	0	2
# teams_italy
# id	team_api_id	team_long_name	team_short_name
# 20513	8524	Atalanta	ATA
# 20514	8551	Siena	SIE
# 20515	8529	Cagliari	CAG
# 20516	8543	Lazio	LAZ
# Instructions
# - Identify Bologna's team ID listed in the teams_italy table by selecting the team_long_name and team_api_id.
-- Select team_long_name and team_api_id from team
SELECT
team_long_name,
team_api_id
FROM teams_italy
-- Filter for team long name
WHERE team_long_name = 'Bologna';
# team_long_name	team_api_id
# Bologna	        9857
# - Select the season and date that a match was played.
# - Complete the CASE statement so that only Bologna's home and away wins are identified.
-- Select the season and date columns
SELECT 
season,
date,
-- Identify when Bologna won a match
CASE WHEN hometeam_id = 9857 
AND home_goal > away_goal 
THEN 'Bologna Win'
WHEN awayteam_id = 9857 
AND away_goal > home_goal 
THEN 'Bologna Win' 
END AS outcome
FROM matches_italy;
# season	  date	      outcome
# 2011/2012	2011-12-21	null
# 2011/2012	2011-12-21	null
# 2011/2012	2011-12-20	null
# Select the home_goal and away_goal for each match.
# Use the CASE statement in the WHERE clause to filter all NULL values generated by the statement in the previous step.
-- Select the season, date, home_goal, and away_goal columns
SELECT 
season,
date,
home_goal,
away_goal
FROM matches_italy
WHERE 
-- Exclude games not won by Bologna
CASE WHEN hometeam_id = 9857 AND home_goal > away_goal THEN 'Bologna Win'
WHEN awayteam_id = 9857 AND away_goal > home_goal THEN 'Bologna Win' 
END IS NOT NULL;
# season	  date	      home_goal	away_goal
# 2011/2012	2011-10-30	3	        1
# 2011/2012	2011-12-04	1	        0
# 2011/2012	2012-01-08	2	        0
# Good work! You now have details on every match in this database where Bologna won. We will work 
# on adding more useful information to your queries later in this course.

# 3 CASE WHEN with aggregate functions.mp4
# COUNT using CASE WHEN
# Do the number of soccer matches played in a given European country differ across seasons? We will use the European Soccer Database to answer this question.
# You will examine the number of matches played in 3 seasons within each country listed in the database. This is much easier to explore with each season's matches in separate columns. Using the country and unfiltered match table, you will count the number of matches played in each country during the 2012/2013, 2013/2014, and 2014/2015 match seasons.
# country
# id	  name
# 1	    Belgium
# 1729	England
# 4769	France
# 7809	Germany
# match
# id	country_id	season	  stage	date	hometeam_id	awayteam_id	home_goal	away_goal
# 757	1	          2011/2012	1	    2011-07-29	1773	8635	2	1
# 758	1	          2011/2012	1	    2011-07-30	9998	9985	1	1
# 759	1	          2011/2012	1	    2011-07-30	9987	9993	3	1
# 760	1	          2011/2012	1	    2011-07-30	9991	9984	0	1
# Instructions
# - Create a CASE statement that identifies the id of matches played in the 2012/2013 season. Specify that you want ELSE values to be NULL.
# - Wrap the CASE statement in a COUNT function and group the query by the country alias.
# SELECT 
c.name AS country,
-- Count games from the 2012/2013 season
COUNT(CASE WHEN m.season = '2012/2013' 
      THEN m.id ELSE NULL END) AS matches_2012_2013
FROM country AS c
LEFT JOIN match AS m
ON c.id = m.country_id
-- Group by country name alias
GROUP BY c.name;
# country	    matches_2012_2013
# Portugal	  240
# France	    380
# Scotland	  228
# Netherlands	306
# - Create 3 CASE WHEN statements counting the matches played in each country across the 3 seasons.
# - END your CASE statement without an ELSE clause.
SELECT 
c.name AS country,
-- Count matches in each of the 3 seasons
COUNT(CASE WHEN m.season = '2012/2013' THEN m.id END) AS matches_2012_2013,
COUNT(CASE WHEN m.season = '2013/2014' THEN m.id END) AS matches_2013_2014,
COUNT(CASE WHEN m.season = '2014/2015' THEN m.id END) AS matches_2014_2015
FROM country AS c
LEFT JOIN match AS m
ON c.id = m.country_id
-- Group by country name alias
GROUP BY c.name;
# country    	matches_2012_2013	matches_2013_2014	matches_2014_2015
# Portugal	  240	              240	              306
# France	    380	              380	              380
# Scotland	  228	              228	              228
# Netherlands	306	              306	              306
# Fantastic! The number of matches played in each season seems relatively consistent across 
# countries. Where do you see the largest difference?

# COUNT and CASE WHEN with multiple conditions
# In R or Python, you have the ability to calculate a SUM of logical values (i.e., TRUE/FALSE) directly. In SQL, you have to convert these values into 1 and 0 before calculating a sum. This can be done using a CASE statement.
# There's one key difference when using SUM to aggregate logical values compared to using COUNT in the previous exercise --
# Your goal here is to use the country and match table to determine the total number of matches won by the home team in each country during the 2012/2013, 2013/2014, and 2014/2015 seasons.
# Instructions
# - Create 3 CASE statements to "count" matches in the '2012/2013', '2013/2014', and '2014/2015' seasons, respectively.
# - Have each CASE statement return a 1 for every match you want to include, and a 0 for every match to exclude.
# - Wrap the CASE statement in a SUM to return the total matches played in each season.
# - Group the query by the country name alias.
SELECT 
c.name AS country,
-- Sum the total records in each season where the home team won
SUM(CASE WHEN m.season = '2012/2013' AND m.home_goal > m.away_goal 
    THEN 1 ELSE 0 END) AS matches_2012_2013,
SUM(CASE WHEN m.season = '2013/2014' AND m.home_goal > m.away_goal 
    THEN 1 ELSE 0 END) AS matches_2013_2014,
SUM(CASE WHEN m.season = '2014/2015' AND m.home_goal > m.away_goal 
    THEN 1 ELSE 0 END) AS matches_2014_2015
FROM country AS c
LEFT JOIN match AS m
ON c.id = m.country_id
-- Group by country name alias
GROUP BY c.name;
# country	    matches_2012_2013	matches_2013_2014	matches_2014_2015
# Portugal	  103	              108	              137
# France	    170	              168	              181
# Scotland	  89	              102	              102
# Netherlands	137	              144	              138
# Good job! You now have a second way of calculating the results from the previous exercise.

# Calculating percent with CASE and AVG
# CASE statements will return any value you specify in your THEN clause. This is an incredibly powerful tool for robust calculations and data manipulation when used in conjunction with an aggregate statement. One key task you can perform is using CASE inside an AVG function to calculate a percentage of information in your database.
# Here's an example of how you set that up:
# AVG(CASE WHEN condition_is_met THEN 1
#          WHEN condition_is_not_met THEN 0 END)
# With this approach, it's important to accurately specify which records count as 0, otherwise your calculations may not be correct!
# Your task is to examine the number of wins, losses, and ties in each country. The matches table is filtered to include all matches from the 2013/2014 and 2014/2015 seasons.
# Instructions 
# - Create 3 CASE statements to COUNT the total number of home team wins, away team wins, and ties, which will allow you to examine the total number of records.
SELECT 
c.name AS country,
-- Count the home wins, away wins, and ties in each country
COUNT(CASE WHEN m.home_goal > m.away_goal THEN m.id 
      END) AS home_wins,
COUNT(CASE WHEN m.home_goal < m.away_goal THEN m.id 
      END) AS away_wins,
COUNT(CASE WHEN m.home_goal = m.away_goal THEN m.id 
      END) AS ties
FROM country AS c
LEFT JOIN matches AS m
ON c.id = m.country_id
GROUP BY country;
# country	  home_wins	away_wins	ties
# Portugal	245	      156	      145
# France	  349	      215	      196
# Scotland	204	      158	      94
# - Calculate the percentage of matches tied using a CASE statement inside AVG.
# - Fill in the logical operators for each statement. Alias your columns as ties_2013_2014 and ties_2014_2015, respectively.
# - The previous "ties" columns returned values with 14 decimal points, which is not easy to interpret. Use the ROUND function to round to 2 decimal points.
SELECT 
c.name AS country,
-- Round the percentage of tied games to 2 decimal points
ROUND(AVG(CASE WHEN m.season='2013/2014' AND m.home_goal = m.away_goal THEN 1
          WHEN m.season='2013/2014' AND m.home_goal != m.away_goal THEN 0
          END),2) AS pct_ties_2013_2014,
ROUND(AVG(CASE WHEN m.season='2014/2015' AND m.home_goal = m.away_goal THEN 1
          WHEN m.season='2014/2015' AND m.home_goal != m.away_goal THEN 0
          END),2) AS pct_ties_2014_2015
FROM country AS c
LEFT JOIN matches AS m
ON c.id = m.country_id
GROUP BY country;
# country	    pct_ties_2013_2014	pct_ties_2014_2015
# Portugal	  0.25	              0.28
# France	    0.28	              0.23
# Scotland	  0.22	              0.19
# Netherlands	0.27	              0.24
# Spain	      0.23	              0.24
# Fantastic! That's much easier to read. What patterns/differences are you seeing in the results? 
# Who tends to score ties more often?

# 4 WHERE are the Subqueries?.mp4
# Filtering using scalar subqueries
# Subqueries are incredibly powerful for performing complex filters and transformations. You can 
 #filter data based on single, scalar values using a subquery in ways you cannot by using WHERE 
# statements or joins. Subqueries can also be used for more advanced manipulation of your data set. 
# You will likely encounter subqueries in any real-world setting that uses relational databases.
# In this exercise, you will generate a list of matches where the total goals scored (for both 
# teams in total) is more than 3 times the average for games in the matches_2013_2014 table, 
# which includes all games played in the 2013/2014 season.
# Instructions
# Calculate triple the average home + away goals scored across all matches. This will become your 
# subquery in the next step. Note that this column does not have an alias, so it will be called 
# ?column? in your results.
# matches_2013_2014
# id	country_id	season	stage	date	hometeam_id	awayteam_id	home_goal	away_goal
# 1237	1	2013/2014	1	2014-03-29	8475	9989	2	0
# 1238	1	2013/2014	1	2014-03-29	9991	8573	0	1
# 1239	1	2013/2014	2	2014-04-05	9989	9991	1	0
# - Calculate triple the average home + away goals scored across all matches. This will become your 
# subquery in the next step. Note that this column does not have an alias, so it will be called ?column? in your results.
-- Select the average of home + away goals, multiplied by 3
SELECT 
3 * AVG(home_goal + away_goal)
FROM matches_2013_2014;
# 8.3004617414248020
# - Select the date, home goals, and away goals in the main query.
# - Filter the main query for matches where the total goals scored exceed the value in the subquery.
SELECT 
-- Select the date, home goals, and away goals scored
date,
home_goal,
away_goal
FROM  matches_2013_2014
-- Filter for matches where total goals exceeds 3x the average
WHERE (home_goal + away_goal) > 
  (SELECT 3 * AVG(home_goal + away_goal)
   FROM matches_2013_2014); 
# date	      home_goal	away_goal
# 2013-12-14	6	        3
# 2014-03-22	3	        6
# 2013-10-30	7	        3
# Look at those scores! This calculation would have been much more difficult without a subquery.

# Filtering using a subquery with a list
# Your goal in this exercise is to generate a list of teams that never played a game in their home city. Using a subquery, you will generate a list of unique hometeam_ID values from the unfiltered match table to exclude in the team table's team_api_ID column.
# In addition to filtering using a single-value (scalar) subquery, you can create a list of values in a subquery to filter data based on a complex set of conditions. This type of subquery generates a one column reference list for the main query. As long as the values in your list match a column in your main query's table, you don't need to use a join -- even if the list is from a separate table.
# Instructions
# - Create a subquery in the WHERE clause that retrieves all unique hometeam_ID values from the match table.
# - Select the team_long_name and team_short_name from the team table. Exclude all values from the subquery in the main query.
SELECT 
-- Select the team long and short names
team_long_name,
team_short_name
FROM team 
-- Exclude all values from the subquery
WHERE team_api_id NOT IN
(select DISTINCT hometeam_ID  FROM match)
# team_long_name	      team_short_name
# FCV Dender EH	        DEN
# KSV Roeselare	        ROS
# Tubize	              TUB
# Royal Excel Mouscron	MOU
# Great job! There are 52 teams without any home games recorded in this database. Why do you think that is?

# Filtering with more complex subquery conditions
# In the previous exercise, you generated a list of teams that have no home matches listed in the soccer database using a subquery in WHERE. Let's do some further exploration in this database by creating a list of teams that scored 8 or more goals in a home match.
# In order to do this, you will construct a subquery in the WHERE statement with its own filtering condition.
# Instructions
# Create a subquery in WHERE clause that retrieves all hometeam_ID values from match with a home_goal score greater than or equal to 8.
# Select the team_long_name and team_short_name from the team table. Include all values from the subquery in the main query.
SELECT
-- Select the team long and short names
team_long_name,
team_short_name
FROM team
-- Filter for teams with 8 or more home goals
WHERE team_api_id IN
(SELECT DISTINCT hometeam_ID
  FROM match
  WHERE home_goal >= 8);
# team_long_name	  team_short_name
# Manchester United	MUN
# Chelsea	          CHE
# Southampton	      SOU
# FC Bayern Munich	BMU
# Real Madrid CF	  REA
# Excellent job! This is an interesting list of teams!

# 5 Subqueries in FROM.mp4
# Joining Subqueries in FROM
# The match table in the European Soccer Database does not contain country or team names. You can get this information by joining it to the country table, and use this to aggregate information, such as the number of matches played in each country.
# If you're interested in filtering data from one of these tables, you can also create a subquery from one of the tables, and then join it to an existing table in the database. A subquery in FROM is an effective way of answering detailed questions that requires filtering or transforming data before including it in your final results.
# Your goal in this exercise is to generate a subquery using the match table, and then join that subquery to the country table to calculate information about matches with 10 or more goals in total!
# Instructions
# Create the subquery to be used in the next step, which selects the country ID and match ID (id) from the match table.
# Filter the query for matches with greater than or equal to 10 goals.
# Joining Subqueries in FROM
# The match table in the European Soccer Database does not contain country or team names. You can get this information by joining it to the country table, and use this to aggregate information, such as the number of matches played in each country.
# If you're interested in filtering data from one of these tables, you can also create a subquery from one of the tables, and then join it to an existing table in the database. A subquery in FROM is an effective way of answering detailed questions that requires filtering or transforming data before including it in your final results.
# Your goal in this exercise is to generate a subquery using the match table, and then join that subquery to the country table to calculate information about matches with 10 or more goals in total!
# Instructions
# - Create the subquery to be used in the next step, which selects the country ID and match ID (id) from the match table.
# - Filter the query for matches with greater than or equal to 10 goals.
SELECT 
-- Select the country ID and match ID
id, 
country_id 
FROM match
-- Filter for matches with 10 or more goals in total
WHERE (home_goal + away_goal) >= 10;
# country_id	id
# 1729	3093
# 1729	3369
# 1729	3566
# 7809	9211
# - Construct a subquery that selects only matches with 10 or more total goals.
# - Inner join the subquery onto country in the main query.
# - Select name from country and count the id column from match.
SELECT
-- Select country name and the count match IDs
c.name AS country_name,
COUNT(sub.id) AS matches
FROM country AS c
-- Inner join the subquery onto country
-- Select the country id and match id columns
INNER JOIN (SELECT country_id, id 
            FROM match
            -- Filter the subquery by matches with 10+ goals
            WHERE (home_goal + away_goal) >= 10) AS sub
ON c.id = sub.country_id
GROUP BY country_name;
# country_name	matches
# Netherlands	  1
# Spain	        4
# Germany	      1
# England	      3
# Good job! You generated a count of matches in each country where the total goals was higher 
# than 10. That's a lot of goals!

# Building on Subqueries in FROM
# In the previous exercise, you found that England, Netherlands, Germany and Spain were the only countries that had matches in the database where 10 or more goals were scored overall. Let's find out some more details about those matches -- when they were played, during which seasons, and how many of the goals were home versus away goals.
# You'll notice that in this exercise, the table alias is excluded for every column selected in the main query. This is because the main query is extracting data from the subquery, which is treated as a single table.
# Instructions
# Complete the subquery inside the FROM clause. Select the country name from the country table, along with the date, the home goal, the away goal, and the total goals columns from the match table.
# Create a column in the subquery that adds home and away goals, called total_goals. This will be used to filter the main query.
# Select the country, date, home goals, and away goals in the main query.
# Filter the main query for games with 10 or more total goals.
SELECT
-- Select country, date, home, and away goals from the subquery
country,
date,
home_goal,
away_goal
FROM 
-- Select country name, date, home_goal, away_goal, and total goals in the subquery
(SELECT c.name AS country, 
  m.date, 
  m.home_goal, 
  m.away_goal,
  (m.home_goal + m.away_goal) AS total_goals
  FROM match AS m
  LEFT JOIN country AS c
  ON m.country_id = c.id) AS subq
-- Filter by total goals scored in the main query
WHERE total_goals >= 10;
# country	    date	      home_goal	away_goal
# England	    2011-08-28	8	        2
# England	    2012-12-29	7	        3
# England	    2013-05-19	5	        5
# Germany	    2013-03-30	9	        2
# Netherlands	2011-11-06	6	        4
# Great job! You can manipulate the information you extract by placing joins in or using a 
# subquery. These must have been exciting matches!

# 6 Subqueries in SELECT.mp4
# league
# id	 country_id	name
# 1	   1	        Belgium Jupiler League
# 1729 1729	      England Premier League
# 4769 4769	      France Ligue 1
# 7809 7809	      Germany 1. Bundesliga
# match
# id	country_id	season	stage	date	hometeam_id	awayteam_id	home_goal	away_goal
# 757	1	2011/2012	1	2011-07-29	1773	8635	2	1
# 758	1	2011/2012	1	2011-07-30	9998	9985	1	1
# 759	1	2011/2012	1	2011-07-30	9987	9993	3	1
# 760	1	2011/2012	1	2011-07-30	9991	9984	0	1
# Add a subquery to the SELECT clause
# Subqueries in SELECT statements generate a single value that allow you to pass an aggregate value down a data frame. This is useful for performing calculations on data within your database.
# In the following exercise, you will construct a query that calculates the average number of goals per match in each country's league.
# Instructions
# - In the subquery, select the average total goals by adding home_goal and away_goal.
# - Filter the results so that only the average of goals in the 2013/2014 season is calculated.
# - In the main query, select the average total goals by adding home_goal and away_goal. This calculates the average goals for each league.
# - Filter the results in the main query the same way you filtered the subquery. Group the query by the league name.
SELECT 
l.name AS league,
-- Select and round the league s total goals
    ROUND(AVG(m.home_goal + m.away_goal), 2) AS avg_goals,
    -- Select & round the average total goals for the season
    (SELECT ROUND(AVG(home_goal + away_goal), 2) 
     FROM match
     WHERE season = '2013/2014') AS overall_avg
FROM league AS l
LEFT JOIN match AS m
ON l.country_id = m.country_id
-- Filter for the 2013/2014 season
WHERE season = '2013/2014'
GROUP BY league;
# league	                  avg_goals	overall_avg
# Switzerland Super League	2.89	    2.77
# Poland Ekstraklasa	      2.64	    2.77
# Netherlands Eredivisie	  3.20	    2.77
# Scotland Premier League	  2.75	    2.77
# Good job! With a subquery in select, you can compare each league's average to the overall average.
# Which leagues tend to score higher than average?

# Subqueries in Select for Calculations
# Subqueries in SELECT are a useful way to create calculated columns in a query. A subquery in SELECT can be treated as a single numeric value to use in your calculations. When writing queries in SELECT, it's important to remember that filtering the main query does not filter the subquery -- and vice versa.
# In the previous exercise, you created a column to compare each league's average total goals to the overall average goals in the 2013/2014 season. In this exercise, you will add a column that directly compares these values by subtracting the overall average from the subquery.
# Instructions
# - Select the average goals scored in a match for each league in the main query.
# - Select the average goals scored in a match overall for the 2013/2014 season in the subquery.
# - Subtract the subquery from the average number of goals calculated for each league.
# - Filter the main query so that only games from the 2013/2014 season are included.
SELECT
-- Select the league name and average goals scored
l.name AS league,
ROUND(AVG(m.home_goal + m.away_goal),2) AS avg_goals,
-- Subtract the overall average from the league average
ROUND(AVG(m.home_goal + m.away_goal) - 
        (SELECT AVG(home_goal + away_goal)
         FROM match 
         WHERE season = '2013/2014'),2) AS diff
FROM league AS l
LEFT JOIN match AS m
ON l.country_id = m.country_id
-- Only include 2013/2014 results
WHERE m.season = '2013/2014'
GROUP BY l.name;
# league	                  avg_goals	 diff
# Switzerland Super League	2.89	     0.12
# Poland Ekstraklasa	      2.64	    -0.13
# Netherlands Eredivisie	  3.20	     0.43
# Scotland Premier League	  2.75	    -0.02
# France Ligue 1	          2.46	    -0.31
# Spain LIGA BBVA	          2.75	    -0.02
# Good job! Games in the Netherlands tend to score the highest number of goals on average in this season. Why do you think that is?

# 7 Subqueries everywhere! And best practices!.mp4
# ALL the subqueries EVERYWHERE
# In soccer leagues, games are played at different stages. Winning teams progress from one stage to the next, until they reach the final stage. In each stage, the stakes become higher than the previous one. The match table includes data about the different stages that each match took place in.
# In this lesson, you will build a final query across 3 exercises that will contain three subqueries -- one in the SELECT clause, one in the FROM clause, and one in the WHERE clause. In the final exercise, your query will extract data examining the average goals scored in each stage of a match. Does the average number of goals scored change as the stakes get higher from one stage to the next?
# Instructions
# Extract the average number of home and away team goals in two SELECT subqueries.
# Calculate the average home and away goals for the specific stage in the main query.
# Filter both subqueries and the main query so that only data from the 2012/2013 season is included.
# Group the query by the m.stage column.
SELECT 
-- Select the stage and average goals for each stage
m.stage,
ROUND(AVG(m.home_goal + m.away_goal),2) AS avg_goals,
-- Select the average overall goals for the 2012/2013 season
ROUND((SELECT AVG(home_goal + away_goal) 
       FROM match 
       WHERE season = '2012/2013'),2) AS overall
FROM match AS m
-- Filter for the 2012/2013 season
WHERE m.season = '2012/2013'
-- Group by stage
GROUP BY m.stage;
# stage	avg_goals	overall
# 29	  2.63	    2.77
# 4	    2.80	    2.77
# 34	  2.68	    2.77
# 32	  2.75	    2.77
# Excellent job! You will continue building on this data set in the next exercises.

# Add a subquery in FROM
# In the previous exercise, you created a data set listing the average home and away goals in each match stage of the 2012/2013 match season.
# In this next step, you will turn the main query into a subquery to extract a list of stages where the average home goals in a stage is higher than the overall average for home goals in a match.
# Instructions
# - Calculate the average home goals and average away goals from the match table for each stage in the FROM clause subquery.
# - Add a subquery to the WHERE clause that calculates the overall average home goals.
# - Filter the main query for stages where the average home goals is higher than the overall average.
# - Select the stage and avg_goals columns from the s subquery into the main query.
SELECT 
-- Select the stage and average goals from the subquery
s.stage,
ROUND(avg_goals,2) AS avg_goals
FROM 
-- Select the stage and average goals in 2012/2013
(SELECT
  stage,
  AVG(home_goal + away_goal) AS avg_goals
  FROM match
  WHERE season = '2012/2013'
  GROUP BY stage) AS s
WHERE 
-- Filter the main query using the subquery
s.avg_goals > (SELECT AVG(home_goal + away_goal) 
               FROM match WHERE season = '2012/2013');
# stage	avg_goals
# 4	    2.80
# 10	  2.96
# 38	  3.17
# 6	    2.78
# 12	  3.23
# Excellent job! Quite a few stages were eliminated with this filter. Do you notice anything interesting?

# Add a subquery in SELECT
# In the previous exercise, you added a subquery to the FROM statement and selected the stages where the number of average goals in a stage exceeded the overall average number of goals in the 2012/2013 match season. In this final step, you will add a subquery in SELECT to compare the average number of goals scored in each stage to the total.
# Instructions
# - Create a subquery in SELECT that yields the average goals scored in the 2012/2013 season. Name the new column overall_avg.
# - Create a subquery in FROM that calculates the average goals scored in each stage during the 2012/2013 season.
# - Filter the main query for stages where the average goals exceeds the overall average in 2012/2013.
SELECT 
-- Select the stage and average goals from s
s.stage,
ROUND(s.avg_goals,2) AS avg_goal,
-- Select the overall average for 2012/2013
(SELECT AVG(home_goal + away_goal) from match WHERE season = '2012/2013') AS overall_avg
FROM 
-- Select the stage and average goals in 2012/2013 from match
(SELECT
  stage,
  AVG(home_goal + away_goal) AS avg_goals
  FROM match
  WHERE season = '2012/2013'
  GROUP BY stage) AS s
WHERE 
-- Filter the main query using the subquery
s.avg_goals > (SELECT AVG(home_goal + away_goal) 
               FROM match WHERE season = '2012/2013');
# stage	avg_goal	overall_avg
# 4	    2.80	    2.7726993865030675
# 10	  2.96	    2.7726993865030675
# 38	  3.17	    2.7726993865030675
# 6	    2.78	    2.7726993865030675
# 12	  3.23	    2.7726993865030675
# Amazing job! You used 3 subqueries to generate a list of stages that have higher than average goals scored in matches.

# 8 Correlated subqueries.mp4
# Basic Correlated Subqueries
# Correlated subqueries are subqueries that reference one or more columns in the main query. Correlated subqueries depend on information in the main query to run, and thus, cannot be executed on their own.
# Correlated subqueries are evaluated in SQL once per row of data retrieved -- a process that takes a lot more computing power and time than a simple subquery.
# In this exercise, you will practice using correlated subqueries to examine matches with scores that are extreme outliers for each country -- above 3 times the average score!
# Instructions
# - Select the country_id, date, home_goal, and away_goal columns in the main query.
# - Complete the AVG value in the subquery.
# - Complete the subquery column references, so that country_id is matched in the main and subquery.
SELECT 
-- Select country ID, date, home, and away goals from match
main.country_id,
main.date,
main.home_goal, 
main.away_goal
FROM match AS main
WHERE 
-- Filter the main query by the subquery
(home_goal + away_goal) > 
  (SELECT AVG((sub.home_goal + sub.away_goal) * 3)
   FROM match AS sub
   -- Join the main query to the subquery in WHERE
   WHERE main.country_id = sub.country_id);
# country_id	date	home_goal	away_goal
# 1	    2011-10-29	4	5
# 1729	2011-08-28	8	2
# 1729	2012-12-29	7	3
# 1729	2013-05-19	5	5
# 1729	2013-12-14	6	3
# 1729	2014-03-22	3	6
# Great job! Correlated subqueries take longer to produce results, but they often prevent you from having to create multiple subqueries.

# Correlated subquery with multiple conditions
# Correlated subqueries are useful for matching data across multiple columns. In the previous exercise, you generated a list of matches with extremely high scores for each country. In this exercise, you're going to add an additional column for matching to answer the question -- what was the highest scoring match for each country, in each season?
# *Note: this query may take a while to load.
# Instructions
# - Select the country_id, date, home_goal, and away_goal columns in the main query.
# - Complete the subquery: Select the matches with the highest number of total goals.
# - Match the subquery to the main query using country_id and season.
# - Fill in the correct logical operator so that total goals equals the max goals recorded in the subquery.
SELECT 
-- Select country ID, date, home, and away goals from match
main.country_id,
main.date,
main.home_goal,
main.away_goal
FROM match AS main
WHERE 
-- Filter for matches with the highest number of goals scored
(home_goal + away_goal) = 
  (SELECT MAX(sub.home_goal + sub.away_goal)
   FROM match AS sub
   WHERE main.country_id = sub.country_id
   AND main.season = sub.season);
# country_id	date	home_goal	away_goal
# 1	    2011-10-29	4	5
# 1	    2012-11-17	2	6
# 1	    2012-12-09	1	7
# 1	    2013-01-19	2	6
# 1	    2012-08-19	2	6
# 1	    2014-04-19	2	4
# ........
# 1729	2011-08-28	8	2
# 1729	2012-12-29	7	3
# 1729	2013-05-19	5	5
# 1729	2013-12-14	6	3
# 1729	2014-03-22	3	6
# Great job! Subqueries can be very useful for selecting data broken into multiple categories -- 
# though you may have to wait for the data to load!

# 9 Nested subqueries.mp4
# Nested simple subqueries
# Nested subqueries can be either simple or correlated.
# Just like an unnested subquery, a nested subquery's components can be executed independently of the outer query, while a correlated subquery requires both the outer and inner subquery to run and produce results.
# In this exercise, you will practice creating a nested subquery to examine the highest total 
# number of goals in each season, overall, and during July across all seasons.
# Instructions
# - Complete the main query to select the season and the max total goals in a match for each season. Name this max_goals.
# - Complete the first simple subquery to select the max total goals in a match across all seasons. Name this overall_max_goals.
# - Complete the nested subquery to select the maximum total goals in a match played in July across all seasons.
# - Select the maximum total goals in the outer subquery. Name this entire subquery july_max_goals.
SELECT
-- Select the season and max goals scored in a match
season,
MAX(home_goal + away_goal) AS max_goals,
-- Select the overall max goals scored in a match
(SELECT MAX(home_goal + away_goal) FROM match) AS overall_max_goals,
-- Select the max number of goals scored in any match in July
(SELECT MAX(home_goal + away_goal) 
  FROM match
  WHERE id IN (
    SELECT id FROM match WHERE EXTRACT(MONTH FROM date) = 07)) AS july_max_goals
-- AK: the same.. why we need this ^ construction??
  -- WHERE EXTRACT(MONTH FROM date) = 07) as july_max_goals
FROM match
GROUP BY season;
# season	  max_goal	overall_max_goals	july_max_goals
# 2013/2014	10	      11	              7
# 2012/2013	11	      11	              7
# 2014/2015	10	      11	              7
# 2011/2012	10	      11	              7
# Good job! You just set up a nested subquery inside your main query to get some interesting data.

# Nest a subquery in FROM
# What's the average number of matches per season where a team scored 5 or more goals? How does 
# this differ by country?
# Let's use a nested, correlated subquery to perform this operation. In the real world, you will 
# probably find that nesting multiple subqueries is a task you don't have to perform often. In 
# some cases, however, you may find yourself struggling to properly group by the column you want, 
# or to calculate information requiring multiple mathematical transformations (i.e., an AVG of a COUNT).
# Nesting subqueries and performing your transformations one step at a time, adding it to a subquery, and then performing the next set of transformations is often the easiest way to yield accurate information about your data. Let's get to it!
# Instructions
# Generate a list of matches where at least one team scored 5 or more goals.
-- Select matches where a team scored 5+ goals
SELECT
country_id,
season,
id
FROM match
WHERE home_goal >= 5 OR away_goal >= 5;
# country_id	season	id
# 1	2011/2012	764
# 1	2011/2012	766
# 1	2011/2012	781
# 1	2011/2012	791
# - Turn the query from the previous step into a subquery in the FROM statement.
# - COUNT the match ids generated in the previous step, and group the query by country_id and season.
-- Count match ids
SELECT
country_id,
season,
COUNT(subquery.id) AS matches
-- Set up and alias the subquery
FROM (
  SELECT
  country_id,
  season,
  id
  FROM match
  WHERE home_goal >= 5 OR away_goal >= 5)
as subquery
-- Group by country_id and season
GROUP BY country_id, season;
# country_id	season	  matches
# 19694	      2012/2013	5
# 21518	      2012/2013	23
# 13274	      2011/2012	24
# 17642	      2014/2015	12
# 13274	2013/2014	20
# - Finally, declare the same query from step 2 as a subquery in FROM with the alias outer_s.
# - Left join it to the country table using the outer query's country_id column.
# - Calculate an AVG of high scoring matches per country in the main query.
# SELECT
c.name AS country,
-- Calculate the average matches per season
AVG(outer_s.matches) AS avg_seasonal_high_scores
FROM country AS c
-- Left join outer_s to country
LEFT JOIN (
  SELECT country_id, season,
  COUNT(id) AS matches
  FROM (
    SELECT country_id, season, id
    FROM match
    WHERE home_goal >= 5 OR away_goal >= 5) AS inner_s
  -- Close parentheses and alias the subquery
  GROUP BY country_id, season) as outer_s
ON c.id = outer_s.country_id
GROUP BY country;
# country	    avg_seasonal_high_scores
# Portugal	  7.5000000000000000
# France	    8.0000000000000000
# Scotland	  8.0000000000000000
# Netherlands	21.0000000000000000
# Spain	      22.0000000000000000
# AK: this is avg. number of matches per country accross seasons
# Great job!! This required quite a few steps, but you've got the hang of steps necessary to nest 
# subqueries in FROM that you can apply to other parts of your query as well!

# 10 Common Table Expressions.mp4
# Clean up with CTEs
# In chapter 2, you generated a list of countries and the number of matches in each country with more than 10 total goals. The query in that exercise utilized a subquery in the FROM statement in order to filter the matches before counting them in the main query. Below is the query you created:
# SELECT
#   c.name AS country,
#   COUNT(sub.id) AS matches
# FROM country AS c
# INNER JOIN (
#   SELECT country_id, id 
#   FROM match
#   WHERE (home_goal + away_goal) >= 10) AS sub
# ON c.id = sub.country_id
# GROUP BY country;
# You can list one (or more) subqueries as common table expressions (CTEs) by declaring them ahead of your main query, which is an excellent tool for organizing information and placing it in a logical order.
# In this exercise, let's rewrite a similar query using a CTE.
# league
# id	  country_id	name
# 1	    1	          Belgium Jupiler League
# 1729	1729	      England Premier League
# 4769	4769	      France Ligue 1
# 7809	7809	      Germany 1. Bundesliga
# Instructions
# - Complete the syntax to declare your CTE.
# - Select the country_id and match id from the match table in your CTE.
# - Left join the CTE to the league table using country_id.
-- Set up your CTE
WITH match_list AS (
  SELECT 
  country_id, 
  id
  FROM match
  WHERE (home_goal + away_goal) >= 10)
-- Select league and count of matches from the CTE
SELECT
l.name AS league,
COUNT(match_list.id) AS matches
FROM league AS l
-- Join the CTE to the league table
LEFT JOIN match_list ON l.id = match_list.country_id
GROUP BY l.name;
# league	                  matches
# Switzerland Super League	0
# Poland Ekstraklasa	      0
# Netherlands Eredivisie	  1
# Scotland Premier League 	0
# France Ligue 1	          0
# Spain LIGA BBVA	          4
# Nice job! You just rearranged a query to produce similar results to a subquery in FROM! Isn't 
# this much easier to read?

# Organizing with CTEs
# Previously, you modified a query based on a statement you completed in chapter 2 using common table expressions.
# This time, let's expand on the exercise by looking at details about matches with very high scores using CTEs. Just like a subquery in FROM, you can join tables inside a CTE.
# Instructions
# - Declare your CTE, where you create a list of all matches with the league name.
# - Select the league, date, home, and away goals from the CTE.
# - Filter the main query for matches with 10 or more goals.
-- Set up your CTE
WITH match_list AS (
  -- Select the league, date, home, and away goals
  SELECT 
  l.name AS league, 
  m.date, 
  m.home_goal, 
  m.away_goal,
  (m.home_goal + m.away_goal) AS total_goals
  FROM match AS m
  LEFT JOIN league as l ON m.country_id = l.id)
-- Select the league, date, home, and away goals from the CTE
SELECT league, date, home_goal, away_goal
FROM match_list
-- Filter by total goals
WHERE total_goals >= 10;
# league	                date	      home_goal	away_goal
# England Premier League	2011-08-28	8	        2
# England Premier League	2012-12-29	7	        3
# England Premier League	2013-05-19	5	        5
# Germany 1. Bundesliga	  2013-03-30	9	        2
# Netherlands Eredivisie	2011-11-06	6	        4
# Nice job! CTEs are much easier to read than multiple subqueries!

# CTEs with nested subqueries
# If you find yourself listing multiple subqueries in the FROM clause with nested statement, your query will likely become long, complex, and difficult to read.
# Since many queries are written with the intention of being saved and re-run in the future, proper organization is key to a seamless workflow. Arranging subqueries as CTEs will save you time, space, and confusion in the long run!
# Instructions
# - Declare a CTE that calculates the total goals from matches in August of the 2013/2014 season.
# - Left join the CTE onto the league table using country_id from the match_list CTE.
# - Filter the list on the inner subquery to only select matches in August of the 2013/2014 season.
-- Set up your CTE
WITH match_list AS (
  SELECT 
  country_id,
  (home_goal + away_goal) AS goals
  FROM match
  -- Create a list of match IDs to filter data in the CTE
  WHERE id IN (
    SELECT id
    FROM match
    WHERE season = '2013/2014' AND EXTRACT(MONTH FROM date) = 8))
-- Select the league name and average of goals in the CTE
SELECT 
l.name,
AVG(goals)
FROM league AS l
-- Join the CTE onto the league table
LEFT JOIN match_list ON l.id = match_list.country_id
GROUP BY l.name;
# name	                    avg
# Switzerland Super League	1.9375000000000000
# Poland Ekstraklasa	      2.3103448275862069
# Netherlands Eredivisie	  3.4146341463414634
# Scotland Premier League	  2.1379310344827586
# France Ligue 1	          2.0270270270270270
# Fantastic work! Your CTE with a nested subquery is easy to read -- and produces an interesting set of data!

# 11 Deciding on techniques to use.mp4
# Get team names with a subquery
# Let's solve a problem we've encountered a few times in this course so far -- How do you get both the home and away team names into one final query result?
# Out of the 4 techniques we just discussed, this can be performed using subqueries, correlated subqueries, and CTEs. Let's practice creating similar result sets using each of these 3 methods over the next 3 exercises, starting with subqueries in FROM.
# team
# id	team_api_id	team_long_name	  team_short_name
# 1	  9987	      KRC Genk	        GEN
# 2	  9993	      Beerschot AC	    BAC
# 3	  10000	      SV Zulte-Waregem	ZUL
# Instructions
# - Create a query that left joins team to match in order to get the identity of the home team. This becomes the subquery in the next step.
SELECT 
m.id, 
t.team_long_name AS hometeam
-- Left join team to match
FROM match AS m
LEFT JOIN team as t
ON m.hometeam_id = team_api_id;
# id	hometeam
# 757	Oud-Heverlee Leuven
# 758	RAEC Mons
# 759	KRC Genk
# 760	KAA Gent
# - Add a second subquery to the FROM statement to get the away team name, changing only the 
# hometeam_id. Left join both subqueries to the match table on the id column.
# Warning: if your code is timing out, you have probably made a mistake in the JOIN and tried to join on the wrong fields which caused the table to be too big! Read the provided code and comments carefully, and check your ON conditions!
SELECT
m.date,
-- Get the home and away team names
home.hometeam,
away.awayteam,
m.home_goal,
m.away_goal
FROM match AS m

-- Join the home subquery to the match table
INNER JOIN (
  SELECT match.id, team.team_long_name AS hometeam
  FROM match
  LEFT JOIN team
  ON match.hometeam_id = team.team_api_id) AS home
ON home.id = m.id

-- Join the away subquery to the match table
INNER JOIN (
  SELECT match.id, team.team_long_name AS awayteam
  FROM match
  LEFT JOIN team
  -- Get the away team ID in the subquery
  ON match.awayteam_id = team.team_api_id) AS away
ON away.id = m.id;
# date	      hometeam	    awayteam	              home_goal	away_goal
# 2011-07-29	Oud-Heverlee  Leuven	RSC Anderlecht	2	        1
# 2011-07-30	RAEC Mons	    Standard de Liège	      1	        1
# 2011-07-30	KRC Genk	    Beerschot AC	          3	        1
# 2011-07-30	KAA Gent	    KSV Cercle Brugge	      0	        1
# Great job!! This is one clear way to generate a list of team names who played in a match. 
# This is much easier to read than team ID numbers!

# Get team names with correlated subqueries
# Let's solve the same problem using correlated subqueries -- How do you get both the home and away team names into one final query result?
# This can easily be performed using correlated subqueries. But how might that impact the performance of your query? Complete the following steps and let's find out!
# Please note that your query will run more slowly than the previous exercise!
# Instructions
# - Using a correlated subquery in the SELECT statement, match the team_api_id column from team to the hometeam_id from match.
SELECT
m.date,
(SELECT team_long_name
 FROM team AS t
 -- Connect the team to the match table
 WHERE m.hometeam_id = t.team_api_id) AS hometeam
FROM match AS m;
# date	      hometeam
# 2011-07-29	Oud-Heverlee Leuven
# 2011-07-30	RAEC Mons
# 2011-07-30	KRC Genk
# 2011-07-30	KAA Gent
# - Create a second correlated subquery in SELECT, yielding the away team's name.
# - Select the home and away goal columns from match in the main query.
SELECT
m.date,
(SELECT team_long_name
 FROM team AS t
 WHERE t.team_api_id = m.hometeam_id) AS hometeam,
-- Connect the team to the match table
(SELECT team_long_name
  FROM team AS t
  WHERE m.awayteam_id = t.team_api_id) AS awayteam,
-- Select home and away goals
m.home_goal,
m.away_goal
FROM match AS m;
# date	      hometeam      	    awayteam	        home_goal	away_goal
# 2011-07-29	Oud-Heverlee Leuven	RSC Anderlecht	  2	        1
# 2011-07-30	RAEC Mons	          Standard de Liège	1	        1
# 2011-07-30	KRC Genk	          Beerschot AC	    3	        1
# 2011-07-30	KAA Gent	          KSV Cercle Brugge	0	        1
# Fantastic work! The syntax here is much simpler, but if you're working with a large dataset your query may take a while to run!

# Get team names with CTEs
# You've now explored two methods for answering the question, How do you get both the home and away team names into one final query result?
# Let's explore the final method - common table expressions. Common table expressions are similar to the subquery method for generating results, mainly differing in syntax and the order in which information is processed.
# Instructions
# - Select id from match and team_long_name from team. Join these two tables together on hometeam_id in match and team_api_id in team.
SELECT 
-- Select match id and team long name
m.id, 
t.team_long_name AS hometeam
FROM match AS m
-- Join team to match using team_api_id and hometeam_id
LEFT JOIN team AS t 
ON t.team_api_id = m.hometeam_id;
# id	hometeam
# 757	Oud-Heverlee Leuven
# 758	RAEC Mons
# 759	KRC Genk
# 760	KAA Gent
# - Declare the query from the previous step as a common table expression. SELECT everything from the CTE into the main query. Your results will not change at this step!
-- Declare the home CTE
WITH home AS (
  SELECT m.id, t.team_long_name AS hometeam
  FROM match AS m
  LEFT JOIN team AS t 
  ON m.hometeam_id = t.team_api_id)
-- Select everything from home
SELECT *
  FROM home;
# id	hometeam
# 757	Oud-Heverlee Leuven
# 758	RAEC Mons
# 759	KRC Genk
# 760	KAA Gent
# - Let's declare the second CTE, away. Join it to the first CTE on the id column.
# - The date, home_goal, and away_goal columns have been added to the CTEs. SELECT them into the main query.
# WITH 
home AS (
  SELECT m.id, m.date, 
  t.team_long_name AS hometeam, m.home_goal
  FROM match AS m
  LEFT JOIN team AS t 
  ON m.hometeam_id = t.team_api_id),
-- Declare and set up the away CTE
away AS (
  SELECT m.id, m.date, 
  t.team_long_name AS awayteam, m.away_goal
  FROM match AS m
  LEFT JOIN team AS t 
  ON m.awayteam_id = t.team_api_id)
-- Select date, home_goal, and away_goal
SELECT 
home.date,
home.hometeam,
away.awayteam,
home.home_goal,
away.away_goal
-- Join away and home on the id column
FROM home
INNER JOIN away
ON home.id = away.id;
# date	      hometeam	          awayteam	        home_goal	away_goal
# 2011-07-29	Oud-Heverlee Leuven	RSC Anderlecht  	2	        1
# 2011-07-30	RAEC Mons	          Standard de Liège	1	        1
# 2011-07-30	KRC Genk	          Beerschot AC	    3	        1
# 2011-07-30	KAA Gent	          KSV Cercle Brugge	0	        1
# And you're done!! You now know three separate ways to manipulate and transform data to produce a 
# complex query result! This is a great set of skills to have when working with complex relational databases!

# Which technique to use?
# The previous three exercises demonstrated that, in many cases, you can use multiple techniques in SQL to answer the same question.
# Based on what you learned, which of the following statements is false regarding differences in the use and performance of multiple/nested subqueries, correlated subqueries, and common table expressions?
# Answer the question
# Possible Answers
# - Correlated subqueries can allow you to circumvent multiple, complex joins.
# - Common table expressions are declared first, improving query run time.
# - Correlated subqueries can reduce the length of your query, which improves query run time.+
# - Multiple or nested subqueries are processed first, before your main query.
# "Correct! Although correlated subqueries can reduce the length of your query, they also significantly increase query run time."

# 
# The match is OVER
# The OVER() clause allows you to pass an aggregate function down a data set, similar to subqueries in SELECT. The OVER() clause offers significant benefits over subqueries in select -- namely, your queries will run faster, and the OVER() clause has a wide range of additional functions and clauses you can include with it that we will cover later on in this chapter.
# In this exercise, you will revise some queries from previous chapters using the OVER() clause.
# Instructions
# - Select the match ID, country name, season, home, and away goals from the match and country tables.
# - Complete the query that calculates the average number of goals scored overall and then includes the aggregate value in each row using a window function.
SELECT 
-- Select the id, country name, season, home, and away goals
m.id, 
c.name AS country, 
m.season,
m.home_goal,
m.away_goal,
-- Use a window to include the aggregate average in each row
AVG(m.home_goal + m.away_goal) OVER() AS overall_avg
FROM match AS m
LEFT JOIN country AS c ON m.country_id = c.id;
# id	country	season	  home_goal	away_goal	overall_avg
# 757	Belgium	2011/2012	2	        1	        2.7321025161642128
# 758	Belgium	2011/2012	1	        1	        2.7321025161642128
# 759	Belgium	2011/2012	3	        1	        2.7321025161642128
# Good job! The basic window function with an OVER clause functions just like a subquery in SELECT.

# What's OVER here?
# Window functions allow you to create a RANK of information according to any variable you want to use to sort your data. When setting this up, you will need to specify what column/calculation you want to use to calculate your rank. This is done by including an ORDER BY clause inside the OVER() clause. Below is an example:
# SELECT 
#   id,
#   RANK() OVER(ORDER BY home_goal) AS rank
# FROM match;
# In this exercise, you will create a data set of ranked matches according to which leagues, on average, score the most goals in a match.
# Instructions
# - Select the league name and average total goals scored from league and match.
# - Complete the window function so it calculates the rank of average goals scored across all leagues in the database.
# - Order the rank by the average total of home and away goals scored.
SELECT 
-- Select the league name and average goals scored
l.name AS league,
AVG(m.home_goal + m.away_goal) AS avg_goals,
-- Rank each league according to the average goals
RANK() OVER(ORDER BY AVG(m.home_goal + m.away_goal)) AS league_rank
FROM league AS l
LEFT JOIN match AS m 
ON l.id = m.country_id
WHERE m.season = '2011/2012'
GROUP BY l.name
-- Order the query by the rank you created
ORDER BY league_rank;
# league	                  avg_goals	          league_rank
# Poland Ekstraklasa	      2.1958333333333333	1
# France Ligue 1	          2.5157894736842105	2
# Italy Serie A	            2.5837988826815642	3
# Switzerland Super League	2.6234567901234568	4
# Scotland Premier League	  2.6359649122807018	5
# Good job! You'll notice that the default order is from smallest to largest value -- in this case, 
# the Polish league tends to have the fewest goals scored on average.

# Flip OVER your results
# In the last exercise, the rank generated in your query was organized from smallest to largest. By adding DESC to your window function, you can create a rank sorted from largest to smallest.
# SELECT 
#   id,
#   RANK() OVER(ORDER BY home_goal DESC) AS rank
# FROM match;
# Instructions
# - Complete the same parts of the query as the previous exercise.
# - Complete the window function to rank each league from highest to lowest average goals scored.
# - Order the main query by the rank you just created.
SELECT 
-- Select the league name and average goals scored
l.name AS league,
AVG(m.home_goal + m.away_goal) AS avg_goals,
-- Rank leagues in descending order by average goals
RANK() OVER(ORDER BY AVG(m.home_goal + m.away_goal) DESC) AS league_rank
FROM league AS l
LEFT JOIN match AS m 
ON l.id = m.country_id
WHERE m.season = '2011/2012'
GROUP BY l.name
-- Order the query by the rank you created
ORDER BY league_rank;
# league	                avg_goals	          league_rank
# Netherlands Eredivisie	3.2581699346405229	1
# Belgium Jupiler League	2.8791666666666667	2
# Germany 1. Bundesliga	  2.8594771241830065	3
# England Premier League	2.8052631578947368	4
# Spain LIGA BBVA	        2.7631578947368421	5
# Great job! Unlike a subquery in SELECT, your window function will apply the filter that you include in your WHERE clause.

# 12 OVER with a PARTITION.mp4
# PARTITION BY a column
# The PARTITION BY clause allows you to calculate separate "windows" based on columns you want to divide your results. For example, you can create a single column that calculates an overall average of goals scored for each season.
# In this exercise, you will be creating a data set of games played by Legia Warszawa (Warsaw League), the top ranked team in Poland, and comparing their individual game performance to the overall average for that season.
# Where do you see more outliers? Are they Legia Warszawa's home or away games?
# Instructions
# - Complete the two window functions that calculate the home and away goal averages. Partition the window functions by season to calculate separate averages for each season.
# - Filter the query to only include matches played by Legia Warszawa, id = 8673.
SELECT
  date,
  season,
  home_goal,
  away_goal,
  CASE WHEN hometeam_id = 8673 THEN 'home' 
  ELSE 'away' END AS warsaw_location,
  -- Calculate the average goals scored partitioned by season
  AVG(home_goal) OVER(PARTITION BY season) AS season_homeavg,
  AVG(away_goal) OVER(PARTITION BY season) AS season_awayavg
FROM match
-- Filter the data set for Legia Warszawa matches only
WHERE 
  hometeam_id = 8673 
  OR awayteam_id = 8673
ORDER BY (home_goal + away_goal) DESC;
# date	      season	  home_goal	away_goal	warsaw_location	season_homeavg	    season_awayavg
# 2013-09-14	2013/2014	3	        5	        away	          1.7666666666666667	1.2333333333333333
# 2014-09-13	2014/2015	4	        3	        home	          1.5666666666666667	1.3333333333333333
# 2013-07-20	2013/2014	5	        1	        home	          1.7666666666666667	1.2333333333333333
# 2013-10-20	2013/2014	4	        1	        home	          1.7666666666666667	1.2333333333333333
# 2013-06-02	2012/2013	5	        0	        home	          1.5666666666666667	1.1333333333333333
# Good job! You've generated an ordered data set with window functions to compare each game's score 
# to averages for that season.

# PARTITION BY multiple columns
# The PARTITION BY clause can be used to break out window averages by multiple data points (columns). You can even calculate the information you want to use to partition your data! For example, you can calculate average goals scored by season and by country, or by the calendar year (taken from the date column).
# In this exercise, you will calculate the average number home and away goals scored Legia Warszawa, and their opponents, partitioned by the month in each season.
# Instructions
# - Construct two window functions partitioning the average of home and away goals by season and month.
# - Filter the dataset by Legia Warszawa's team ID (8673) so that the window calculation only includes matches involving them.
SELECT 
  date,
  season,
  home_goal,
  away_goal,
  CASE WHEN hometeam_id = 8673 THEN 'home' 
  ELSE 'away' END AS warsaw_location,
  -- Calculate average goals partitioned by season and month
  AVG(home_goal) OVER(PARTITION BY season, 
                    EXTRACT(month FROM date)) AS season_mo_home,
  AVG(away_goal) OVER(PARTITION BY season, 
                    EXTRACT(month FROM date)) AS season_mo_away
FROM match
WHERE 
  hometeam_id = 8673
  OR awayteam_id = 8673
ORDER BY (home_goal + away_goal) DESC;
# date	season	home_goal	away_goal	warsaw_location	season_mo_home	season_mo_away
# 2013-09-14	2013/2014	3	5	away	2.2500000000000000	2.5000000000000000
# 2014-09-13	2014/2015	4	3	home	2.0000000000000000	2.6666666666666667
# 2013-07-20	2013/2014	5	1	home	2.5000000000000000	2.0000000000000000
# 2014-08-09	2014/2015	5	0	home	2.0000000000000000	1.00000000000000000000
# 2012-10-28	2012/2013	3	2	home	1.6666666666666667	2.0000000000000000
# Excellent! You successfully partitioned your averages by each month in the season. In the next 
# lesson, we'll expand on the usefulness of information generated with window functions.

# 13 Sliding windows.mp4
# Slide to the left
# Sliding windows allow you to create running calculations between any two points in a window using functions such as PRECEDING, FOLLOWING, and CURRENT ROW. You can calculate running counts, sums, averages, and other aggregate functions between any two points you specify in the data set.
# In this exercise, you will expand on the examples discussed in the video, calculating the running total of goals scored by the FC Utrecht when they were the home team during the 2011/2012 season. Do they score more goals at the end of the season as the home or away team?
# Instructions
# - Complete the window function by:
# - Assessing the running total of home goals scored by FC Utrecht.
# - Assessing the running average of home goals scored.
# - Ordering both the running average and running total by date.
SELECT 
  date,
  home_goal,
  away_goal,
  -- Create a running total and running average of home goals
  SUM(home_goal) OVER(ORDER BY date 
                    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(home_goal) OVER(ORDER BY date 
                    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_avg
FROM match
WHERE 
  hometeam_id = 9908 
  AND season = '2011/2012';
# date	home_goal	away_goal	running_total	running_avg
# 2011-08-14	2	2	2	2.0000000000000000
# 2011-08-27	3	1	5	2.5000000000000000
# 2011-09-18	2	2	7	2.3333333333333333
# 2011-10-01	3	0	10	2.5000000000000000
# 2011-10-22	1	4	11	2.2000000000000000
# 2011-11-06	6	4	17	2.8333333333333333
# Good job! Sliding windows allow for a wide variety of calculations with your database.
  
# Slide to the right
# Now let's see how FC Utrecht performs when they're the away team. You'll notice that the total for the season is at the bottom of the data set you queried. Depending on your results, this could be pretty long, and scrolling down is not very helpful.
# In this exercise, you will slightly modify the query from the previous exercise by sorting the data set in reverse order and calculating a backward running total from the CURRENT ROW to the end of the data set (earliest record).
# Instructions
# - Complete the window function by:
# - Assessing the running total of home goals scored by FC Utrecht.
# - Assessing the running average of home goals scored.
# - Ordering both the running average and running total by date, descending.
SELECT 
  -- Select the date, home goal, and away goals
  date,
  home_goal,
  away_goal,
  -- Create a running total and running average of home goals
  SUM(home_goal) OVER(ORDER BY date DESC
                      ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) AS running_total,
  AVG(home_goal) OVER(ORDER BY DATE DESC
                      ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) AS running_avg
FROM match
WHERE 
  awayteam_id = 9908 
  AND season = '2011/2012';
# date	home_goal	away_goal	running_total	running_avg
# 2012-05-06	1	3	25	1.4705882352941176
# 2012-04-21	0	2	24	1.5000000000000000
# 2012-04-12	3	0	24	1.6000000000000000
# 2012-03-25	3	1	21	1.5000000000000000
# 2012-03-11	1	1	18	1.3846153846153846
# 2012-02-26	1	0	17	1.4166666666666667
# Good job! You can reverse your calculations with your window functions to best suit your needs in an analysis.
  
# Setting up the home team CTE
# In this course, we've covered ways in which you can use CASE statements, subqueries, common table expressions, and window functions in your queries to structure a data set that best meets your needs. For this exercise, you will be using all of these concepts to generate a list of matches in which Manchester United was defeated during the 2014/2015 English Premier League season.
# Your first task is to create the first query that filters for matches where Manchester United played as the home team. This will become a common table expression in a later exercise.
# Instructions
# - Create a CASE statement that identifies each match as a win, lose, or tie for Manchester United.
# - Fill out the logical operators for each WHEN clause in the CASE statement (equals, greater than, less than).
# - Join the tables on home team ID from match, and team_api_id from team.
# - Filter the query to only include games from the 2014/2015 season where Manchester United was the home team.
SELECT 
  m.id, 
  t.team_long_name,
  -- Identify matches as home/away wins or ties
  CASE WHEN m.home_goal > m.away_goal THEN 'MU Win'
  WHEN m.home_goal < m.away_goal THEN 'MU Loss'
  ELSE 'Tie' END AS outcome
FROM match AS m
  -- Left join team on the home team ID and team API id
LEFT JOIN team AS t 
  ON m.hometeam_id = t.team_api_id
WHERE 
  -- Filter for 2014/2015 and Manchester United as the home team
  season = '2014/2015'
  AND t.team_long_name = 'Manchester United';
# id	team_long_name	outcome
# 4013	Manchester United	MU Loss
# 4031	Manchester United	MU Win
# 4051	Manchester United	MU Win
# 4062	Manchester United	MU Win
# 4085	Manchester United	MU Win
# Good job! This query you wrote will become the first common table expression.
  
# Setting up the away team CTE
# Great job! Now that you have a query identifying the home team in a match, you will perform a similar set of steps to identify the away team. Just like the previous step, you will join the match and team tables. Each of these two queries will be declared as a Common Table Expression in the following step.
# The primary difference in this query is that you will be joining the tables on awayteam_id, and reversing the match outcomes in the CASE statement.
# When altering CASE statement logic in your own work, you can reverse either the logical condition (i.e., home_goal > away_goal) or the outcome in THEN -- just make sure you only reverse one of the two!
# Instructions
# - Complete the CASE statement syntax.
# - Fill out the logical operators identifying each match as a win, loss, or tie for Manchester United.
# - Join the table on awayteam_id, and team_api_id.
SELECT 
  m.id, 
  t.team_long_name,
  -- Identify matches as home/away wins or ties
  CASE WHEN m.home_goal > m.away_goal THEN 'MU Loss'
    WHEN m.home_goal < m.away_goal THEN 'MU Win'
    ELSE 'Tie' END AS outcome
-- Join team table to the match table
FROM match AS m
LEFT JOIN team AS t 
  ON m.awayteam_id = t.team_api_id
WHERE 
  -- Filter for 2014/2015 and Manchester United as the away team
  m.season = '2014/2015'
  AND t.team_long_name = 'Manchester United';
# id	team_long_name	outcome
# 4026	Manchester United	MU Loss
# 4039	Manchester United	MU Win
# 4075	Manchester United	MU Win
# 4089	Manchester United	Tie
# 4117	Manchester United	Tie
# 4126	Manchester United	Tie  
# Great job! You correctly constructed a CASE statement to accurately identify matches as a win or 
# loss for Manchester United. This will become your second common table expression.

# Putting the CTEs together
# Now that you've created the two subqueries identifying the home and away team opponents, it's time to rearrange your query with the home and away subqueries as Common Table Expressions (CTEs). You'll notice that the main query includes the phrase, SELECT DISTINCT. Without identifying only DISTINCT matches, you will return a duplicate record for each game played.
# Continue building the query to extract all matches played by Manchester United in the 2014/2015 season.
# Instructions
# - Declare the home and away CTEs before your main query.
# - Join your CTEs to the match table using a LEFT JOIN.
# - Select the relevant data from the CTEs into the main query.
# - Select the date from match, team names from the CTEs, and home/ away goals from match in the main query.
-- Set up the home team CTE
WITH home AS (
    SELECT m.id, t.team_long_name,
    CASE WHEN m.home_goal > m.away_goal THEN 'MU Win'
    WHEN m.home_goal < m.away_goal THEN 'MU Loss' 
    ELSE 'Tie' END AS outcome
    FROM match AS m
    LEFT JOIN team AS t ON m.hometeam_id = t.team_api_id),
-- Set up the away team CTE
away AS (
    SELECT m.id, t.team_long_name,
    CASE WHEN m.home_goal > m.away_goal THEN 'MU Win' # here mistake in >, whould be <
    WHEN m.home_goal < m.away_goal THEN 'MU Loss' # here mistake in <, whould be >
    ELSE 'Tie' END AS outcome
    FROM match AS m
    LEFT JOIN team AS t ON m.awayteam_id = t.team_api_id)
-- Select team names, the date and goals
SELECT DISTINCT # without DISTINCT it the platform also acepted results
  date,
  home.team_long_name AS home_team,
  away.team_long_name AS away_team,
  m.home_goal,
  m.away_goal
-- Join the CTEs onto the match table
FROM match AS m
INNER JOIN home ON m.id = home.id
INNER JOIN away ON m.id = away.id
WHERE m.season = '2014/2015'
  AND (home.team_long_name = 'Manchester United' 
       OR away.team_long_name = 'Manchester United');
# Fantastic! You wrote a query that extracted data from two common table expressions. You can scale 
# up the number of CTEs as necessary, depending on your data.
  
# Add a window function
# Fantastic! You now have a result set that retrieves the match date, home team, away team, and the goals scored by each team. You have one final component of the question left -- how badly did Manchester United lose in each match?
# In order to determine this, let's add a window function to the main query that ranks matches by the absolute value of the difference between home_goal and away_goal. This allows us to directly compare the difference in scores without having to consider whether Manchester United played as the home or away team!
# The equation is complete for you -- all you need to do is properly complete the window function!
# Instructions
# - Set up the CTEs so that the home and away teams each have a name, ID, and score associated with them.
# - Select the date, home team name, away team name, home goal, and away goals scored in the main query.
# - Rank the matches and order by the difference in scores in descending order.
-- Set up the home team CTE
WITH home AS (
    SELECT m.id, t.team_long_name,
    CASE WHEN m.home_goal > m.away_goal THEN 'MU Win'
    WHEN m.home_goal < m.away_goal THEN 'MU Loss' 
    ELSE 'Tie' END AS outcome
    FROM match AS m
    LEFT JOIN team AS t ON m.hometeam_id = t.team_api_id),
-- Set up the away team CTE
away AS (
    SELECT m.id, t.team_long_name,
    CASE WHEN m.home_goal > m.away_goal THEN 'MU Loss'
    WHEN m.home_goal < m.away_goal THEN 'MU Win' 
    ELSE 'Tie' END AS outcome
    FROM match AS m
    LEFT JOIN team AS t ON m.awayteam_id = t.team_api_id)
-- Select columns and and rank the matches by goal difference
SELECT DISTINCT
  date,
  home.team_long_name AS home_team,
  away.team_long_name AS away_team,
  m.home_goal, m.away_goal,
  RANK() OVER(ORDER BY ABS(home_goal - away_goal) DESC) as match_rank
-- Join the CTEs onto the match table
FROM match AS m
INNER JOIN home ON m.id = home.id
INNER JOIN away ON m.id = away.id
WHERE m.season = '2014/2015'
  AND ((home.team_long_name = 'Manchester United' AND home.outcome = 'MU Loss')
       OR (away.team_long_name = 'Manchester United' AND away.outcome = 'MU Loss'));
# date	home_team	away_team	home_goal	away_goal	match_rank
# 2014-08-16	Manchester United	Swansea City	1	2	3
# 2014-09-21	Leicester City	Manchester United	5	3	2
# 2014-11-02	Manchester City	Manchester United	1	0	3
# 2015-01-11	Manchester United	Southampton	0	1	3
# 2015-02-21	Swansea City	Manchester United	2	1	3
# 2015-04-18	Chelsea	Manchester United	1	0	3
# 2015-04-26	Everton	Manchester United	3	0	1
# 2015-05-02	Manchester United	West Bromwich Albion	0	1	3
# Awesome! This is an interesting list of matches. You can easily modify this query for any team 
# of your choice. Finally, if you liked this course, let your instructor know by tweeting them at @monakay!
  
  
  
  
  
  
  
  




































































































































































































































































































  
  











