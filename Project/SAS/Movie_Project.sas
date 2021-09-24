
data movies;
infile "/home/u59211922/sasuser.v94/movies.csv" dlm="," firstobs=2 dsd;
input name$ rating$ genre$ year$ releaseddate$ score votes director$ writer$ star$
country$ budget gross company$ runtime;
run;
proc print data=movies;
run; 


/* To check is there any missing values present in table*/
proc means data=movies nmiss;
run;

/* from the above, score and votes have very few null values, so lets delete those rows  */
data movies;
  set movies;
  if score = . then delete;
  if votes =  . then delete;
run;


/* To check the summary  of the data*/
proc summary data=movies print n mean median  mode stddev min max;
var score votes budget gross runtime ;
run;

/* To check the correlation between columns */
proc corr data=movies;
run;
/* Result : 
   The movie score  mainly correlates with the values of votes and duration of movie*/



/* Histogram */
title"Histogram Duration vs Score";
proc sgplot  data=movies;
histogram runtime/group=score transparency=0.5 fillattrs=(color=olive);
density runtime /type=normal group=score;
keylegend /location=inside position=topright across=1;
run;

/* Histogram */
title"Histogram Votes vs Score";
proc sgplot  data=movies;
histogram votes/group=score transparency=0.5;
density votes /type=normal group=score;
keylegend /location=inside position=topright across=1;
run;


/* To find information of data */
proc contents data=movies ;
run;

/* use SQL to query data */
libname sql 'SAS-library';

/* To create Table with Diabtes patients only*/
proc sql;
create table MOVIEDATA as 
select name, rating, genre, year, releaseddate, score, votes, director, writer, star,
country, budget, gross, company, runtime from movies;
quit;
proc print data=MOVIEDATA;
run;

/* get top 10 directors with highest score who has at lest 5 movies*/
proc sql outobs=10;
   title 'Top 10 directors with highest score';
   select director, avg(score) as score
      from MOVIEDATA
      group by director
      order by 2 desc;



/* SCATTER PLOT OF Score VS Votes */

proc sgplot data=movies;
scatter x=votes y= score;
title 'Score vs Votes';
run;

/* SCATTER PLOT OF Score VS Duration */
proc sgplot data=movies;
scatter x=runtime y= score;
title 'Score vs Duration';
run;

/* SCATTER PLOT OF Score VS Gross */
proc sgplot data=movies;
scatter x=gross y= score;
title 'Score vs Gross';
run;

/* BARGRAPH OF SCORE VS Duration */

proc  sgplot data= movies;
title "score vs Duration";
vbar score/response=runtime group=score stat=percent datalabel;
xaxis display=(nolabel);
yaxis grid label='percentage';
run;





     









