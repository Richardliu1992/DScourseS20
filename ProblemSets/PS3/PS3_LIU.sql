.mode csv
.import /home/ouecon030/DScourseS20/ProblemSets/PS3/FL_insurance_sample.csv datname

SELECT * FROM datname LIMIT 10;

SELECT DISTINCT county FROM datname;

SELECT AVG(tiv_2012 - tiv_2011) FROM datname;

SELECT construction, COUNT(*) FROM datname GROUP BY construction;
