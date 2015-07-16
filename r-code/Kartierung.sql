CREATE TABLE "hyal_survey" (
id INT NOT NULL,
done CHAR(1),
detailed CHAR(1),
PRIMARY KEY (id));

\copy "hyal_survey" FROM /home/pillo/Dropbox/Kartierung/Kartierung.csv

SELECT AddGeometryColumn ( 'hyal_survey', 'the_geom', 31467, 'POINT', 2 );
UPDATE hyal_survey as a SET the_geom = (SELECT the_geom FROM hyal_pres as b WHERE a.id=b.id);


UPDATE hyal_survey SET done =1 WHERE id = 302;







CREATE TABLE "test" (
id INT NOT NULL,
hyal_pres INT);

\copy "test" FROM /home/pillo/Dropbox/Doktorarbeit/Kartierung/Kartierung_Ho_occurrences.csv WITH CSV HEADER DELIMITER AS E'\t' NULL AS '' 

/*
\copy "test" FROM /home/pillo/Dropbox/Doktorarbeit/Kartierung/Kartierung_Ho_occurrences.csv WITH CSV HEADER DELIMITER AS ',' NULL AS '' 

- E begins an escape sequence. Think of E much like you would a double quoted string in C. E'\t' == "\t"
- to ignore header: Use HEADER option with CSV option
*/


ALTER TABLE hyal_pres ADD COLUMN "hyal_pres" INT;
UPDATE hyal_pres as a SET hyal_pres = (SELECT hyal_pres FROM test as b WHERE a.id=b.id);







