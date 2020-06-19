DROP TABLE IF EXISTS bibliometrics;
CREATE TABLE bibliometrics(
    handle varchar(500) NOT NULL PRIMARY KEY,
    year int DEFAULT NULL,
    cites int DEFAULT 0,
    if_ numeric DEFAULT 0,
    if_5 numeric DEFAULT 0,
    documents_published int DEFAULT 0,
    h_index int DEFAULT 0,
    type varchar(500) DEFAULT '',
    issn1 varchar(500) DEFAULT '',
    issn2 varchar(500) DEFAULT '',
    type2 varchar(500) DEFAULT '',
    year3 int DEFAULT 0,
    scimago_id bigint DEFAULT 0,
    sjr numeric DEFAULT 0,
    type4 varchar(500) DEFAULT '',
    year5 int DEFAULT 0,
    journ_qual varchar(2) DEFAULT '',
    type6 varchar(500) DEFAULT '',
    year7 int DEFAULT 0,
    bwl varchar(2) DEFAULT '',
    type8 varchar(500) DEFAULT '',
    year9 int DEFAULT 0,
    vwl varchar(2) DEFAULT '',
    journal_name varchar(500) DEFAULT ''
);

DROP TABLE IF EXISTS altmetrics;
CREATE TABLE altmetrics (
  id serial PRIMARY KEY,
  altmetric_score int DEFAULT 0,
  title varchar(500) DEFAULT '',
  journal_name varchar(500) DEFAULT '',
  print_issn varchar(500) DEFAULT '',
  online_issn varchar(500) DEFAULT '',
  subjects varchar(500) DEFAULT '',
  affiliation varchar(500) DEFAULT '',
  publication_date varchar(100) DEFAULT '',
  doi varchar(500) DEFAULT '',
  news int DEFAULT 0,
  blog int DEFAULT 0,
  policy int DEFAULT 0,
  patent int DEFAULT 0,
  twitter int DEFAULT 0,
  peer_review int DEFAULT 0,
  weibo int DEFAULT 0,
  facebook int DEFAULT NULL,
  wikipedia int DEFAULT 0,
  google int DEFAULT 0,
  linkedIn int DEFAULT 0,
  reddit int DEFAULT 0,
  pinterest int DEFAULT 0,
  f1000 int DEFAULT 0,
  qa int DEFAULT 0,
  videos int DEFAULT 0,
  syllabi int DEFAULT 0,
  mendeley int DEFAULT 0,
  dimensions_citations int DEFAULT 0,
  details_page varchar(500) DEFAULT '',
  badge_url varchar(500) DEFAULT ''
);


DROP TABLE IF EXISTS mendeley_doi;
CREATE TABLE mendeley_doi (
  id serial PRIMARY KEY,
  doi varchar(50) NOT NULL DEFAULT '',
  count int NOT NULL DEFAULT '0',
  year varchar(50) NOT NULL DEFAULT '',
  issn varchar(500) DEFAULT '',
  title varchar(1000) DEFAULT '',
  publisher varchar(200) DEFAULT '',
  discipline varchar(350) DEFAULT ''
);

DROP TABLE IF EXISTS mendeley_doi_simp;
CREATE TABLE mendeley_doi_simp (
  issn varchar(50) PRIMARY KEY,
  instances int NOT NULL DEFAULT 0,
  id varchar(50),
  count int NOT NULL DEFAULT 0
);

DROP TABLE IF EXISTS mendeley_country;
CREATE TABLE mendeley_country (
  id serial PRIMARY KEY,
  id_doi int,
  country varchar(50),
  count int NOT NULL DEFAULT '0',
  code varchar(4)
);

DROP TABLE IF EXISTS mendeley_status;
CREATE TABLE mendeley_status (
  id serial PRIMARY KEY,
  id_doi int,
  status varchar(50),
  count int NOT NULL DEFAULT '0'
);

DROP TABLE IF EXISTS mendeley_discipline;
CREATE TABLE mendeley_discipline (
  id serial PRIMARY KEY,
  id_doi int,
  category varchar(150) NOT NULL DEFAULT '',
  discipline varchar(50) NOT NULL DEFAULT '',
  count int NOT NULL DEFAULT '0'
);

DROP TABLE IF EXISTS alt_simp;
CREATE TABLE alt_simp (
  print_issn varchar(50) PRIMARY KEY,
  journal_name varchar(50) DEFAULT '',
  instances int NOT NULL DEFAULT '0',
  id int NOT NULL DEFAULT '0',
  altmetric_score int NOT NULL DEFAULT '0',
  news int NOT NULL DEFAULT '0',
  blog int NOT NULL DEFAULT '0',
  policy int NOT NULL DEFAULT '0',
  patent int NOT NULL DEFAULT '0',
  twitter int NOT NULL DEFAULT '0',
  peer_review int NOT NULL DEFAULT '0',
  weibo int NOT NULL DEFAULT '0',
  facebook int NOT NULL DEFAULT '0',
  wikipedia int NOT NULL DEFAULT '0',
  google int NOT NULL DEFAULT '0',
  linkedIn int NOT NULL DEFAULT '0',
  reddit int NOT NULL DEFAULT '0',
  pinterest int NOT NULL DEFAULT '0',
  f1000 int NOT NULL DEFAULT '0',
  qa int NOT NULL DEFAULT '0',
  videos int NOT NULL DEFAULT '0',
  syllabi int NOT NULL DEFAULT '0',
  mendeley int NOT NULL DEFAULT '0',
  dimensions_citations int NOT NULL DEFAULT '0'
);

--
-- Load Data in Tables
--
COPY bibliometrics (handle, year, cites, if_, if_5, documents_published, h_index, type, issn1, issn2, type2, year3, scimago_id, sjr, type4, year5, journ_qual, type6, year7, bwl, type8, year9, vwl, journal_name)
from '/srv/conda/data/biblio_data.csv' (FORMAT CSV, DELIMITER(';'), HEADER)

\COPY mendeley_doi from '/srv/conda/data/mendeley_doi.csv' (FORMAT CSV, DELIMITER(';'), HEADER, QUOTE('"'))
\COPY altmetrics from '/srv/conda/data/cleaned_altmetrics.csv' (FORMAT CSV, DELIMITER(';'), HEADER, QUOTE('"'))
\COPY mendeley_doi_simp from '/srv/conda/data/simplified_mend_doi.csv' (FORMAT CSV, DELIMITER(';'), HEADER, QUOTE('"'))
\COPY mendeley_country from '/srv/conda/data/mendeley_country.csv' (FORMAT CSV, DELIMITER(';'), HEADER, QUOTE('"'))
\COPY mendeley_status from '/srv/conda/data/mendeley_status.csv' (FORMAT CSV, DELIMITER(';'), HEADER, QUOTE('"'))
\COPY mendeley_discipline from '/srv/conda/data/mendeley_discipline.csv' (FORMAT CSV, DELIMITER(';'), HEADER, QUOTE('"'))

\COPY alt_simp from '/srv/conda/data/simplified_alt.csv' (FORMAT CSV, DELIMITER(';'), HEADER, QUOTE('"'))
