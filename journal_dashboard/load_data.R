source("helpers.R")

db <- start()

alt <- query(db, "select * from altmetrics")
jd <- query(db, "select * from bibliometrics")
mend_geo <- query(db, "select * from mendeley_country")
mend_status <- query(db, "select * from mendeley_status")
mend_doi <- query(db, "select * from mendeley_doi")
mend_dis <- query(db, "select * from mendeley_discipline")
alt_simp <- query(db, "select * from alt_simp")

# Remove 0s and NAs
jd <- jd %>% replace(.=="null", 0)
jd[jd == ''] <- 0
jd[is.na(jd)] <- 0

# https://stackoverflow.com/questions/1563961/how-to-find-top-n-of-records-in-a-column-of-a-dataframe-using-r
n <- 10
top_10_per_cites_cutoff <- quantile(jd$cites, prob=1-n/100)
top_10_per_if_cutoff <- quantile(jd$if_, prob=1-n/100)
top_10_per_if_5_cutoff <- quantile(jd$if_5, prob=1-n/100)
top_10_per_hindex_cutoff <- quantile(jd$h_index, prob=1-n/100)
top_10_per_publications_cutoff <- quantile(jd$docs_published, prob=1-n/100)
top_10_per_sjr_cutoff <- quantile(jd$sjr, prob=1-n/100)


mend_map_comp = "SELECT doi.id, geo.count, geo.country, geo.code, doi.publisher
                 FROM mendeley_doi as doi
                 JOIN mendeley_country as geo
                     ON geo.id_doi = doi.id"

mend_map_comp <- query(db, mend_map_comp)

spider_data = "Select bib.journal_name, bib.issn1, bib.if_, bib.sjr, bib.cites, alt.altmetric_score, alt.mendeley
         FROM bibliometrics as bib
         INNER JOIN alt_simp as alt
            ON bib.issn1 = alt.print_issn"

spider_data <- query(db, spider_data)

treemap_data = "SELECT alt.journal_name, status.status, sum(status.count) as total
         FROM mendeley_status as status
         JOIN mendeley_doi as doi
            ON status.id_doi = doi.id
         JOIN alt_simp as alt
            ON alt.print_issn = doi.issn
        GROUP BY alt.journal_name, status.status"

treemap_data <- query(db, treemap_data)

journal_comp = "SELECT *
      FROM alt_simp as alt
      JOIN bibliometrics as bib
        ON alt.print_issn = bib.issn1"


journal_comp <- query(db, journal_comp)

stop(db)
