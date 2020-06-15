#############################################
# Prepare the data to be used in the charts #
#############################################
# Altmetrics
alt <- read.csv('../data/cleaned_altmetrics.csv', header=TRUE, sep=';', stringsAsFactors = FALSE)
alt_simp <- read.csv('../data/simplified_alt.csv', header=TRUE, sep=';', stringsAsFactors = FALSE)

# Spider Chart
spider_chart_data <- read.csv2('../data/csv_tableau/spider_chart.csv', header=TRUE, sep=';',  stringsAsFactors = FALSE)

# Mendeley
mend_geo <- read.csv('../data/mendeley_country.csv', header=TRUE, sep=';', stringsAsFactors = FALSE)
mend_status <- read.csv('../data/mendeley_status.csv', header=TRUE, sep=';', stringsAsFactors = FALSE)
mend_doi <- read.csv('../data/mendeley_doi.csv', header=TRUE, sep=';', stringsAsFactors = FALSE)
mend_disc <- read.csv('../data/mendeley_discipline.csv', header=TRUE, sep=';', stringsAsFactors = FALSE)

# Bibliometrics
jd <- read.csv('../data/biblio_data.csv', header=FALSE, sep=';', stringsAsFactors = FALSE)
colnames(jd) <- c('handle', 'year', 'cites', 'if_', 'if_5', 'docs_published', 'h_index', 'type', 'issn1', 'issn2', 'type2', 'year3',
                  'scimago_id', 'sjr', 'type4', 'year5', 'jourqual', 'type6', 'year7', 'bwl', 'type8', 'year9', 'vwl', 'journal_name')
jd <- jd %>% replace(.=="null", 0)
jd[jd == ''] <- 0 # Set empty values to 0
jd[is.na(jd)] <- 0 # Set NA values to 0


# https://stackoverflow.com/questions/1563961/how-to-find-top-n-of-records-in-a-column-of-a-dataframe-using-r
n <- 10
top_10_per_cites_cutoff <- quantile(jd$cites, prob=1-n/100)
top_10_per_if_cutoff <- quantile(jd$if_, prob=1-n/100)
top_10_per_if_5_cutoff <- quantile(jd$if_5, prob=1-n/100)
top_10_per_hindex_cutoff <- quantile(jd$h_index, prob=1-n/100)
top_10_per_publications_cutoff <- quantile(jd$docs_published, prob=1-n/100)
top_10_per_sjr_cutoff <- quantile(jd$sjr, prob=1-n/100)
