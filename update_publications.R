# ============================================================================
# update_publications.R
# Detect new publications from OpenAlex (keyed to ORCID) and prepare rows
# ready to paste into data/cv.xlsx. Re-run anytime to fetch only new entries.
#
# OpenAlex is an open bibliographic database (aggregates Crossref and other
# sources). No API key required, no bot-blocking — unlike Google Scholar.
# ============================================================================

# Install once if needed:
# install.packages(c("openalexR", "readxl", "dplyr", "stringr", "writexl", "here"))

library(openalexR)
library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(here)

# --- Settings ---------------------------------------------------------------
orcid <- "0000-0002-7881-0578"

# --- 1. Fetch all your works from OpenAlex ----------------------------------
works <- oa_fetch(entity = "works", author.orcid = orcid, verbose = TRUE)

# Run this ONCE to confirm the column names in your openalexR version,
# then comment it out again:
# dplyr::glimpse(works)

# --- 2. DOIs already present in cv.xlsx (normalised for comparison) ----------
clean_doi <- function(x) {
  x |>
    tolower() |>
    str_remove("https?://(dx\\.)?doi\\.org/") |>
    str_trim()
}

existing <- read_excel(here("data", "cv.xlsx"), sheet = "pubs") |>
  pull(doi) |>
  clean_doi()

# --- 3. Keep only genuine journal articles not already in cv.xlsx ------------
new <- works |>
  filter(type %in% c("article", "review"),   # drop datasets, preprints, paratext
         !is.na(issn_l),                      # must be in a journal (drops Zenodo/figshare/SSRN/RG)
         !is_paratext,                        # drop cover blurbs / supplements
         publication_year >= 2012) |>         # drop the 1895 homonym
  mutate(doi_bare = clean_doi(doi)) |>
  filter(!doi_bare %in% existing) |>
  arrange(desc(publication_year))

# --- 4. Authors as a single string (column = 'authorships', field = 'display_name') ---
authors_str <- vapply(
  new$authorships,
  function(a) if (is.data.frame(a)) paste(a$display_name, collapse = ", ") else NA_character_,
  character(1)
)

# Sanity check: both numbers must be equal
length(authors_str); nrow(new)

# --- 5. Shape to your cv.xlsx columns ---------------------------------------

# Build a "volume, pages" string matching your cv.xlsx convention
# (e.g. "8, 1755-1772", or just the volume / article number when no page range)
make_number <- function(vol, iss, fp, lp) {
  pages <- ifelse(!is.na(fp) & !is.na(lp) & fp != lp, paste0(fp, "-", lp), fp)
  out <- vol
  out <- ifelse(!is.na(out) & !is.na(pages), paste0(out, ", ", pages),
                ifelse(is.na(out), pages, out))   # handle missing volume gracefully
  out
}

# Build a PDF filename: FirstAuthor_et_al_YEAR_Journal.pdf
make_pdf_name <- function(authorships, year, journal) {
  first <- vapply(authorships, function(a) {
    if (is.data.frame(a) && nrow(a) > 0) tail(strsplit(a$display_name[1], " ")[[1]], 1)
    else NA_character_
  }, character(1))
  j <- gsub("[^A-Za-z0-9]+", "_", journal)          # journal -> safe filename
  paste0(first, "_et_al_", year, "_", j, ".pdf")
}

out <- tibble(
  category = "peer_reviewed",                  # adjust per row if needed
  summary  = FALSE,
  image    = FALSE,
  pub_date = new$publication_date,
  id_scholar = NA_character_,                  # cannot be fetched (Scholar blocks bots)
  year     = new$publication_year,
  author   = authors_str,                      # already abbreviated + your name in bold
  title    = new$display_name,
  journal  = new$source_display_name,
  number   = make_number(new$volume, new$issue, new$first_page, new$last_page),
  doi      = new$doi_bare,
  url_pub  = new$landing_page_url,             # publisher page (fallback: paste0("https://doi.org/", new$doi_bare))
  url_pdf  = new$pdf_url,
  pdf_name = make_pdf_name(new$authorships, new$publication_year, new$source_display_name),
  abstract = new$abstract
)

# --- 6. Review + export ------------------------------------------------------
print(out, n = 50)
write_xlsx(out, here("data", "new_pubs_to_add.xlsx"))
message(nrow(out), " new publication(s) found. Saved to data/new_pubs_to_add.xlsx")

