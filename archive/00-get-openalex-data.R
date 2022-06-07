library(tidyverse)
library(httr)

# query for all of medicine
base_query <- "https://api.openalex.org/works?filter=concepts.id:C71924100"

new <- modify_url(base_query, query = list("cursor" = "*"))
new


res <- GET(base_query)
res$results
content(res)
res

save_results <- function(get_result, file) {
  only_text <- content(res, as = "text", encoding = "utf-8")
  cleaned <- stringr::str_extract(only_text, '(?<="results":).*(?=,"group_by)')
  write_lines(cleaned, file, append = TRUE)
}

save_results(res, "data/test.jsonl")


the_rex <- '(?<="results":).*(?=,"group_by)'



# benchmark
bench::mark(stringr::str_extract(only_text, the_rex))

write_lines(content(res, as = "text", encoding = "utf-8"),
            file = "data/test.jsonl")

# get it back
roundabout <- jsonlite::stream_in(file("data/test.jsonl"))
roundabout$results

# this is fine. still should write the cursor to file too
base_query <- "https://api.openalex.org/works?filter=concepts.id:C71924100"

new <- modify_url(base_query, query = list("cursor" = "*"))
new




res <- GET(new)
res$results
out <- content(res, as = "text", encoding = "utf-8")
out


res

