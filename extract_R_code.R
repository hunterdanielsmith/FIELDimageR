r <- readLines("README.md")

cat('---
title: "Untitled"
output: html_document
---
', file='foo.Rmd')  ## basic YAML header, may be customized
for (i in seq(r)) {cat(r[i], '\n', file='foo.Rmd', append=TRUE)}

lines = readLines('foo.Rmd')
new_lines = gsub("```r", "```{r}",lines)
writeLines(new_lines, 'foo.Rmd')

knitr::purl(input = "foo.Rmd", output = "tut_code.R",documentation = 0)
