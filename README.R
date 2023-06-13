rmarkdown::render("RProject1HollidayG.Rmd", 
                  output_format = "github_document",
                  output_file="README.md",
                  output_options = list(
                    toc=TRUE,
                    toc_depth=2,
                    number_sections=TRUE,
                    df_print="tibble"
                  )
)
