rmarkdown::render("RProject1HollidayG.Rmd", 
                  output_format = "github_document",
                  output_file="README.md",
                  output_options = list(
                    number_sections=FALSE,
                    df_print="tibble"
                  )
)
