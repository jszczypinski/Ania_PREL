# R/functions.R
get_data <- function(file, language) {
  # read data
  data <- as.data.table(read_xlsx(file), sheet = "behavior_raw")
  # add column with id 
  data$id <- 1:nrow(data)
  # melt data to long format
  data <- melt(data,
               measure.vars = c("L1_SEM_CAT",
                                "L2_SEM_CAT",
                                "L1_PHONO",
                                "L2_PHONO"),
               variable.name = "condition",
               value.name = "accuracy")
  # create new columns with factors for lang and task separately 
  data$lang <- fifelse(grepl("L1", data$condition), "polish", "PJM")
  data$task <- fifelse(grepl("PHONO", data$condition), "PHONO", "SEM_CAT")
  # subset data of given language
  data <- data[lang == language]
  # remove column "condition" since we won't use it
  data <- data[, !("condition")]
  # change "grupa" to "group" to have a more coherent naming 
  setnames(data, "grupa", "group")
  data
}

# function for fitting poisson model
fit_poisson_model <- function (data) {
  formula <- accuracy ~ group * task + (1|id)
  model <- glmer(formula, data = data, family = poisson(link = "log"), nAGQ = 100)
  model
}

# function for plotting model diagnostics using DHARMa package
plot_model_diagnostics <- function (model) {
  simulationOutput <- simulateResiduals(fittedModel = model, plot = T)
}

# function for estimating p-values using bootstrap
# this step takes a lot of time and needs 
get_pvalues <- function(model, .data, .nsim = 10) {
  set.seed(2137)
  model_pvals <- mixed(model,
                       method = "PB",
                       family = poisson(link = "log"),
                       args_test = list(nsim = .nsim, details = 2),
                       data = .data,
                       progress = FALSE)
  model_pvals
}

make_table <- function(pvals, language) {
  # check for dir and create it if needed
  if (!dir.exists("./tables")) {
    dir.create("./tables")
  } 
  # set path for saving table to word file
  path <- paste0("./tables/Table_",
                 deparse(quote(pvals)),
                 "_",
                 language,
                 ".docx")
  # set defaults for table formatting
  set_flextable_defaults(
    font.family = "Times New Roman",
    font.size = 10,
    theme_fun = "theme_apa"
  )
  # new coulmn names
  new_names <- c("Effect", 
                 "Chi_square", 
                 "DF", "p value", 
                 "Bootstrapped\np value")
  # table caption
  caption <- paste0("Poisson analysis of variance for the number of correct responses 
                    in semantic and phonological task for ",
                    language, ".")
  # get table from pvals object
  table <- copy(pvals[["anova_table"]])
  # set rownames as column; rownames are effects names 
  table <- setDT(table, keep.rownames = TRUE)[] 
  # set new column names for the table
  setnames(table, colnames(table), new_names)
  # make a flextable
  ft <- flextable(table) |>
    # change allignment of the first column
    align(j = 1, align = "left", part = "body") |>
    align(j = 1, align = "left", part = "header") |>
    # change chi square to a symbol
    compose(part = "header",
            j = 2,
            value = as_paragraph("\U1D712",
                                 as_sup("2"))) |>
    # format pvalues decimals 
    colformat_double(j = 4:5, digits = 3) |>
    # format DFs decimals 
    colformat_double(j = 3, digits = 0) |>
    # set caption
    set_caption(as_paragraph(as_chunk(caption, props = fp_text_default(italic = TRUE)))) |>
    # fit the cell content
    autofit() |>
    # save to docx
    save_as_docx(path = path)
}

post_hocs <- function(model) {
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 317cf63 (Adding functions for estimating and plotting post-hoc tables)
=======
>>>>>>> test
=======
>>>>>>> test
  #create data.frames from post hocs objects 
  con1 <- as.data.frame(emmeans(model, pairwise~task|group, type = "response")$contrasts)
  con2 <- as.data.frame(emmeans(model, pairwise~group|task, type = "response")$contrasts)
  # rename colnames to be able to bind two post hocs data.frames
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
=======
>>>>>>> test
=======
  con1 <- as.data.frame(emmeans(model, pairwise~task|group, type = "response")$contrasts)
  con2 <- as.data.frame(emmeans(model, pairwise~group|task, type = "response")$contrasts)
>>>>>>> 1b76e3215947bab3681d19f9d5f7ebb212370597
<<<<<<< HEAD
>>>>>>> 317cf63 (Adding functions for estimating and plotting post-hoc tables)
=======
>>>>>>> test
=======
  con1 <- as.data.frame(emmeans(model, pairwise~task|group, type = "response")$contrasts)
  con2 <- as.data.frame(emmeans(model, pairwise~group|task, type = "response")$contrasts)
>>>>>>> test
  con1$contrast <-  with(con1, paste0(contrast, " on ", group))
  con1$group <- NULL
  con2$contrast <-  with(con2, paste0(contrast, " on ", task))
  con2$task <- NULL
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 317cf63 (Adding functions for estimating and plotting post-hoc tables)
=======
>>>>>>> test
=======
>>>>>>> test
  # binding
  con <- rbind(con1,con2)
  # transform to a data.table
  con <- as.data.table(con)
  # remove unwanted columns
  con <- con[, !c("df", "null")]
  # add column with adjusted P.values
  con[, p.value.adj := p.adjust(p.value, method = "holm")]
  # function for formatting p.values
  format_decimals <- function(value) {
    value <- fifelse(value < 0.001, "< 0.001", as.character(round(value, 3)))
  }
  # format p.values
  con[, c("p.value", "p.value.adj")] <- con[, lapply(.SD, format_decimals), .SDcols = c("p.value", "p.value.adj")] 
  # new names for columns
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
=======
>>>>>>> test
=======
=======
>>>>>>> test
  con <- rbind(con1,con2)
  con <- as.data.table(con)
  con <- con[, !c("df", "null")]
  con[, p.value.adj := p.adjust(p.value, method = "holm")]
<<<<<<< HEAD
>>>>>>> 1b76e3215947bab3681d19f9d5f7ebb212370597
<<<<<<< HEAD
>>>>>>> 317cf63 (Adding functions for estimating and plotting post-hoc tables)
=======
>>>>>>> test
=======
>>>>>>> test
  new_names <- c("Contrast", 
                 "Ratio", 
                 "SE", 
                 "Z ratio",
                 "p value",
                 "Adjusted\np value")
<<<<<<< HEAD
<<<<<<< HEAD
  # set new names
  setnames(con, colnames(con), new_names)
=======
<<<<<<< HEAD
<<<<<<< HEAD
  # set new names
  setnames(con, colnames(con), new_names)
=======
>>>>>>> 1b76e3215947bab3681d19f9d5f7ebb212370597
>>>>>>> 317cf63 (Adding functions for estimating and plotting post-hoc tables)
=======
>>>>>>> 1b76e3215947bab3681d19f9d5f7ebb212370597
>>>>>>> test
=======
  # set new names
  setnames(con, colnames(con), new_names)
>>>>>>> test
  con
}

make_post_hoc_table <- function(con, language) {
  
# check for dir and create it if needed
if (!dir.exists("./tables")) {
  dir.create("./tables")
} 
caption <- paste0("Post-hoc tests for Poisson model for ",
                    language, ".")  
<<<<<<< HEAD
<<<<<<< HEAD
footer <- ("Post-hoc tests adjusted with the use of the Holm's method.
=======
<<<<<<< HEAD
<<<<<<< HEAD
footer <- ("Post-hoc tests adjusted with the use of the Holm's method.
=======
footer <- ("Post-hoc tests adjusted with the use of the Holm's method.\n
>>>>>>> 1b76e3215947bab3681d19f9d5f7ebb212370597
>>>>>>> 317cf63 (Adding functions for estimating and plotting post-hoc tables)
=======
footer <- ("Post-hoc tests adjusted with the use of the Holm's method.\n
>>>>>>> 1b76e3215947bab3681d19f9d5f7ebb212370597
>>>>>>> test
=======
footer <- ("Post-hoc tests adjusted with the use of the Holm's method.
>>>>>>> test
           Tests were performed on the log scale.")
# set path for saving table to word file
path <- paste0("./tables/Table_post_hocs",
               "_",
               language,
               ".docx")
<<<<<<< HEAD
<<<<<<< HEAD

=======
<<<<<<< HEAD
<<<<<<< HEAD

=======
>>>>>>> 1b76e3215947bab3681d19f9d5f7ebb212370597
>>>>>>> 317cf63 (Adding functions for estimating and plotting post-hoc tables)
=======
>>>>>>> 1b76e3215947bab3681d19f9d5f7ebb212370597
>>>>>>> test
=======

>>>>>>> test
# set defaults for table formatting
set_flextable_defaults(
  font.family = "Times New Roman",
  font.size = 10,
  theme_fun = "theme_apa"
)

ft <- flextable(con) |>
  # change allignment of the first column
  align(j = 1, align = "left", part = "body") |>
  align(j = 1, align = "left", part = "header") |>
<<<<<<< HEAD
<<<<<<< HEAD
=======
<<<<<<< HEAD
<<<<<<< HEAD
=======
  # format pvalues decimals 
  colformat_double(j = 5:6, digits = 3) |>
>>>>>>> 1b76e3215947bab3681d19f9d5f7ebb212370597
>>>>>>> 317cf63 (Adding functions for estimating and plotting post-hoc tables)
=======
  # format pvalues decimals 
  colformat_double(j = 5:6, digits = 3) |>
>>>>>>> 1b76e3215947bab3681d19f9d5f7ebb212370597
>>>>>>> test
=======
  # format pvalues decimals 
  colformat_double(j = 5:6, digits = 3) |>
>>>>>>> test
  # set caption
  set_caption(as_paragraph(as_chunk(caption, 
                                    props = fp_text_default(italic = TRUE)))) |>
  # set footer
  add_footer_row(as_paragraph(as_chunk(footer, 
                                       props = fp_text_default(italic = TRUE))), 
                 colwidths = c(6), 
                 top = FALSE) |>
  # fit the cell content
  autofit() |>
  # save to docx
  save_as_docx(path = path)
}
