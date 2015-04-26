# import files ----------------------------------------------- start

# -- walmart
#read_files <- function() {
if(0) {
  print("Importing walmart data")
  walmart <- read_csv("../QPackage/inst//extdata/books/walmart.csv", status = read_status)
  print(read_status)
  
  # check id
  status <- check_id(walmart, list("id"))
  print(status)
  
  
  # set id
  status <- set_id(walmart, list("id"))
  print(status)
  
  
  # -- bowker
  print("Importing bowker data")
  bowker <- read_csv("../QPackage/inst//extdata/books/bowker.csv", status = read_status)
  print(read_status)
  
  # check id
  status <- check_id(bowker, list("id"))
  print(status)
  
  
  # set id
  status <- set_id(bowker, list("id"))
  print(status)
#}

}
# View data ----------------------------------------------- start
#View(bowker)
#View(walmart)

# ---- blocking

#do_block <- function(walmart, bowker, attr1, attr2) {
if(0) {
attr1 <- "isbn"
attr2 <- "isbn"
cand_set <-  apply_block(walmart, bowker, attr_equiv_block, attr1, attr2, col_names_a = list("title", "author", "binding", "publisher", "pages"), col_names_b = list("title", "author", "binding", "publisher", "pages"))
#}

# ---- sample data set
sampled_data <- sample_qtable(cand_set, 25)
  
# ---- label data set
labeled_data <- label_data(sampled_data)
}