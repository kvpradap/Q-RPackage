# import files ----------------------------------------------- start

# -- walmart
#read_files <- function() {
if(0) {
  print("Importing walmart data")
  walmart <- read_csv("../QPackage/inst//extdata/walmartProds_cleaned.csv", status = read_status)
  print(read_status)
  
  # check id
  status <- check_id(walmart, list("id"))
  print(status)
  
  
  # set id
  status <- set_id(walmart, list("id"))
  print(status)
  
  
  # -- amazon
  print("Importing amazon data")
  amazon <- read_csv("../QPackage/inst//extdata/amazonProds_cleaned.csv", status = read_status)
  print(read_status)
  
  # check id
  status <- check_id(amazon, list("id"))
  print(status)
  
  
  # set id
  status <- set_id(amazon, list("id"))
  print(status)
#}

}
# View data ----------------------------------------------- start
#View(amazon)
#View(walmart)

# ---- blocking

#do_block <- function(walmart, amazon, attr1, attr2) {
if(0) {
attr1 <- "brand"
attr2 <- "brand"
cand_set <-  apply_block(walmart, amazon, attr_equiv_block, attr1, attr2, col_names_a = list("title", "price"), col_names_b = list("title", "price"))
#}

# ---- sample data set
sampled_data <- sample_qtable(cand_set, 25)
  
# ---- label data set
labeled_data <- label_data(sampled_data)
}