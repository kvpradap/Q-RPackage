# -- import data
d <- ReadCsv('../QPackage/data/DBLP_cleaned.csv', idCol="id")

d_sample <- QTable(d[1:10, ])

d_sample <- SetId(d_sample, d@idCol)

#inv_index <- create_inv_index(d_sample, "year")

#inv_index <- inv_index[lapply(inv_index, length) > 1]

#cand_set <- get_pairs(inv_index)
df <- block_attr_equivalence(d_sample, "year")




