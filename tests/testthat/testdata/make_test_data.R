
require(testthat)
require(data.table)

test_data_path <- file.path(test_path(), "testdata")

example_data <- data.frame(x = 1L:5L, y = LETTERS[5L:1L], z = 0L)

fwrite(example_data, file.path(test_data_path, "simple.csv"))

saveRDS(example_data, file.path(test_data_path, "simple.rds"))
