
require(testthat)
require(data.table)

test_data_path <- file.path(test_path(), "testdata")

example_data <- data.frame(x = 1:5, y = LETTERS[5:1], z = 0)

fwrite(example_data, file.path(test_data_path, "simple.csv"))

saveRDS(example_data, file.path(test_data_path, "simple.rds"))
