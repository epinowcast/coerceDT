
test_data <- function(filename) file.path(test_path(), "testdata", filename)

# paths
test_csv <- test_data("simple.csv")
test_rds <- test_data("simple.rds")
# files
test_obj <- readRDS(test_rds)
test_std <- as.data.table(readRDS(test_rds))

allmodes <- list(csv = test_csv, rds = test_rds, obj = test_obj, ref = test_std)

get_a <- function(mode = c("csv", "rds", "obj", "std")) {
  switch (match.arg(mode),
    csv = test_csv, rds = test_rds,
    obj = test_obj,
    std = data.table::copy(test_std)
  )
}
