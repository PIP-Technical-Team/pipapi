log_file <- tempfile()
fs::file_delete(log_file)

test_that("clear_logs() is working as expected", {

  # Logs not found
  res <- clear_logs(log_file)
  expect_equal(res$status, 'error')
  expect_equal(res$msg, 'Log file not found')

  # Logs found and successfully cleared
  fs::file_create(log_file)
  res <- clear_logs(log_file)
  expect_equal(res$status, 'success')
  expect_equal(res$msg, 'Logs cleared.')
})
