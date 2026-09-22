test_that("background workers resolve internal helpers in the current package", {
  job <- background_job("redact_urls_in_text", list("Denied https://example.org/a?sig=SECRET&se=1 next"))
  on.exit(if (job$is_alive()) job$kill_tree(), add = TRUE)
  job$wait(timeout = 60000)
  expect_false(job$is_alive())
  expect_identical(job$get_result(), "Denied https://example.org/a next")
})

test_that("background remote previews reach licence validation", {
  tile <- data.frame(url = "https://example.org/a.laz", filename = "a.laz", provider = "test")
  job <- background_job("remote_preview_job", list(tile, tempfile(), 2, 100, 50, 50, 0))
  on.exit(if (job$is_alive()) job$kill_tree(), add = TRUE)
  job$wait(timeout = 60000)
  expect_false(job$is_alive())
  expect_error(job$get_result(), "licence and attribution")
})

test_that("embedded URL redaction handles multiple parameters and fragments", {
  expect_identical(redact_urls_in_text('First https://example.org/a?s=SECRET&x=2 then https://example.org/b#SECRET done'),
    'First https://example.org/a then https://example.org/b done')
})
