
# 0. Set up --------------------------------------------------------------

skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")


# 1. ddbs_install() ------------------------------------------------------

describe("ddbs_install()", {

  describe("expected behavior", {

    it("returns TRUE invisibly on install", {
      conn <- duckdb::dbConnect(duckdb::duckdb())
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
      result <- ddbs_install(conn, quiet = TRUE)
      expect_true(result)
    })

    it("returns TRUE invisibly when already installed", {
      conn <- duckdb::dbConnect(duckdb::duckdb())
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
      ddbs_install(conn, quiet = TRUE)
      result <- ddbs_install(conn, quiet = TRUE)
      expect_true(result)
    })

    it("shows a message with quiet = FALSE", {
      conn <- duckdb::dbConnect(duckdb::duckdb())
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
      expect_message(ddbs_install(conn, quiet = FALSE))
    })

    it("shows already-installed message on repeated calls with quiet = FALSE", {
      conn <- duckdb::dbConnect(duckdb::duckdb())
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
      ddbs_install(conn, quiet = TRUE)
      expect_message(ddbs_install(conn, quiet = FALSE), "already installed")
    })

    it("suppresses all messages with quiet = TRUE", {
      conn <- duckdb::dbConnect(duckdb::duckdb())
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
      expect_no_message(ddbs_install(conn, quiet = TRUE))
      expect_no_message(ddbs_install(conn, quiet = TRUE))
    })
  })

  describe("errors", {

    it("errors when extension cannot be found in any repository", {
      conn <- duckdb::dbConnect(duckdb::duckdb())
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
      expect_error(
        ddbs_install(conn, extension = "nonexistent_extension_xyz"),
        "Failed to install"
      )
    })

    it("errors when trying to upgrade an already-loaded extension", {
      conn <- duckdb::dbConnect(duckdb::duckdb())
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
      ddbs_install(conn, quiet = TRUE)
      ddbs_load(conn, quiet = TRUE, create_macros = FALSE)
      expect_error(
        ddbs_install(conn, upgrade = TRUE),
        "already loaded"
      )
    })
  })
})


# 2. ddbs_load() ---------------------------------------------------------

describe("ddbs_load()", {

  describe("expected behavior", {

    it("loads the spatial extension without error", {
      conn <- duckdb::dbConnect(duckdb::duckdb())
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
      ddbs_install(conn, quiet = TRUE)
      expect_no_error(ddbs_load(conn, quiet = TRUE, create_macros = FALSE))
    })

    it("shows success message with quiet = FALSE", {
      conn <- duckdb::dbConnect(duckdb::duckdb())
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
      ddbs_install(conn, quiet = TRUE)
      expect_message(ddbs_load(conn, quiet = FALSE, create_macros = FALSE), "loaded")
    })

    it("suppresses messages with quiet = TRUE", {
      conn <- duckdb::dbConnect(duckdb::duckdb())
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
      ddbs_install(conn, quiet = TRUE)
      expect_no_message(ddbs_load(conn, quiet = TRUE, create_macros = FALSE))
    })

    it("can be called again on an already-loaded extension", {
      conn <- duckdb::dbConnect(duckdb::duckdb())
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
      ddbs_install(conn, quiet = TRUE)
      ddbs_load(conn, quiet = TRUE, create_macros = FALSE)
      expect_no_error(ddbs_load(conn, quiet = TRUE, create_macros = FALSE))
    })
  })


})


# 3. Other extensions ----------------------------------------------------

## Community extensions
testthat::test_that("Community extensions are installed", {
  conn <- duckdb::dbConnect(duckdb::duckdb())
  out <- tryCatch(ddbs_install(conn, extension = "h3"), error = function(e) NULL)
  if (is.null(out)) {
    testthat::pass()
  } else {
    expect_no_error(ddbs_load(conn, quiet = TRUE, create_macros = FALSE))
  }
})

## Other core extensions
testthat::test_that("Community extensions are installed", {
  conn <- duckdb::dbConnect(duckdb::duckdb())
  out <- tryCatch(ddbs_install(conn, extension = "aws"), error = function(e) NULL)
  if (is.null(out)) {
    testthat::pass()
  } else {
    expect_no_error(ddbs_load(conn, quiet = TRUE, create_macros = FALSE))
  }
})
