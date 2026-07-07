# Conversión incremental de archivos BACI CSV a particiones Parquet.

prepare_baci_parquet <- function(
  raw_dir = "data/raw",
  parquet_dir = "data/raw/parquet",
  overwrite = FALSE
) {
  csv_files <- fs::dir_ls(raw_dir, regexp = "BACI_HS07_Y.*\\.csv$")

  if (length(csv_files) == 0) {
    rlang::abort(glue::glue("No se encontraron archivos BACI CSV en '{raw_dir}'."))
  }

  connection <- DBI::dbConnect(
    duckdb::duckdb(),
    file.path(raw_dir, "baci.duckdb")
  )
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  purrr::walk(csv_files, function(csv_file) {
    year <- stringr::str_extract(fs::path_file(csv_file), "\\d{4}")
    output_file <- file.path(
      parquet_dir,
      glue::glue("year={year}"),
      "data.parquet"
    )

    if (file.exists(output_file) && !overwrite) {
      return(invisible(output_file))
    }

    fs::dir_create(fs::path_dir(output_file))
    normalized_input <- normalizePath(csv_file, winslash = "/", mustWork = TRUE)
    normalized_output <- normalizePath(
      fs::path_dir(output_file),
      winslash = "/",
      mustWork = TRUE
    ) |>
      file.path("data.parquet") |>
      stringr::str_replace_all("\\\\", "/")

    DBI::dbExecute(connection, glue::glue("\n      COPY (\n        SELECT\n          t::INTEGER AS year,\n          k::VARCHAR AS product,\n          i::INTEGER AS exporter,\n          j::INTEGER AS importer,\n          v::DOUBLE AS value_1000usd,\n          q::DOUBLE AS quantity_tons\n        FROM read_csv_auto('{normalized_input}', HEADER = TRUE)\n      )\n      TO '{normalized_output}' (FORMAT PARQUET);\n    "))
  })

  invisible(parquet_dir)
}
