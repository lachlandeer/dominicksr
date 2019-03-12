
file_writable <- function(path) {
    assertthat::is.string(path) &&
        file.create(path) &&
        assertthat::is.writeable(path)
}

download_customer_counts <- function(destination = "./ccounts.csv"){
    # check destination is writeable
    file_writable(destination)

    counts_url <- "https://www.chicagobooth.edu/boothsitecore/docs/dff/store-demos-customer-count/ccount_stata.zip"
    # create a temporary directory
    td = tempdir()
    # create the placeholder file
    tf = tempfile(tmpdir = td, fileext = ".zip")
    message('Attempting to download customer counts')
    # download into the placeholder file
    download.file(counts_url, tf)
    # get the name of the first file in the zip archive
    fname = unzip(tf, list=TRUE)$Name[1]
    # unzip the file to the temporary directory
    unzip(tf, files=fname, exdir=td, overwrite=TRUE)
    # fpath is the full path to the extracted file
    fpath = file.path(td, fname)
    # import data and write to csv. encoding option to due old stata
    message(paste('Writing customer counts to file:', destination, sep = " "))
    df <- haven::read_dta(fpath, encoding = 'latin1')
    rio::export(df, destination)
    # clean up
    temp_files <- list.files(td, full.names = T, pattern = "^file")
    invisible(file.remove(temp_files))
}
