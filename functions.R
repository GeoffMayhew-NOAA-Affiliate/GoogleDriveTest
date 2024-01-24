# Author: Geoff Mayhew
# Functions for GoogleDriveTest repository. Developing functions for streamlining workflow with Shared Google Drive.

# Make sure required packages are installed
if( !require("data.table", quietly = T) ) install.packages("data.table")
if( !require("rstudioapi", quietly = T) ) install.packages("rstudioapi")
if( !any(installed.packages()[, 1] == "googledrive") ) install.packages("googledrive")
# We want to make sure googledrive package is installed, but not necessarily loaded. Ideally, we will call googledrive::
# functions explicitly, because using drive_upload() or drive_download() accidentally may compromise the robustness of 
# the workflow the functions below provide. In the future, we will make sure "googledrive" is a dependency of the FMA
# analyst functions
if( !any(installed.packages()[, 1] == "crayon") ) install.packages("crayon")
# Same thing with the crayon package, which is useful for the bold() and italic() functions to format text that is sent
# to print in the console. However, those functions conflict with the highly-used flextable package, so crayon functions
# will also be called explicity with the package name.

#======================================================================================================================#
# Functions ####
#======================================================================================================================#

# This is a helper function for gdrive_dir() that checks for subfolders within folders
dir_search <- function(dribble) {
  
  dribble_lst <- vector(mode = "list", length = nrow(dribble))
  
  # Keep all parent folders
  parent <- copy(dribble)
  # Initialize count of files
  parent$files <- 0
  
  for( i in 1:length(dribble_lst) ) {
    
    drive_items <- googledrive::drive_ls(dribble[i, ])
    
    if( nrow(drive_items) == 0 ) {
      dribble_lst[[i]] <- list(
        parent = parent[i,],
        child = NULL
      )
    } else {

      x1 <- as.data.table(drive_items)
      # check which items are folders
      x1_check <- sapply(x1$drive_resource, function(x) x$mimeType %like% ".folder")
      parent[i,]$files <- sum(!x1_check)
      
      # Make new child folder names and their ids
      child <- x1[x1_check]
      for(j in 1:nrow(child)) {
        child[j, "name"] <- paste0(parent[i, ]$name, "/", child[j, ]$name)
      }
      
      dribble_lst[[i]] <- list(
        parent = parent[i,],
        child = child
      )
    }
  }
  
  list(
    parent = do.call(rbind, lapply(dribble_lst, "[[", "parent")),
    child = do.call(rbind, lapply(dribble_lst, "[[", "child"))
  )
  
}

#======================================================================================================================#

# The function looks up and displays the folder structure of a shared drive. By default, it searches through the FMA 
# Analytical Services shared drive.
gdrive_dir <- function(shared_id = c("Analytics")) {
  # Search the folder structure of the FMA shared google drive
  
  if( !is.character(shared_id) | length(shared_id) != 1) stop("'id' needs to be a length = 1 character string.")
  
  # Recall Hard coded dids from an alias
  if( shared_id == "Analytics") {
    id <- "0AJcHJWlPgKIgUk9PVA"
  } else stop("")
  
  # Get the dribble of the shared drive
  gdrive_dribble <- googledrive::shared_drive_get(id = id)
  parent <- copy(gdrive_dribble)
  parent_search <- dir_search(parent)
  
  #while( !is.null(parent_search$child) ) {
  while( nrow(parent_search$child) > 0 ) {
    new_parent <- parent_search$child
    new_parent_search <- dir_search(new_parent)
    parent_search <- list(
      parent = rbind(parent_search$parent, new_parent_search$parent),
      child = new_parent_search$child,
      fill = T
    )
  }
  
  # Omit the shared folder from the output
  folders_dt <- parent_search$parent[-1, ]
  # Omit the shared folder from name, and add a "/" to the path names to specify these are folders and not files
  folders_dt$gdrive_path <- sub(paste0(gdrive_dribble$name, "/"), "", paste0(folders_dt$name, "/"))

  # Print the results
  cat("Shared Drive:", gdrive_dribble$name, "\n")
  
  # Format the output
  out <- as.data.table(folders_dt[, c("gdrive_path", "files")])
  out <- out[order(out$gdrive_path)]
  out$abbr_name <- gsub("([^/]+)(?=/.+)", "..", out$gdrive_path, perl = T)  # close, just a bit too much
  out$nchar <- nchar(out$abbr_name)
  out$ws <- max(out$nchar) - out$nchar  
  out$Directory <- apply(out, 1, function(x) paste0(x["abbr_name"], paste(rep(" ", times = x["ws"]), collapse = "")))
  out[, c("Directory", "files", "gdrive_path")]
  
}

#======================================================================================================================#

# This function takes a path name from a shared drive and returns a single-row dribble to be used as the target folder 
# for uploads and downloads
gdrive_set_dribble <- function(gdrive_path, shared_id = "Analytics"){
  # `gdrive_path` is the google drive path of the one folder you want you target for uploads and downloads.
  
  if( !is.character(gdrive_path) | length(gdrive_path) != 1) stop("'id' needs to be a length = 1 character string.")
  if( !is.character(shared_id) | length(shared_id) != 1) stop("'shared_id' needs to be a length = 1 character string.")
  
  # Recall Hard coded ids from an alias
  if( shared_id == "Analytics") {
    id <- "0AJcHJWlPgKIgUk9PVA"
  }
  
  # Get the dribble object from the gdrive_path. It will contain roww for all enclosed folders and files.
  dribble_out <- googledrive::drive_get(path = gdrive_path, shared_drive = googledrive::as_id(id))
  
  # Only allow folders to be set as targets (exclude files)
  dribble_out <- dribble_out[
    sapply(dribble_out$drive_resource, "[[", "mimeType") == "application/vnd.google-apps.folder", 
  ]
  if( nrow(dribble_out) == 0 ){
    stop(paste0("Path ", crayon::bold(gdrive_path), " was not found!"))
  } else if( nrow(dribble_out) > 1 ){
    stop({
      cat(paste0(
        "Path name ", crayon::bold(gdrive_path), " is not specific enough. ", nrow(dribble_out), " matches found.\n"
      ))
    })
  } else if ( nrow(dribble_out) == 1 ){
    dribble_out
  }
}

#======================================================================================================================#

# This function shows all files, and not folders, within the a specified folder, referenced by name. This function is 
# really only for reference, and intentionally does not give results in dribble class.
gdrive_ls <- function(gdrive_dribble){

  if( !googledrive::is_dribble(gdrive_dribble) | nrow(gdrive_dribble) != 1) {
    stop("'gdrive_dibble' needs to be a nrow = 1 dribble.")
  }
  
  # Get all items in gdrive_dibble
  dribble_items <- googledrive::drive_ls(gdrive_dribble)
  # Subset to only files
  dribble_items <- dribble_items[
    sapply(dribble_items$drive_resource, "[[", "mimeType") != "application/vnd.google-apps.folder",
  ]
  
  if( nrow(dribble_items) == 0 ){
    cat(paste0("No files exist in ", crayon::bold(gdrive_dribble$path), ".\n"))
  } else {
    # Include the create dates in the output
    create_dates <- create_dates <- as.POSIXlt(
      sapply(dribble_items$drive_resource, "[[", "createdTime"), 
      format = "%Y-%m-%dT%H:%M:%OS", tz = "GMT")
    # Convert create dates to local timezone
    dribble_items$create_date <- as.POSIXct(format(as.POSIXct(create_dates), tz = Sys.timezone(), usetz = T))
    # Draw the file sizes from the drive_resources
    dribble_items$size <- sapply(
      as.integer(sapply(dribble_items$drive_resource, "[[", "size")),
      function(x) utils:::format.object_size(x, units = "auto"))
    # Return a data.frame without drive_resources so this object can't be abused as a dribble
    dribble_items$file_name <- dribble_items$name
    as.data.frame(
      dribble_items[order(dribble_items$create_date, decreasing = T), c("file_name", "create_date", "size")]
    )
  }
}

#======================================================================================================================#

# A helper function to parse a local path into its directory, file name, and file extension.
parse_local_path <- function(local_path){
  
  # Error checks
  # local_path must be a length 1 character string
  if( length(local_path) != 1 | !is.character(local_path) ) stop("`local_path` must a length = 1 character string.")
  # Can a file be found at the specified local path? If not, return 
  if( !file_test(op = "-f",  local_path) ) local_exists <- F else local_exists <- T
  
  # Get the name of the file (remove the directory, anything left if the final "/")
  name <- sub("^.*[/]", "", local_path)
  
  # Identify the extension and exclude it from the
  if( grepl(pattern = "^(.+)[.?](.+)$", x = name) ) {
    # If an extension is found, grab it
    extension <- sub(pattern = "^(.*)(?=[.])", "", name, perl = T)  # Omit everything to the left of the last period
  } else {
    # If an extension is not found, throw an error
    stop(paste0("'local_path' needs to have the file extension specified."))
  }
  # Get the file name without the extension
  name_no_ext <- sub(extension, "", name)
  
  # Determine whether a version suffix is present in the file name
  ver_flag <- grepl("(.+)(?=_v[0-9]{3}[.])", name, perl = T)
  
  # Get the directory, if present
  directory <- sub(name, "", local_path)
  
  # Return the parsed path
  list(
    path = local_path, name = name, directory = directory, name_no_ext = name_no_ext, 
    extension = extension, ver_flag = ver_flag, local_exists = local_exists)
}

#======================================================================================================================#

# A helper function to parse the google drive dribble, using the outputs of parse_local_path() to determine which files
# to search for.
parse_dribble <- function(gdrive_dribble, l_path) {

  # Can the Gdrive folder be found?
  if( !googledrive::is_dribble(gdrive_dribble) ) {
    stop(paste0(
      "'gdrive_dribble' must be specified as a dribble. Use ", italic("gdrive_set_dribble()"), " to do this."
    ))
  }
  if( nrow(gdrive_dribble) != 1 ) stop("'gdrive_dribble' must be 1 row.")
  
  # Identify all folders/files in the Gdrive path
  gdrive_items <- googledrive::drive_ls(gdrive_dribble)
  
  # If l_path has a version number, find that item specifically, Otherwise, match by name.
  if( l_path$ver_flag ) {
    
    # If a version flag is in l_path, does the specified version exist in the gdrive?
    ver_exists <- l_path$name == gdrive_items$name
    if( sum(ver_exists) == 1 ){
      gdrive_file <- gdrive_items[ver_exists, ]
    } else if( sum(ver_exists) > 1) {
      stop(paste0("File name is not unique!"))
    } else {
      stop(cat(paste0(crayon::bold(l_path$name), " not found in ", crayon::bold(gdrive_dribble$name))))
    }
    
  } else {
    # If a version flag is absent, search for all files with a similar l_path$name
    files_like_name_l <- grepl(
      pattern = paste0("^", l_path$name_no_ext, "_v[0-9]{3}", l_path$extension, "$"), 
      x = gdrive_items$name, ignore.case = T)
    gdrive_file <- gdrive_items[files_like_name_l, ]
  }
  
  # Get the create dates (when each file was uploaded to the Gdrive)
  if( nrow(gdrive_file) > 0 ){
    create_dates <- as.POSIXlt(
      sapply(gdrive_file$drive_resource, "[[", "createdTime"), 
      format = "%Y-%m-%dT%H:%M:%OS", tz = "GMT")
    # Ensure the files are sorted by create dates
    date_order <- order(create_dates, decreasing = T)
    create_dates <- create_dates[date_order]
    gdrive_file <- gdrive_file[date_order, ]
  } else create_dates <- NULL
  
  # Check if any files have duplicated names.
  if( any(duplicated(gdrive_file$name)) ){
    warning(paste0(
      "Gdrive files ", crayon::bold(paste0(unique(gdrive_file$name[duplicated(gdrive_file$name)]), collapse = ", " )), 
      " have duplicated file names. Fix this!"
    ))
  }
  
  # Make sure the order of create dates is in the same order of the version numbers.
  if( 
    any(diff(as.integer(sub(
      "(.+)_v(?=[0-9]{3})", "", sub(paste0(l_path$extension, "$"), "", gdrive_file$name), perl = T
    ))) >= 1)
  ){
    warning(paste0("Version numbers and create dates of Gdrive files are not in the same order! Fix this!"))
  }

  # Return dribble of Gdrive files and vector of create dates using local timezone
  list(
    files = gdrive_file, c_date_og = create_dates, 
    c_date = format(as.POSIXct(create_dates), tz = Sys.timezone(), usetz = T)
  )
}

#======================================================================================================================#

# A helper function that compares the local file with its versions on the gdrive (modify date, byte length, bytes)
compare_local_and_gdrive <- function(l_path, g_path){
  # local_path is length 1, but target can be an nrow()>1 dribble of files
  
  # Get information of local file
  local_info <- file.info(l_path$path)
  
  #' Get extended information from the target files on the Gdrive. This retrieves the 'modified time' of the local file 
  #' when it was uploaded (not the modified time of the object in the gdrive, which may be affected by things like file 
  #' name changes)
  #' drive_reveal() does take a little bit of time to run but is MUCH faster than running drive_read_raw on large files.
  #' It is also useful because this version of modifiedTime does not change if someone were to edit the file in any way, 
  #' including renaming it to something else and back.  
  drive_rev_info <- googledrive::drive_reveal(g_path$files, what = "published")$revision_resource
  drive_mtime <- as.POSIXct(sapply(
    sapply(drive_rev_info, "[[", "modifiedTime"),
    function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"), USE.NAMES = F))
  
  # Get the differences in modified time
  mtime_diff <- sapply(drive_mtime, function(x) difftime(local_info$mtime, x, units = "secs"))
  # Indicate relative recency of modify dates of local versus Gdrive files
  mtime_diff_sign <- sign(ifelse(abs(mtime_diff) < 1, 0, mtime_diff))
  # If difftime is less than 1 second, assume the modified times are the same
  mtime_same <- abs(mtime_diff) < 1
  
  # Get the size in bytes of the Gdrive files
  drive_bytes <- as.numeric(sapply(drive_rev_info, "[[", "size"))
  bytes_same <- local_info$size == drive_bytes
  
  # Test to see which files are identical in size and modified time
  files_same <- mtime_same & bytes_same
  
  # If more than one file matches perfectly, we've allowed duplicates on the gdrive. This should never happen!
  if( length(which(mtime_same)) > 1 ){
    # If more than one file has the same modify time and size, probably need to delete one!
    warning("Somehow multiple Gdrive files have the same modified time as the local file. Aborting!")
    print(paste("Local file modified time: ", local_info$mtime))
    print(data.frame(file = g_path$files$name, drive_mtime = drive_mtime))
    return(list(local_mtime = local_info$mtime, data.frame(file = g_path$files$name, drive_mtime = drive_mtime)))
  } 
  
  # If the local file is more recent, test to see if the data actually differs from the most recent Gdrive version.
  if( mtime_diff_sign[1] == 1 ){
    # If the number of bytes is the same, proceed to reading bytes. Reading bytes for Gdrive files might take a few 
    # seconds for a 250Mb file, but it otherwise pretty quick.
    if( bytes_same[1] ){
      # If the files are identical, flip the files_same flag
      files_same[1] <- identical(
        # Local file
        readBin(l_path$path, what = "raw", n = local_info$size), 
        # Gdrive file
        googledrive::drive_read_raw(g_path$files[1,])
      )
      cat(paste0(
        "Local file ", crayon::bold(l_path$name), " is identical ", crayon::bold(g_path$files[1,]$name), 
        " although it was modified recently.\n"
      ))
    } 
  }
  
  # Print messages
  # If the local is up to date, proceed
  if( files_same[1] == TRUE ){
    # If local is up-to-date, no message is needed.
  } else if( mtime_diff_sign[1] == -1 ){
    # If the local is behind the gdrive, give a warning
    warning(paste0("Local file ", crayon::bold(l_path$name), " is ", crayon::bold("behind"), " the gdrive!\n"))
    # If the local matches an older version, print it.
    if( any(files_same) ){
      cat(paste0(
        "Local file ", l_path$name, " seems to match ", crayon::bold(g_path$files$name[files_same]), "(",  drive_mtime[files_same], ") but ", 
        crayon::bold(g_path$files$name[1]), " (", drive_mtime[1], ") is the most recent version.\n"))
    }
  } else if( mtime_diff_sign[1] == 1 ){
    # If the local is ahead of the gdrive
    cat(paste0("Local file ", l_path$name, " is ", crayon::bold("ahead"), " of the gdrive.\n"))
  }
  
  # Outputs
  list(
    local_up_to_date = files_same[1],
    local_minus_gdrive_mtime = mtime_diff_sign[1],
    local_match_gdrive_vec = files_same
  )
}

#======================================================================================================================#

# Uploads files to the specified directory in the shared Gdrive.
gdrive_upload <- function(local_path, gdrive_dribble) {
  # 'local_path' is the path to the local file and must include the file extension.
  # 'path' is the dribble of the Gdrive where you want to upload the file to.

  # Parse the local path to get the directory, file name, extension, and whether a version flag exists
  l_path <- parse_local_path(local_path)
  # If the local path doesn't exist, abort!
  if( !l_path$local_exists ) stop(paste0(crayon::bold(local_path), " cannot be found."))
  
  # Prevent uploading files with a version suffix.
  if( l_path$ver_flag ) stop("Do not upload files with a version suffix!")
  
  # Parse the path to the google drive, looking for the files specified in local_path
  g_path <- parse_dribble(gdrive_dribble, l_path)
  
  # If other version of this file already exist on the drive...
  if( nrow(g_path$files) > 0  ) {
    
    # Determine the next version number as the next number after the most recent version
    # Remove the file name, "_v", and the extension to get left with the version number
    next_v <- paste0("_v", formatC(as.integer(sub(
      paste0(l_path$name_no_ext, "_v"),
      replace = "", 
      sub(paste0(l_path$extension, "$"), "", g_path$files$name[1])
    )) + 1, digits = 0, width = 3, flag = "0"))
    # Or match it with a lookbehind for "_v" an a lookahead for the extension
    # next_v <- paste0("_v", formatC(as.integer(regmatches(
    #   g_path$files$name[1],
    #   regexpr(paste0("(?<=_v)([0-9]{3})", "(?=", l_path$extension, ")"), g_path$files$name[1], perl = T)
    # )) + 1, digits = 0, width = 3, flag = "0"))
    
    gdrive_file_name <- paste0(l_path$name_no_ext, next_v, l_path$extension)
    
    # Grab the most recent version
    most_recent <- g_path$files[1, ]
    
    # Print the count of instances of their object in the Gdrive folder
    cat(paste0(nrow(g_path$files), " instance(s) of ", crayon::bold(l_path$name), " found in the Gdrive.\n"))
    cat(paste0("The most recent version is ", crayon::bold(most_recent$name), ", uploaded on ", g_path$c_date[1], ".\n\n" ))

    # Compare the local file with Gdrive versions 
    local_check <- compare_local_and_gdrive(l_path, g_path)
    
    # Skip the upload if the local is up to date or behind the gdrive
    if( local_check$local_up_to_date | local_check$local_minus_gdrive_mtime == -1 ){
      # If the Gdrive is already up to date or ahead of the local, cancel the upload
      return(cat(paste0(
        crayon::bold(l_path$name), " is identical to ", crayon::bold(most_recent$name), ". Skipping upload.\n"
      )))
    }  
  } else {
    # If this name does not exist, assign it a version number starting with _v000
    gdrive_file_name <- paste0(l_path$name_no_ext, "_v000", l_path$extension)
  }

  # Confirm name and location of upload
  cat(paste0(
    crayon::bold(local_path), " will be uploaded as ", crayon::bold(gdrive_file_name), 
    " to ", crayon::bold(gdrive_dribble$name), ".\n"
  ))
  upload_response <- rstudioapi::showPrompt(
    title = "Notice!",
    message = "Proceed with upload? (Y/N)"
  )
  if( upload_response == "Y"){
    # Upload the file. Make the modifiedTime match that of the local file
    # Get the modify time of the local file
    local_mtime <- file.info(local_path)$mtime
    # convert this to the format Google wants
    new_mtime <- paste0(sub("(?<=[0-9])( )(?=[0-9])", "T", format(local_mtime, tz = "GMT"), perl = T), ".000Z")
    googledrive::drive_upload(
      media = local_path, path = gdrive_dribble, name = gdrive_file_name, overwrite = FALSE,
      modifiedTime = new_mtime
    )
  } else if ( upload_response == "N"){
    message("Upload to Gdrive aborted.")
  } else {
    stop("Response was not either 'Y' or 'N'. Aborting upload.")
  }
}

#======================================================================================================================#

# Downloads files to the specified directory in the shared Gdrive. Checks to make sure that the local data file exists
# and is up-to-date with the Gdrive, and otherwise skips the download.
gdrive_download <- function(local_path, gdrive_dribble) {
  # `local_path` is the local path to where you want to save the file and contains the name of the file.
  # `gdrive_dribble` is the dribble of the folder containing the file you want to pull
  
  # Parse the local path to determine whether the file exists locally and what to search for on the Gdrive.
  l_path <- parse_local_path(local_path)
  # Parse the Gdrive path, using l_path to determine what to search for in the Gdrive.
  g_path <- parse_dribble(gdrive_dribble, l_path)
  
  # Input checks
  # Can the directory in local_path be found?
  if( !file_test(op = "-d",  l_path$directory) ) {
    stop(paste0("Local path, ", crayon::bold(l_path$directory), ", not found."))} 

  # Vary the action depending on if a local version of the file exists and if the target is a specific version.
  if( l_path$local_exists ){
    # If a local exists, compare it with the version(s) on the gdrive.
    local_check <- compare_local_and_gdrive(l_path, g_path)
    
    if( l_path$ver_flag ){
      # If downloading a specific version, make sure it matches
      if( local_check$local_up_to_date ){
        return(cat(paste0(crayon::bold(l_path$name), " found locally. Skipping download.\n")))
      } else {
        stop(paste0("Somehow the local version of ", l_path$name, "differs from the gdrive's. Fix this!"))
      }
    } else {
      # If trying to stay up-to-date
      if( local_check$local_up_to_date ){
        return(cat(paste0(crayon::bold(l_path$name), " is up-to-date with Gdrive. Skipping download.\n")))
      } else if (  local_check$local_minus_gdrive_mtime == 1 ){
        # If the local is ahead
        return(cat(paste0("Skipping download, but consider if you need up update the Gdrive with ", italic("gdrive_upload()", ".\n"))))
      } else if ( local_check$local_minus_gdrive_mtime == -1 ){
        # If the local is behind, prompt the download and overwrite
        cat(paste0("Local version of ", crayon::bold(l_path$name), " is ", red("out of date"), " with the Gdrive!\n"))
        if( any(local_check$local_match_gdrive_vec) ){
          cat(paste0(
            "Most recent version is ", crayon::bold(g_path$files[1, ]$name), " but your local matches ", 
            crayon::bold(g_path$files[local_check$local_match_gdrive_vec ,]$name), ".\n" ))
        }
        download_response <- rstudioapi::showPrompt(
          title = "Notice!", 
          message = paste0("Overwrite and update your local version of ", l_path$name, "? (Y/N)"))
        if( toupper(download_response) == "Y" ) {
          googledrive::drive_download(file = g_path$files[1,], path = local_path, overwrite = T)
        } else if( toupper(download_response) == "N") {
          return(cat(paste0("Download of ", crayon::bold(g_path$files[1, ]$name), " aborted.")))
        } else stop("You didnt enter Y or N!")
      }
    }
  } else{
    # If no local exists, simply download the file
    # Download the file
    if( l_path$ver_flag ){
      # If a version suffix exists, download that specific file version.
      googledrive::drive_download(file = g_path$files, path = local_path, overwrite = F)
    } else {
      # If no version suffix exists, download the most recent version.
      googledrive::drive_download(file = g_path$files[1,], path = local_path, overwrite = F)
    }
  }
}
