# Author: Geoff Mayhew
# Functions for GoogleDriveTest repository. Developing functions for streamlining workflow with Shared Google Drive

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


#' TODO 
#' *If you wanted to upload multiple files*, 
#' can you make a convenient way to do this? allow local_path to be length >1?

#' TODO
#' *rename function arguments 'gdrive_path' to 'target?'* 
#' Make something more explicit? 


#' TODO
#' *Currently version number looks at the most recent create date.* Make sure it's also max(version number?)
#' Make g_path output include vector of version numbers?

#======================================================================================================================#
# Functions ####
#======================================================================================================================#

# This is a helper function for gdrive_dir() that checks for subfolders within folders
gdrive_dir_search <- function(dribble) {
  
  # TODO 
  # googledrive::drive_reveal(x, what = "path")
  # This gives the whole path to a file or folder!
  # 
  
  # dribble <- copy(fma_gdrive_dribble)
  # dribble <- copy(new_parent)
  # dribble <- copy(parent_search[[1]]$child)
  
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
# TODO # Automatically omit some folder names from the search, or specify subfolders to focus the search on?
# TODO Make this return only 'name' and not in dribble format so it cannot be abused? Return # of folders and files?

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
  parent_search <- gdrive_dir_search(parent)
  
  #while( !is.null(parent_search$child) ) {
  while( nrow(parent_search$child) > 0 ) {
    new_parent <- parent_search$child
    new_parent_search <- gdrive_dir_search(new_parent)
    parent_search <- list(
      parent = rbind(parent_search$parent, new_parent_search$parent),
      child = new_parent_search$child,
      fill = T
    )
  }
  
  # Omit the shared folder from the output
  folders_dt <- parent_search$parent[-1, ]
  # Omit the shared folder from name, and add a "/" to the path names to specify these are folders and not files
  folders_dt$path_name <- sub(paste0(gdrive_dribble$name, "/"), "", paste0(folders_dt$name, "/"))

  # Print the results
  cat("Shared Drive:", gdrive_dribble$name, "\n")
  #print(folders_dt[order(folders_dt$name), ], justify = "left")
  
  
  new <- as.data.table(folders_dt[, c("path_name", "files")])
  # Make sure everything is ordered alphabetically
  new <- new[order(new$path_name)]
  new$abbr_name <- gsub("([^/]+)(?=/.+)", "..", new$path_name, perl = T)  # close, just a bit too much
  # Can I print all this using 'cat' so I don't have row numbers and I can color file names? Need to find max(nchar()) of abbr_name column
  #print(new[, "abbr_name"], justify = "left")
  
  new$nchar <- nchar(new$abbr_name)
  new$ws <- max(new$nchar) - new$nchar  
  new$abbr_name <- apply(new, 1, function(x) paste0(x["abbr_name"], paste(rep(" ", times = x["ws"]), collapse = "")))
  
  new[, c("abbr_name", "files", "path_name")]
  
}

#======================================================================================================================#

# This function takes a path name from a shared drive and returns a single-row dribble to be used as the target for
# uploads and downloads
gdrive_target <- function(path_name, shared_id = "Analytics"){
  # `path_name` is the path of the folders you want you focus on
  
  if( !is.character(path_name) | length(path_name) != 1) stop("'id' needs to be a length = 1 character string.")
  if( !is.character(shared_id) | length(shared_id) != 1) stop("'shared_id' needs to be a length = 1 character string.")
  
  # Recall Hard coded dids from an alias
  if( shared_id == "Analytics") {
    shared_id <- "0AJcHJWlPgKIgUk9PVA"
  }
  
  # Search 
  dribble_out <- googledrive::drive_get(path = path_name, shared_drive = googledrive::as_id(shared_id))
  
  # Only allow folders to be set as targets!
  dribble_out <- dribble_out[sapply(dribble_out$drive_resource, "[[", "mimeType") == "application/vnd.google-apps.folder", ]
  
  if( nrow(dribble_out) == 0 ){
    stop(paste0("Path ", crayon::bold(path_name), " was not found!"))
  } else if( nrow(dribble_out) > 1 ){
    stop({
      cat(paste0("Path name ", crayon::bold(path_name), " is not specific enough. ", nrow(dribble_out), " matches found.\n"))
    })
  } else if ( nrow(dribble_out) == 1 ){
    dribble_out
  }
}

#======================================================================================================================#

# This function shows all files, and not folders, within the a specified folder, referenced by name. This function is 
# really only for reference, and intentionally does not give results in dribble class.
gdrive_ls <- function(target){
  # `path_name` is the path of the folders you want you focus on
  
  if( !googledrive::is_dribble(target) | nrow(target) != 1) stop("'target' needs to be a nrow = 1 dribble.")
  
  # Get all items in target
  target_items <- googledrive::drive_ls(target)
  # Subset to only files
  target_items <- target_items[sapply(target_items$drive_resource, "[[", "mimeType") != "application/vnd.google-apps.folder"  ,]
  
  if( nrow(target_items) == 0 ){
    cat(paste0("No files exist in ", crayon::bold(target$path), ".\n"))
  } else {
    # Include the create dates in the output
    create_dates <- create_dates <- as.POSIXlt(
      sapply(target_items$drive_resource, "[[", "createdTime"), 
      format = "%Y-%m-%dT%H:%M:%OS", tz = "GMT")
    # Convert create dates to local timezone
    target_items$create_date <- as.POSIXct(format(as.POSIXct(create_dates), tz = Sys.timezone(), usetz = T))
    # Draw the file sizes from the drive_resources
    target_items$size <- sapply(
      as.integer(sapply(target_items$drive_resource, "[[", "size")),
      function(x) utils:::format.object_size(x, units = "auto"))
    # Return a data.frame without drive_resources so this object can't be abused as a dribble
    target_items$file_name <- target_items$name
    as.data.frame(target_items[, c("file_name", "create_date", "size")])
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
  
  # Identify the extension
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

# A helper function to parse the google drive path, using the outputs of parse_local_path() to determine which files
# to search for.
parse_gdrive_path <- function(gdrive_path, l_path) {
  # gdrive_path <- copy(gdrive_test_dribble); l_path <- parse_local_path(local_path)
  
  # Identify all folders/files in the gdrive path
  gdrive_path_items <- googledrive::drive_ls(gdrive_path)
  
  # If l_path has a version number, find that item specifically, Otherwise, match by name.
  if( l_path$ver_flag ) {
    
    # If a version flag is in l_path, does the specified version exist in the gdrive?
    ver_exists <- l_path$name == gdrive_path_items$name
    if( sum(ver_exists) == 1 ){
      gdrive_file <- gdrive_path_items[ver_exists, ]
    } else if( sum(ver_exists) > 1) {
      stop(paste0("File name is not unique!"))
    } else {
      stop(cat(paste0(crayon::bold(l_path$name), " not found in ", crayon::bold(gdrive_path$name))))
    }
    
  } else {
    # If a version flag is absent, search for all files with a similar l_path$name
    files_like_name_l <- grepl(
      pattern = paste0("^", l_path$name_no_ext, "_v[0-9]{3}", l_path$extension, "$"), 
      x = gdrive_path_items$name, ignore.case = T)
    gdrive_file <- gdrive_path_items[files_like_name_l, ]
  }
  
  # Get the create dates
  if( nrow(gdrive_file) > 0 ){
    create_dates <- as.POSIXlt(
      sapply(gdrive_file$drive_resource, "[[", "createdTime"), 
      format = "%Y-%m-%dT%H:%M:%OS", tz = "GMT")
  } else create_dates <- NULL
  
  # Ensure the files are sorted by create dates
  date_order <- order(create_dates, decreasing = T)
  create_dates <- create_dates[date_order]
  gdrive_file <- gdrive_file[date_order, ]
  
  # Return dribble of gdrive files and vector of create dates using local timezone
  list(files = gdrive_file, c_date_og = create_dates, c_date = format(as.POSIXct(create_dates), tz = Sys.timezone(), usetz = T))
}

#======================================================================================================================#

# A helper function that compares the local file with its versions on the gdrive (modify date, byte length, bytes)
compare_local_and_gdrive <- function(l_path, g_path){
  # local_path is length 1, but target can be an nrow()>1 dribble of files
  
  # Get information of local file
  local_info <- file.info(l_path$path)
  
  # Get extended information from the target files on the gdrive
  # This retrieves the 'modified time' of the local file when it was uploaded (not the modified time of the object in
  # the gdrive, which may be affected by things like file name changes)
  # drive_reveal does take a little bit of time to run bit its MUCH faster than running drive_read_raw on large files
  drive_rev_info <- googledrive::drive_reveal(g_path$files, what = "published")$revision_resource
  drive_mtime <- as.POSIXct(sapply(
    sapply(drive_rev_info, "[[", "modifiedTime"),
    function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"), USE.NAMES = F))
  
  # Get the differences in modified time
  mtime_diff <- sapply(drive_mtime, function(x) difftime(local_info$mtime, x, units = "secs"))
  # Indicate relative recency of modify dates of local versus gdrive files
  mtime_diff_sign <- sign(ifelse(abs(mtime_diff) < 1, 0, mtime_diff))
  # If difftime is less than 1 second, assume the modified times are the same
  mtime_same <- abs(mtime_diff) < 1
  
  # Get the size in bytes of the gdrive files
  drive_bytes <- as.numeric(sapply(drive_rev_info, "[[", "size"))
  bytes_same <- local_info$size == drive_bytes
  
  # Test to see which files are identical in size and modified time
  files_same <- mtime_same & bytes_same
  
  # If more than one file matches perfectly, we've allowed duplicates on the gdrive. This should never happen!
  if( length(which(mtime_same)) > 1 ){
    # If more than one file has the same modify time and size, probably need to delete one!
    warning("Somehow multiple gdrive files have the same modified time as the local file. Aborting!")
    print(paste("Local file modified time: ", local_info$mtime))
    print(data.frame(file = g_path$files$name, drive_mtime = drive_mtime))
    return(list(local_mtime = local_info$mtime, data.frame(file = g_path$files$name, drive_mtime = drive_mtime)))
  } 
  
  # If the local file is more recent, test to see if the data actually differs from the most recent Gdrive version.
  if( mtime_diff_sign[1] == 1 ){
    # If the number of bytes is the same, proceed to reading bytes. Reading bytes for gdrive files might take a few 
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
        "Local file ", crayon::bold(l_path$name), " is identical to ", crayon::bold(g_path$files[1,]$name), 
        " in spite of being modified recently.\n"
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
  # 'path' is the dribble of the gdrive where you want to upload the file to 
  
  
  # local_path <- "Data/data_test.Rdata"; gdrive_dribble <- gdrive_test_dribble
  
  # FOR TESTING
  # char_vec <- c(local_path, "file/missing_extension.txt", "file/missing_extension.", "file/.missingextension", "file/missing_extension", "file/something.other.txt", "file.txt")
  
  # Parse the local path to get the directory, file name, extension, and whether a version flag exists
  l_path <- parse_local_path(local_path)
  # If the local path doesn't exist, abort!
  if( !l_path$local_exists ) stop(paste0(crayon::bold(local_path), " cannot be found."))
  
  # Prevent uploading files with a version suffix.
  if( l_path$ver_flag ) stop("Do not upload files with a version suffix!")
  
  # Parse the path to the google drive, looking for the files specified in local_path
  g_path <- parse_gdrive_path(gdrive_dribble, l_path)
  
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
    
    # Print the count of instances of their object in the gdrive folder
    cat(paste0(nrow(g_path$files), " instance(s) of ", crayon::bold(l_path$name), " found.\n"))
    cat(paste0("The most recent version is ", crayon::bold(most_recent$name), ", uploaded on ", g_path$c_date[1], ".\n\n" ))

    # Compare the local file with gdrive versions 
    local_check <- compare_local_and_gdrive(l_path, g_path)
    
    # Skip the upload if the local is up to date or behind the gdrive
    if( local_check$local_up_to_date | local_check$local_minus_gdrive_mtime == -1 ){
      # If the gdrive is already up to date or ahead of the local, cancel the upload
      return(cat("Skipping upload.\n"))
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
    googledrive::drive_upload(media = local_path, path = gdrive_dribble, name = gdrive_file_name, overwrite = FALSE)
  } else if ( upload_response == "N"){
    stop("Upload to Gdriveaborted.")
  } else {
    stop("Response was not either 'Y' or 'N'. Aborting upload.")
  }
}

#======================================================================================================================#

# Downloads files to the specified directory in the shared Gdrive. Checks to make sure that the local data file exists
# and is up-to-date with the Gdrive, and otherwise skips the download.
gdrive_download <- function(local_path, gdrive_dribble) {
  
  # `local_path` is the local path to where you want to save the file and contains the name of the file.
  # `gdrive_path` is the dribble of the folder containing the file you want to pull
  
  # Parse the local path to determine whether the file exists locally and what to search for on the Gdrive.
  l_path <- parse_local_path(local_path)
  # Parse the Gdrive path, using l_path to determine what to search for in the Gdrive.
  g_path <- parse_gdrive_path(gdrive_dribble, l_path)
  
  # Input checks
  # Can the directory in local_path be found?
  if( !file_test(op = "-d",  l_path$directory) ) {
    stop(paste0("Local path, ", crayon::bold(l_path$directory), ", not found."))} 
  # Can the Gdrive folder be found?
  if( !googledrive::is_dribble(gdrive_dribble) ) stop("The Gdrive location, 'gdrive_dribble', must be specified as a dribble.")

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
        return(cat(paste0("Skipping download, but consider if you need up update the gdrive with ", italic("gdrive_upload()", ".\n"))))
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
    googledrive::drive_download(file = g_path$files, path = local_path, overwrite = F)
  }
}