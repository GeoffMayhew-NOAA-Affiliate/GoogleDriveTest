# Author: Geoff Mayhew
# Functions for GoogleDriveTest repository. Developing functions for streamlining workflow with Shared Google Drive.

# Make sure required packages are installed
if( !require("data.table", quietly = T) ) install.packages("data.table")   #' TODO *Remove dependency on data.table!*

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

#' *A helper function that compares the local file with its versions on the gdrive*
compare_local_and_gdrive <- function(l_path, g_path){
  # local_path is length 1, but target can be an nrow()>1 dribble of files
  
  # Get information of local file
  local_info <- file.info(l_path$path)
  
  # Get the most recent gdrive version
  gdrive_head <- g_path$revision_lst[[g_path$current_ver]]
  # compare file size
  size_match <- gdrive_head$size == file.size(l_path$path)
  # compare modified time (gdrive - local)
  mtime_match <- (gdrive_head$modifiedTime - trunc(local_info$mtime))
  
  # Compare files
  if( size_match & mtime_match == 0 ){
    #' *If modified times and byte lengths are the same, treat them as identical*
    identical <- T
    local_status <- "up to date with"
    gdrive_raw <- NULL
  } else if( size_match ){
    # If the sizes match, compare the bytes. This may be time consuming for large files.
    local_raw <- readBin(l_path$path, what = "raw", n = local_info$size)
    gdrive_raw <- gargle::request_make(gargle::request_build(
      method = "GET",
      path = "drive/v3/files/{fileId}",  
      params = list(
        fileId = g_path$gdrive_item$id, revisionId = gdrive_head$id, supportsAllDrives = TRUE, alt = "media"
      ),
      token = googledrive::drive_token()
    ))
    
    # If the bytes are the same, we know the files are identical
    if( all(gdrive_raw$content == local_raw) ) {
      identical <- T
      local_status <- "up to date with"
    }
  } else {
    #' *If the files aren't identical, declare whether the local or the gdrive is ahead*
    identical <- F
    local_status <- ifelse(mtime_match > 0, "behind", "ahead of")
    gdrive_raw <- NULL
    
    if( local_status == "behind" ){
      #' *If the local is behind, check to see if the local_mtime matches any prior gdrive versions*
      local_match_ver <- (sapply(g_path$revision_lst, "[[", "modifiedTime") == trunc(local_info$mtime))
      # If there is a match, print the version
      if( any(local_match_ver) ){
        cat(paste0(
          "Local copy of ", crayon::bold(l_path$name), " appears to be on ", 
          crayon::yellow(paste0("[ver", which(local_match_ver), "]")), 
          " whereas the Gdrive is on ", crayon::yellow(paste0("[ver", g_path$current_ver, "]")), ".\n"
        ))
      }
    }
    
    # Print the modified dates of the local and gdrive versions
    cat(paste0(
      "Modified datetimes of ", crayon::bold(l_path$name), ":\n- Local:  ", round(local_info$mtime),
      "\n- Gdrive: ", gdrive_head$modifiedTime, "\n"
    ))
  }
  
  # Outputs
  list(
    identical = identical,
    mtime_match = mtime_match,
    local_status = local_status,
    gdrive_bytes = gdrive_raw$content
  )
}

#======================================================================================================================#

#' *Uploads files to the specified directory in the shared Gdrive.*
gdrive_upload <- function(local_path, gdrive_dribble) {
  # 'local_path' is the path to the local file and must include the file extension.
  # 'path' is the dribble of the Gdrive where you want to upload the file to.

  # Parse the local path to get the directory, file name, extension, and whether a version flag exists
  l_path <- parse_local_path(local_path)
  # If the local path doesn't exist, abort!
  if( !l_path$local_exists ) stop(paste0(crayon::cyan(local_path), " cannot be found."))
  # Prevent uploading files with a version suffix.
  if( l_path$ver_flag ) stop("Do not upload files with a version suffix!")
  # Parse the path to the google drive, looking for the files specified in local_path
  g_path <- parse_dribble(gdrive_dribble, l_path)
  
  # Get the modify time of the local file
  local_mtime <- file.info(local_path)$mtime
  # convert this to the format Google wants
  new_mtime <- paste0(sub("(?<=[0-9])( )(?=[0-9])", "T", format(local_mtime, tz = "GMT"), perl = T), ".000Z")
  
  # Either upload or update the file
  if( nrow(g_path$gdrive_item) == 0 ) {
    #' *Upload File - If the file does not yet exist on the gdrive, upload the file to the gdrive*
    cat(paste0(
      crayon::cyan(local_path), " will be uploaded to ", crayon::yellow(gdrive_dribble$path),
      " as ", crayon::yellow("[ver1]"), ".\n"
    ))
    upload_response <- toupper(rstudioapi::showPrompt(
      title = "Notice!",
      message = "Proceed with upload? (Y/N)"
    ))
    if( is.null(upload_response) ) {
      stop("Aborting upload.")
    } else if( upload_response == "Y" ){
      # Upload the file. Make the modifiedTime match that of the local file
      googledrive::drive_upload(
        media = local_path, path = gdrive_dribble, overwrite = FALSE, 
        modifiedTime = new_mtime, keepRevisionForever = TRUE
      )
    } else if ( upload_response == "N" ){
      return("Aborting upload.")
    } else {
      stop("Aborting upload. Response was not either 'Y' or 'N'.")
    }
  } else {
    #' *Update File - If the file already exists on the gdrive, and the local is ahead, update the file*
    
    # Compare the file sizes and modified dates of the local file and the head gdrive file
    compare_res <- compare_local_and_gdrive2(l_path, g_path)
    
    # Decide whether to update the existing file
    if( compare_res$identical ) {
      #' *If the files are identical, don't bother updating!*
      return(cat(paste0(
        "Local is ", crayon::bold(compare_res$local_status), " the Gdrive. Skipping upload.\n"
      )))
    } else {
      
      if( compare_res$local_status == "behind" ){
        #' * If local is behind, prompt the user to check before proceeding*
        local_behind_response <- toupper(rstudioapi::showPrompt(
          title = "Notice!",
          message = paste0(
            "Local copy of ", l_path$name, 
            " appears to be behind the Gdrive. Are you sure you want to continue with the upload? (Y/N)"
          )
        ))
        if( is.null(local_behind_response) ){
          return("Aborting upload.")
        } else if( local_behind_response == "N" ){
          return("Aborting upload.")
        } else if( local_behind_response != "Y" ){
          return("Response was not 'N' or 'Y'. Aborting upload.")
        }
      } 
      
      # Prepare to update
      cat(paste0(
        crayon::bold(l_path$name), " in ", crayon::yellow(gdrive_dribble$path), " will be updated to ", 
        crayon::yellow(paste0("[ver", g_path$current_ver + 1,  "]")), ".\n"
      ))
      update_response <- toupper(rstudioapi::showPrompt(
        title = "Notice!",
        message = "Proceed with upload? (Y/N)"
      ))
      if( is.null(update_response) ){
        stop("Aborting upload")
      } else if ( update_response == "N"){
        stop("Aborting upload.")
      } else if ( update_response != "Y") {
        stop("Aborting upload. Response was not either 'Y' or 'N'.")
      } else if( update_response == "Y" ){
        # Perform the update, assigning the modified time of the file
        googledrive::drive_update(
          file = g_path$gdrive_item, media = local_path, modifiedTime = new_mtime, keepRevisionForever = TRUE
        )
      }
    }
  }
}

#======================================================================================================================#

#' *Downloads files to the specified directory in the shared Gdrive.* 
#' Checks to make sure that the local data file exists and is up-to-date with the Gdrive, and otherwise skips the 
#' download. Be default (when ver = NULL), downloads the most recent version. If `ver` is a length 1 numeric, it will
#' download a new instance of the file with a version suffix (e.g, _v000) added to the file name.
gdrive_download <- function(local_path, gdrive_dribble, ver = NULL) {
  # `local_path` is the local path to where you want to save the file and contains the name of the file.
  # `gdrive_dribble` is the dribble of the folder containing the file you want to pull
  
  # Make sure ver, if specified, is numeric
  if( !is.null(ver) ) if ( !is.numeric(ver) ) stop("Version number 'ver' needs to be numeric!")
  
  # Parse the local path to determine whether the file exists locally and what to search for on the Gdrive.
  l_path <- parse_local_path(local_path)
  # Parse the Gdrive path, using l_path to determine what to search for in the Gdrive.
  g_path <- parse_dribble(gdrive_dribble, l_path)
  
  # Can the directory in local_path be found?
  if( !file_test(op = "-d",  l_path$directory) ) {
    stop(paste0("Local path, ", crayon::bold(l_path$directory), ", not found."))} 
  
  if( is.null(ver) ){
    #' *Downloading the most recent version*
    if( l_path$local_exists ){
      # If a local version already exists, compare it with the gdrive version
      compare_res <- compare_local_and_gdrive2(l_path, g_path)
      
      if( compare_res$local_status %in% c("up to date with", "ahead of") ){
        #' *If the local is ahead or up to date, skip the download*
        
        return(cat(paste0(
          "Local copy of ", crayon::bold(l_path$name), " is ", crayon::green(compare_res$local_status), 
          " the Gdrive. Skipping download.\n"
        )))
      } else if( compare_res$local_status == "behind" ){
        #' *If the local is behind, prompt to download and overwrite to bring the local version up to date*
        cat(paste0("Local version of ", crayon::bold(l_path$name), " is ", crayon::red(compare_res$local_status), " the Gdrive!\n"))
        download_response <- toupper(rstudioapi::showPrompt(
          title = "Notice!", 
          message = paste0(
            "Overwrite and update your local version of ", l_path$name, " to ", "[ver",
            g_path$revision_lst[[1]]$version, "]", "? (Y/N)"
          )
        ))
        if( is.null(download_response) ){
          return(cat(paste0("Aborting download of ", crayon::bold(l_path$name), ".")))
        } else if( toupper(download_response) == "N" ) {
          return(cat(paste0("Aborting download of ", crayon::bold(l_path$name), ".")))
        } else if ( toupper(download_response) != "Y") {
          stop("You didnt enter `Y` or `N`! Aborting download.")
        }
      }
    } else {
      #' *If a local version does not exist*
      cat(paste0(
        "No local copy of ", crayon::bold(l_path$name), " found. Downloading the most recent version: ", 
        crayon::yellow(paste0("[ver", g_path$current_ver, "]")), ".\n"
      ))
      download_message <- paste0(
        "Overwrite and update your local version of ", l_path$name, " to ", "[ver", 
        g_path$current_ver, "]", "? (Y/N)"
      )
    }
    #' *Download the most recent version and set the modified time.*
    googledrive::drive_download(file = g_path$gdrive_item, path = local_path, overwrite = T)
    Sys.setFileTime(local_path, g_path$revision_lst[[g_path$current_ver]]$modifiedTime)
    
  } else {
    #' *Downloading a prior version*
    
    # check to see if the file already exists
    ver_path <- paste0(
      l_path$directory, l_path$name_no_ext, "_v", formatC(ver, width = 3, digits = 0, flag = "0" ), l_path$extension 
    )
    
    if( !file_test(op = "-f", ver_path) ){
      #' *If the file doesn't exist, download it*
      
      # Grab the desired version
      if( !(ver %in% seq_along(g_path$revision_lst)) ){
        stop(cat(paste0(
          "No ", crayon::yellow(paste0("[ver", ver, "]")), " of ", crayon::bold(l_path$name), 
          " exists! Currently on ", crayon::yellow(paste0("[ver", g_path$current_ver, "]")), ". Aborting download.\n"
        )))
      } else {
        
        # Subset the revision list to the desired revision
        revision_i <- g_path$revision_lst[[ver]]
        
        # If the version is found, download it as raw bytes
        revision_raw <- gargle::request_make(gargle::request_build(
          method = "GET",
          path = "drive/v3/files/{fileId}",  
          params = list(
            fileId = g_path$gdrive_item$id, revisionId = revision_i$id, supportsAllDrives = TRUE, alt = "media"
          ),
          token = googledrive::drive_token()
        ))
        # Write the file the local path and set its modified date
        cat(paste0(
          "Downloading ", crayon::yellow(paste0(l_path$name, " [ver", ver, "]")), " as ", crayon::cyan(ver_path), ".\n"
        ))
        writeBin(revision_raw$content, con = ver_path)
        Sys.setFileTime(ver_path, revision_i$modifiedTime)
      }
    } else {
      #' *If the file already exists, skip the download*
      #' TODO `Could also check to make sure that the local version and the gdrive version matches? Perhaps overkill?`
      return(cat(paste0(crayon::cyan(ver_path), " already exists locally. Skipping download.\n")))
    }
  }
}

#======================================================================================================================#

#' *View the revision history of a file on the Gdrive*
gdrive_versions("new_data_test2.rdata", v2_dribble)