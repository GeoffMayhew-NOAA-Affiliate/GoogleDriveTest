# Author: Geoff Mayhew
# Created: 2023-Dec-20

library(data.table)
library(googledrive)  # https://googledrive.tidyverse.org/
library(crayon)       # Allows easy formatting of text in console

# First, need to register your NOAA e-mail to authorize access to gdrive via googledrive package
drive_auth()









# googledrive::as_dribble()  # This is where stuff is saved

# link to Geof's Ancillary folder in the 2024 ADP google drive
# https://drive.google.com/drive/folders/1aWgsxLcSfh2dxnceg_8FNf0xVB81my1t
# sandbox.Rdata is something we can test. See if we can load it here and save it locally in the Data folder

# drive_download(
#   file = as_id("https://drive.google.com/file/d/1WRUZaWWvmFMQyLXJRCZ6AkBvYMaVHoTT/view?usp=drive_link"), 
#   path = "Data/sandbox.Rdata")  
# This made a browser window open asking me to grant accesss to Tidyverse API Packages for my google account.
# For now leaving the 'see, edit, create, and delete all of your google drive files' box unchecked, but clicking
# continue below.

# I got:

# Waiting for authentication in browser...
# Press Esc/Ctrl + C to abort
# Authentication complete.
# Error in `map()`:
#   ℹ In index: 1.
# Caused by error in `.f()`:
#   ! Client error: (403) Forbidden
# Request had insufficient authentication scopes.
# PERMISSION_DENIED
# • message: Insufficient Permission
# • domain: global
# • reason: insufficientPermissions
# Run `rlang::last_trace()` to see where the error occurred.

# Might have to check the box to let things run.
# However, now I'm getting errors:

# Error in `map()`:
#   ℹ In index: 1.
# Caused by error in `.f()`:
#   ! Client error: (403) Forbidden
# Request had insufficient authentication scopes.
# PERMISSION_DENIED
# • message: Insufficient Permission
# • domain: global
# • reason: insufficientPermissions

# ?drive_auth_configure()



# Storing user data 
# These packages may store your credentials on your local machine, for later reuse by you. Use caution when using these packages on a shared machine.
# 
# By default, an OAuth token is cached in a local file, such as ~/.R/gargle/gargle-oauth. See the documentation for gargle::gargle_options() and gargle::credentials_user_oauth2() for information on how to control the location of the token cache or suppress token caching, globally or at the individual token level.
# 

# Use this function to get prompted to give permissions again.
# drive_auth()

# # trying this again..
# drive_download(
#   file = as_id("https://drive.google.com/file/d/1WRUZaWWvmFMQyLXJRCZ6AkBvYMaVHoTT/view?usp=drive_link"), 
#   path = "Data/sandbox.Rdata")  
# # Cool, downloaded! This has a current version and Version 1, which is older.
# 
# 
# # Get metatadata for a folder (I'm looking at my local data folder)
# a <- googledrive::as_dribble("Data/")
# a$id
# a$drive_resource[[1]]
# a$drive_resource[[1]]$owners
# a$drive_resource[[1]]$shared

# Putting the google drive's url in here
# a1 <- googledrive::as_dribble(as_id("https://drive.google.com/file/d/1WRUZaWWvmFMQyLXJRCZ6AkBvYMaVHoTT/view?usp=drive_link"))
# a1
# 
# # Let's try to put data up on the drive first
# 
# # Here is the url to my ancillary folder (Geoff's Notes)
# # https://drive.google.com/drive/folders/1aWgsxLcSfh2dxnceg_8FNf0xVB81my1t?usp=drive_link
# 
# # Make an object (copy mtcars)
# data <- copy(mtcars)
# # Save the file as an Rdata file
# save(data, file = "Data/test_data.rdata")
# # Upload it to the google drive
# drive_upload(
#   "Data/test_data.rdata",
#   path = as_id("https://drive.google.com/drive/folders/1aWgsxLcSfh2dxnceg_8FNf0xVB81my1t?usp=drive_link"),
#   name = "test_data.rdata"
#   )  #overwrite = TRUE is the default
# 
# 
# 
# 
# drive_find()  # digs through all the files you have in google drive!
# 
# drive_get() # use this to find a folder.
# 
# fma_analysis_group_dribble <- drive_get("FMA Analysis Group/")
# fma_analysis_group_dribble$drive_resource[[1]]
# 
# # Get the names of items in a dribble
# 
# drive_ls(fma_analysis_group_dribble)
# 
# # Make a new dribble for vision 2024 ADP data folder. This might take a a few seconds, depends on what you have in your google drive?
# folder.dribble <- drive_get("FMA Analysis Group/Vision 2024 ADP/Documents/Ancillary Documents/Geoff's Notes")
# 
# drive_ls(folder.dribble)
# # Now try uploading to this dribble?
# drive_upload(
#   "Data/test_data.rdata",
#   path = folder.dribble,
#   name = "test_data.rdata"
# ) 
# # Cool, now it's up there! 
# 
# # Now let's try updating it...
# data$RN <- runif(n = nrow(data))
# # Overwrite the local version
# save(data, file = "Data/test_data.rdata")
# # Upload, overwriting
# drive_upload(
#   "Data/test_data.rdata",
#   path = folder.dribble,
#   name = "test_data.rdata",
#   overwrite = TRUE  # Default is NA, which would allow multiple files with the same name!
# )    
# # File trashed:
# #   • test_data.rdata <id: 1YQL7B1cg5DA99KrR1nvRTRcc2H9xI73D>
# #   Local file:
# #   • Data/test_data.rdata
# # Uploaded into Drive file:
# #   • test_data.rdata <id: 1XHFhFJSEXU8zi1k7XDaDckF0M3ssm-I8>
# # Overwrite actually TRASHES the old version rather than overwriting it!
# 
# # Instead, use 
# ?drive_update()
# data$RN2 <- runif(n = nrow(data))
# # Overwrite the local version
# save(data, file = "Data/test_data.rdata")
# drive_update(
#   file = paste0(folder.dribble$path, "test_data.rdata"),
#   media = "Data/test_data.rdata",
# ) 
# # File updated:
# #   • test_data.rdata <id: 1XHFhFJSEXU8zi1k7XDaDckF0M3ssm-I8>
# 
# # but it looks like the id is the same as the old one?
# 
# # Cool, this now has 'current version' and 'Version 1`. Now let's see if we can access the old version
# # TODO I did notice that the 'keep forever' box is not checked for this file!
# 
# # Knowing the files's 'id' is the most important thing! 
# 
# my_data_file <- drive_get(as_id("1XHFhFJSEXU8zi1k7XDaDckF0M3ssm-I8"))
# my_data_file$drive_resource[[1]]
# my_data_file$drive_resource[[1]]$version   #Not sure what this 'version' is. I only have 2 versions
# my_data_file$drive_resource[[1]]
# 
# # ? How to change keep_forever to TRUE (won't need this if don't have 'versions' of the same thing.)
# # ? How to access older versions of a file via google drive! (can't do this)
# 
# 
# drive_link(my_data_file)  # here is the current version link


# There isn't a convenient way to get versions from google drive. In any case it's probably best to just have different 
# versions of the data published, whether with a date or otherwise.
# data_2022v01, data_2022v02, etc? Shouldn't ever have more than 99 versions for a year. 
# All data files should be named and dated

# TODO Make a protocol or a function that saves these items, gives the google drive link automatically?
# -- Make sure that you never create files with duplicates names or overwrite unless specifically desired! Just make a new version! 
# TODO Figure out how to use functions specific to Google Shared Drives when that is up and running.


# drive_find("Ancillary Documents/Geoff's Notes")


# drive_get is good when you know exaccly where something is (searches a path or id link). Drive_find (searches by pattern) is more of a tool when you're not sure.
?drive_get()

# very fast
fma_analysis_group_dribble <- drive_get("FMA Analysis Group/")     # this is our current gdrive (non-shared!)
# takes a few seconds
folder.dribble <- drive_get("FMA Analysis Group/Vision 2024 ADP/Documents/Ancillary Documents/Geoff's Notes/") 

# Can also use shared_drive_get?
# https://sites.google.com/noaa.gov/myafsc/technology/google-shared-drives?pli=1
# 'shared_drive' and 'corpus' allows us to work with shared drives specifically?





#========================#
# GOOGLE SHARED DRIVE ####
#========================#

# FMA Analytical Services Program
# Made a folder called 'Google Drive Test'

fma_gdrive_dribble <- shared_drive_get(name = "FMA Analytical Services Program")
fma_gdrive_dribble  # This id should be stable for everyone, can probably be saved in our functions as well!

drive_link(fma_gdrive_dribble)  # returns URL for the folder
drive_ls(fma_gdrive_dribble)  # Lists all files within a dribble (and their IDs)

# Create some data
data_test <- copy(mtcars)
# Now save it locally
save(data_test, file = "Data/data_test.Rdata")

# Make a function to save it to our google drive test. Make a wrapper for drive_upload
a <- drive_ls(fma_gdrive_dribble)
gdrive_test_dribble <- a[a$name == "Google Drive Test",]
gdrive_test_dribble




#======================================================================================================================#


# Get the name of the file (remove any folder names)
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


# A helper function to parse a local path into its directory, file name, and file extension
parse_local_path <- function(local_path){
  
  # Error checks
  # local_path must be a length 1 character string
  if( length(local_path) != 1 | !is.character(local_path) ) stop("`local_path` must a length = 1 character string.")
  # Can a file be found at the specificed local path?
  if( !file_test(op = "-f",  local_path) ) stop(paste0(bold(local_path), " not found."))
  
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
  list(name = name, directory = directory, name_no_ext = name_no_ext, extension = extension, ver_flag = ver_flag )
  
}
parse_local_path("Data/data_test.Rdata")


# A helper function to parse the google drive path, using the outputs of parse_local_path() to determine which files
# to search for.
parse_gdrive_path <- function(gdrive_path, l_path) {
  # gdrive_path <- copy(gdrive_test_dribble); l_path <- parse_local_path(local_path)
  
  # Identify all folders/files in the gdrive path
  gdrive_path_items <- drive_ls(gdrive_path)
  
  # If l_path has a version number, find that item specifically, Otherwise, match by name.
  if( l_path$ver_flag ) {
    
    # If a version flag is in l_path, does the specified version exist in the gdrive?
    ver_exists <- l_path$name == gdrive_path_items$name
    if( sum(ver_exists) == 1 ){
      gdrive_file <- gdrive_path_items[ver_exists, ]
    } else if( sum(ver_exists) > 1) {
      stop(paste0("File name is not unique!"))
    } else {
      stop(cat(paste0(bold(l_path$name), " not found in ", bold(gdrive_path$name))))
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


# This function uploads files to the specified directory in the shared Gdrive
drive_upload_fma <- function(local_path, gdrive_dribble) {
  # 'local_path' is the path to the local file and must include the file extension.
  # 'path' is the dribble of the gdrive where you want to upload the file to 

  
  # local_path <- "Data/data_test.Rdata"; gdrive_dribble <- gdrive_test_dribble
  
  # FOR TESTING
  # char_vec <- c(local_path, "file/missing_extension.txt", "file/missing_extension.", "file/.missingextension", "file/missing_extension", "file/something.other.txt", "file.txt")
  
  # Parse the local path to get the directory, file name, extension, and whether a version flag exists
  l_path <- parse_local_path(local_path)
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
    cat(paste0(nrow(g_path$files), " instance(s) of ", bold(l_path$name), " found.\n"))
    cat(paste0("The most recent version is ", bold(most_recent$name), ", uploaded on ", g_path$c_date[1], ".\n\n" ))
    
    # Check to see if the latest version on the gdrive is identical to the local version
    most_recent_raw <- drive_read_raw(most_recent)
    # Read the local version
    data_raw_local <- readBin(local_path, what = "raw", n = length(most_recent_raw))
    if( identical(most_recent_raw, data_raw_local) ) {
      # If identical, prompt user if they still want to proceed
      switch(
        menu(
          choices = c("Y", "N"), 
          title = paste0(
            bold(local_path), " appears to be identical to ", bold(most_recent$name), ". Proceed with upload? (Y/N)"
          )),
        # Proceed with upload
        invisible(),
        # Abort the upload
        return(cat("Upload to Gdrive aborted."))
      )
    }
    
  } else {
    # If this name does not exist, assign it a version number starting with _v000
    gdrive_file_name <- paste0(l_path$name_no_ext, "_v000", l_path$extension)
  }

  # Confirm name and location of upload
  switch(
    menu(choices = c("Y", "N"), title = paste0(
      bold(local_path), " will be uploaded as ", bold(gdrive_file_name), " to ", bold(gdrive_dribble$name), ". Proceed? (Y/N)"
    )),
    # Proceed with upload
    drive_upload(media = local_path, path = gdrive_dribble, name = gdrive_file_name, overwrite = FALSE),
    # Abort
    return(cat("Upload to Gdrive aborted."))
  )
}

# First, specify which folder to upload to 
fma_gdrive_dribble <- shared_drive_get(name = "FMA Analytical Services Program")
# You can subset this to get a single ID
gdrive_test_dribble <- drive_ls(fma_gdrive_dribble) %>% dplyr::filter(name == "Google Drive Test")
# Upload the data file
drive_upload_fma(local_path = "Data/data_test.Rdata", gdrive_dribble = gdrive_test_dribble)

#======================================================================================================================#



# A function to download from our Shared Google Drive


#' *[RULE]*  `The local versions are considered 'development' versions and should not have the _v### tags. It is meant to`
#' `be up-to-date with the most recent gdrive version. However, if specified, you may want to download a prior version`
#' `to run an older version of an analysis (you will save a _v### version locally`
 
# view items in the 'Google Drive Test' folder
drive_ls(drive_get(as_id("1CUIExr_6csLMzZGZjRduUZtSepxFAcYC")))
# Note that you can get the parent folder for a file/folder from drive_resource[[1]]parents

# TODO make flow charts of these decision trees!

# This function 

gdrive_download <- function(local_path, gdrive_dribble) {

  # TODO If do intentionally have a local _v### version, make sure this function works the same way (don't add another _v### in the search)
  # TODO Is downloading files into a temporary folder ever useful?
  
  # If local_path has a filename with the suffix "_v###", then that file will be downloaded directly. 
  
  #====================================================================================================================#
  #' * TEST VARIABLES *
  # local_path <- "Data/data_test.Rdata"; gdrive_dribble <- drive_get(as_id("1CUIExr_6csLMzZGZjRduUZtSepxFAcYC"))
  # local_path <- "Data/data_test_v003.Rdata"; gdrive_dribble <- drive_get(as_id("1CUIExr_6csLMzZGZjRduUZtSepxFAcYC"))
  # char_vec <- c(media, "file/missing_extension.txt", "file/missing_extension.", "file/.missingextension", "file/missing_extension", "file/something.other.txt", "file.txt", "one/two/three/file.Rdata")
  #====================================================================================================================#
  
  # `local_path` is the local path to where you want to save the file and contains the name of the file.
  # `gdrive_path` is the dribble of the folder containing the file you want to pull
  # `ver` is optional, and allows you to specify which version of a file to load

  # Get the name of the file (remove any folder names)
  name <- sub("^.*[/]", "", local_path)
    
  # Input checks
  if( !file_test(op = "-d",  sub(name, "", local_path)) ) {
    stop(paste0("Local path, ", bold(sub(name, "", local_path)), ", not found."))} 
  if( !is_dribble(gdrive_dribble) ) stop("The Gdrive location, 'gdrive_dribble', must be specified as a dribble.")
  
  # Pulled from drive_upload() - make this step a function?
  # Identify the extension from `media`. 
  if( grepl(pattern = "^(.+)[.?](.+)$", x = name) ) {
    # If an extension is found, grab it
    extension <- sub(pattern = "^(.*)(?=[.])", "", name, perl = T)  # Omit everything to the left of the last period
  } else {
    # If an extension is not found, throw an error
    stop(paste0("'local_path' needs to have the file extension specified."))
  }

  # Check to see if a local version of the file already exists.
  local_exists <-  file_test(op = "-f",  local_path)
  
  # Get the file name without the extension
  name_no_ext <- sub(extension, "", name)
  
  # Get the dribble and names of all folders and files in google drive path
  gdrive_path_items <- drive_ls(gdrive_dribble)
  
  # Determine which version to download
  # Does a suffix exist? If so, target the file directly. Otherwise, target the most recent version.
  if( grepl("(.+)(?=_v[0-9]{3}[.])", name, perl = T) ){
    # Download a specified version
    
    # Does the specified version exist?
    ver_exists <- name == gdrive_path_items$name
    
    if( any(ver_exists) ){
      if( sum(ver_exists) > 1 ){
        stop(paste0("File name and version is not unique!"))
      } else {
        
        if( local_exists ){
          # If a local copy exists, make sure it is identical!
          
          gdrive_raw <- drive_read_raw(gdrive_path_items[ver_exists, ]$id)
          local_raw <- readBin(local_path, what = "raw", n = length(gdrive_raw))
          ver_identical <- identical(gdrive_raw, local_raw)
          
          if( ver_identical ){
            # If the local copy is identical, skip the download
            return(cat(paste0(bold(name), " found locally. Skipping download.\n")))
          } else {
            # If the local version differs from the Gdrive version, this is a bad thing that should not happen! In such
            # a case, we should be trusting our Gdrive version, so delete your local copy and retry.
            stop(paste0(
              "A local version of ", bold(name), " already exists but differs from the Gdrive's version!"
            ))
          }
          
        } else {
          # If a local copy doesn't exist, download it!
          drive_download(
            file = gdrive_path_items[ver_exists, ], 
            path = local_path,
            overwrite = F
          )
        }
      }
    } else stop(paste0("File ", bold(name_with_ver), " not found at the specified location on the Gdrive."))

  } else {
    # Download the most recent version
    
    # Subset Gdrive folder to items with `name` and version number
    gdrive_path_items <- gdrive_path_items[grepl(paste0("^", name_no_ext , "_v[0-9]{3}", extension, "$") , gdrive_path_items$name), ]
    if( nrow(gdrive_path_items) == 0) stop(paste0("No files found with file name like ", bold(name), "!"))
    
    # Find the latest version on the gdrive. Create dates are the dates that the items were uploaded to the drive, 
    # not the create dates of the files that were uploaded. Therefore, if you make new data file and then upload it,
    # the google version's create date will ALWAYS be > local create date.
    create_dates <- as.POSIXlt(sapply(gdrive_path_items$drive_resource, "[[", "createdTime"), format = "%Y-%m-%dT%H:%M:%OS", tz = "GMT" )
    # Ensure the order of gdrive_path_items is most recent first
    gdrive_path_items <- gdrive_path_items[order(create_dates, decreasing = T), ]
    most_recent <- gdrive_path_items[1, ]

    if( local_exists ){
      # If a local version already exists, check to see if the local version is different from the most recent version
      
      # Get size and modified date of the local file
      local_file_info <- file.info(local_path)
      
      # Test to see if the local file is identical to the most recent gdrive version
      if( most_recent$drive_resource[[1]]$size == local_file_info$size ) {
        # If the file sizes are identical, do a check to see if the files are in fact identical
        gdrive_raw <- drive_read_raw(most_recent$id)
        local_raw <- readBin(local_path, what = "raw", n = length(gdrive_raw))
        recent_identical <- identical(gdrive_raw, local_raw)
      } else recent_identical <- F
      
      # Compare dates of local version and gdrive versions. Make a vector, comparing each version
      local_more_recent <- local_file_info$mtime >= create_dates 
      
      # If a local exists, is not identical to and older than the most recent gdrive version, check to see which version exists locally
      if( !recent_identical & local_file_info$mtime <max(create_dates) ) {
        warning("TODO : Have not tested this case yet! The local version is outdated?\n")
        size_match <- which(as.numeric(sapply(gdrive_path_items$drive_resource, "[[", "size" )) == local_file_info$size)
        for(i in size_match) {
          if ( identical(local_raw, drive_read_raw(gdrive_path_items$id[i])) ) {
            cat(paste0("Local ", bold(name), " matches gdrive version ", bold(gdrive_path_items$name[i]), " but ", bold(most_recent$name), " is most recent.\n" ))
            break
          }
        }
      }
    } else {
      recent_identical <- F
      local_more_recent <- F
    }
      
    # Determine whether to proceed with download
    if( recent_identical ){
      return(cat(paste0(bold(name), " is up-to-date with the Gdrive. Skipping download.\n")))
    } else if( local_more_recent[1] ){
      return(cat(paste0(bold(name), " is ahead of the Gdrive. Skipping download.\n")))
    } else if( local_exists ) {
        # If a local version exists, ask the user whether to overwrite with most recent Gdrive version.
        switch(
          menu(choices = c("Y", "N"), title = cat(paste0("Download ", bold(most_recent$name), " and overwrite ", bold(name), "?\n" ))),
          # Proceed with download, overwriting the existing file
          drive_download(file = most_recent, path = local_path, overwrite = T),
          # 
          return(cat(paste0("Download of ", bold(name), " aborted.\n")))
        )
    } else {
      # If adrive_download(file = most_recent, path = local_path, overwrite = T) local version does not exist, just download as requested.
      drive_download(file = most_recent, path = local_path, overwrite = F)
    }
  }

}

# Download the most recent version
gdrive_download(local_path = "Data/data_test.Rdata", gdrive_dribble = drive_get(as_id("1CUIExr_6csLMzZGZjRduUZtSepxFAcYC")) )

# This is a little more explicit to someone else where the file is pulling from. Just takes a second to search the shared drive.
gdrive_download(
  local_path = "Data/data_test.Rdata", 
  gdrive_dribble = drive_get(path = "Google Drive Test", shared_drive = "FMA Analytical Services Program") )

# Download a specific version
gdrive_download(local_path = "Data/data_test_v003.Rdata", gdrive_dribble = drive_get(as_id("1CUIExr_6csLMzZGZjRduUZtSepxFAcYC")) )



#======================================================================================================================#


# This is a helper function for gdrive_folders() that checks for subfolders within folders
gdrive_folders_search <- function(dribble) {
  
  # dribble <- copy(fma_gdrive_dribble)
  # dribble <- copy(new_parent)
  # dribble <- copy(parent_search[[1]]$child)
  
  dribble_lst <- vector(mode = "list", length = nrow(dribble))
  
  # Keep all parent folders
  parent <- as.data.table(dribble)[, c("name", "id")]

  for( i in 1:length(dribble_lst) ) {
    
    drive_items <- drive_ls(dribble[i, ])
    
    if( nrow(drive_items) == 0 ) {
      dribble_lst[[i]] <- list(
        parent = parent[i,],
        child = NULL
      )
    } else {
      
      x1 <- as.data.table(drive_items)
      # check which items are folders
      x1_check <- sapply(x1$drive_resource, function(x) x$mimeType %like% ".folder")

      # Make new child folder names and their ids
      child <- x1[x1_check]
      for(j in 1:nrow(child)) {
        child[j, "name"] <- paste0(parent[i, name], "/", child[j, name])
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

# The function looks up and displays the folder structure of a shared drive
gdrive_folders <- function(id = c("Analytics")) {
  # Search the folder structure of the FMA shared google drive
  
  # Hard code ids using an alias
  if( id == "Analytics") {
    id <- "0AJcHJWlPgKIgUk9PVA"
  } else stop("")
  
  # Get the dribble of the shared drive
  gdrive_dribble <- shared_drive_get(id = id)
  parent <- copy(gdrive_dribble)
  parent_search <- gdrive_folders_search(parent)

  #while( !is.null(parent_search$child) ) {
  while( nrow(parent_search$child) > 0 ) {
    new_parent <- parent_search$child
    new_parent_search <- gdrive_folders_search(new_parent)
    parent_search <- list(
      parent = rbind(parent_search$parent, new_parent_search$parent),
      child = new_parent_search$child
    )
  }
  
  folders_dt <- parent_search$parent
  # Omit the shared folder from the table and names
  folders_dt <- folders_dt[name != folders_dt [1, name]][, name := gsub(paste0(folders_dt[1, name], "/"), "", name)][]
  
  # Print the results
  cat(gdrive_dribble$name, "\n")
  print(folders_dt [order(name)], justify = "left")
  
}

# Look up all the files in our shared drive! By default, it will check our Analytics shared folder
# TODO adding a column that has the count of files in it might be useful?
gdrive_folders()
# Now that you know the names and ids of all folders, you can draw the dribble quickly using drive_get
# Find it using the file path, specifying the shared drive
drive_get(path = "Google Drive Test/Subfolder Test", shared_drive = fma_gdrive_dribble)  
# Or more easily, directly using the folder id. 
drive_get(id = "1S1xckVrEQ66ny3vfAfRAtoCpL8VvVX2A")



#======================================================================================================================#




# TODO Make a tool to pull stuff out of the drive and save it locally (or even temporarily???)
#' *Make it lookup the created date and reference the local created date*

file.info("Data/test_data.rdata")  # ctime is create time, mtime is modified time (on the local, the most recent change, atime is accessed time)

# Todo make it check to see what version the local one is
?drive_download()
a <- drive_get(id = "1CUIExr_6csLMzZGZjRduUZtSepxFAcYC")
a1 <- drive_ls(a)
a1[3, ]$drive_resource[[1]]$createdTime
a1[3, ]$drive_resource[[1]]$modifiedTime  #In our case, create and modified should always be the same (never modify)

# Can I modify the existing data_test_v0001.Rdata? (doing this manually)
# drive_upload(media = "Data/data_test.Rdata", name = "data_test_v001.Rdata", path = as_id("1CUIExr_6csLMzZGZjRduUZtSepxFAcYC"))
# # This just created another file with the same name! overwrite was NA by default!
# drive_upload(media = "Data/data_test.Rdata", name = "data_test_v001.Rdata", path = as_id("1CUIExr_6csLMzZGZjRduUZtSepxFAcYC"), overwrite = F)
# # With overwrite = F, got an error because we asked it to no overwrite any files
# drive_upload(media = "Data/data_test.Rdata", name = "data_test_v001.Rdata", path = as_id("1CUIExr_6csLMzZGZjRduUZtSepxFAcYC"), overwrite = T)
# # with multiple files, it doesn't know which one to overwrite!
# # Let's first upload a v002
# drive_upload(media = "Data/data_test.Rdata", name = "data_test_v002.Rdata", path = as_id("1CUIExr_6csLMzZGZjRduUZtSepxFAcYC"), overwrite = T)
# # And try overwriting it...
# drive_upload(media = "Data/data_test.Rdata", name = "data_test_v002.Rdata", path = as_id("1CUIExr_6csLMzZGZjRduUZtSepxFAcYC"), overwrite = T)
#' Nope, *we don't have permissions to overwrite anything! We have to be careful to maintain unique names in the same folder!*
# By using our custom upload functions, we will always have overwrite = F by default!


# TODO Make it so we can have the option to load data in a temporary drive rather than overwriting existing data file?

# TODO What kind of folder structure should we adopt for the shared Gdrive?



# TODO Create a function to add a new folder to the gdrive! Also must have have 'overwrite = F' hardcoded!
# To add a new folder use drive_mkdir()  

a1[3, ]

# Raw contents of a file
data_raw <- drive_read_raw(file = as_id("1SBnlGylQD4Fpgr3BbuhfZz7pS2h62GYl"))
# This may be a way to determine if files are different? Reading large files might take forever!!!

# I uploaded our 2024 ADP source data, took 5 minutse to manually upload in a browser window

drive_ls(as_id("1fJ1b9GiHoucNg0e78PqqXXAbIf6Ms1O6"))

# How does this this take
system.time(data_raw <- drive_read_raw(file = as_id("1rpK26KwiBkS_aHBEMa22MYOW4mu9NTi7")))
# Only about 6 seconds, not too shabby for a 250+Mb file
# Using the length of this file, we can specifiy how to use readBin for the local file


data_raw_local <- readBin("C:/Users/geoff.mayhew/Work/GitHub/ADP/source_data/2024_Draft_ADP_data.rdata", what = "raw", n = length(data_raw))
identical(data_raw, data_raw_local)  # cool, a perfect match! We have a way to test if files are identical!


?drive_reveal()
drive_reveal(as_id("1rpK26KwiBkS_aHBEMa22MYOW4mu9NTi7"))
test <- drive_get(id = as_id("1rpK26KwiBkS_aHBEMa22MYOW4mu9NTi7"))
drive_reveal(test, what = "created_time")
drive_reveal(test, what = "size")  # could also look at byte size instead of actual bytes?
drive_reveal(test, what = "mime_type")   #This may be an easy way to identify folders
?readBin()



to_read <- file("C:/Users/geoff.mayhew/Work/GitHub/ADP/source_data/2024_Draft_ADP_data.rdata", "rb")
readBin(to_read, what = "raw")


load("C:/Users/geoff.mayhew/Work/GitHub/ADP/source_data/2024_Draft_ADP_data.rdata")
