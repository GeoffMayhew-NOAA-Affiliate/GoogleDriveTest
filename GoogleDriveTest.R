# Author: Geoff Mayhew
# Created: 2023-Dec-20

# Make sure required packages are installed
source("functions.R")

# First, in order the run the underlying functions of the googledrive package, you need to register your NOAA e-mail to
# authorize access to gdrive via googledrive package. You will only have to do this once, and will only be asked to 
# renew your authorization as needed whenever googledrive functions are called.
googledrive::drive_auth()

# Look up the file structure of the FMA Analytical Services Program folder (or any other shared drive if specified)
gdrive_dir()

# Upload files to the shared google drive ####
gdrive_upload()

# Download files from the shared google drive ####
gdrive_download()


# TODO You can sync files to your computer using Google Derive for Desktop
# (https://support.google.com/a/users/answer/7212025?hl=en&ref_topic=12369809&sjid=11972867759425993847-NA)


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

# What is this finding?
what <- googledrive::drive_get("FMA/") 
what[1, ]$drive_resource[[1]]$webViewLink



# First, look at the directory
gdrive_dir()

# Let' say you want to access the third folder ("Yet a Third Folder"). You 
gdrive_dir()[3, path_name]

# Once you know what folder you want to target, you'll need to use the function gdrive_target() to create an object 
# that googledrive can interpret. We will identify it using the filepaths so that it clear to everyone where we will be
# uploading or downloading from.
my_target <- gdrive_target("Google Drive Test/Another Subfolder/Yet a Third Folder/")

# this object will be fed into gdrive_upload() or gdrive_download()

# If you want to see what items are in that folder, use gdrive_ls() with your target
# It will return the names of files (not the folders), the create dates, and the file sizes
gdrive_ls(my_target)



# Simply copy paste a row form 'name' to make that folder your target using gdrive_set_target(). This returns a dribble
# that you will use for gdrive_upload() and gdrive_download()
gdrive_set_target(path_name = "Google Drive Test/Another Subfolder/Yet a Third Folder/")
gdrive_set_target(path_name = "Google Drive Test")


gdrive_ls(gdrive_set_target(path_name = "Google Drive Test"))


gdrive_ls(gdrive_set_target(path_name = "Google Drive Test/Another Subfolder"))

gdrive_ls(path_name = "Google Drive Test/Another Subfolder")
gdrive_ls(path_name = "Subfolder Test")

gdrive_ls(path_name = "Yet a Third Folder")  # this works too, just not very specific. using id or a dribble wouuld be best

# The later functions really need a dribble, so make a function that ensures we return a 1-row dribble with its full path



# how quickly can we check whether a 252Mb file is identical?
my_target <- gdrive_set_dribble("Google Drive Test/Another Subfolder/Yet a Third Folder/")
local_path <- "C:/Users/geoff.mayhew/Work/GitHub/ADP/source_data/2024_Draft_ADP_data.rdata"
gdrive_dribble <- copy(my_target)
gdrive_upload(
  local_path,
  my_target
)

file.info(local_path)[c("mtime", "ctime")]

gdrive_download(local_path, gdrive_dribble)  #


#========================#
# GOOGLE SHARED DRIVE ####
#========================#

# FMA Analytical Services Program
# Made a folder called 'Google Drive Test'

fma_gdrive_dribble <- googledrive::shared_drive_get(name = "FMA Analytical Services Program")
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





# First, specify which folder to upload to 
fma_gdrive_dribble <- googledrive::shared_drive_get(name = "FMA Analytical Services Program")
# You can subset this to get a single ID
gdrive_test_dribble <- drive_ls(fma_gdrive_dribble) %>% dplyr::filter(name == "Google Drive Test")
# Upload the data file
drive_upload_fma(local_path = "Data/data_test.Rdata", gdrive_dribble = gdrive_test_dribble)


gdrive_test_dribble <- gdrive_set_dribble(gdrive_path = "Google Drive Test/")
# Modify the data and then overwrite the local copy
data_test <- cbind(data_test, new_col = F)
save(data_test, file = "Data/data_test.Rdata")
# Upload the modified local, bringing the Gdrive up to date
gdrive_upload(local_path = "Data/data_test.Rdata", gdrive_dribble = gdrive_test_dribble)


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



# UPload something new

my_test_data <- copy(airquality)
gdrive_dir()
gdrive_sub_dribble <- gdrive_set_dribble("Google Drive Test/Subfolder Test/")
save(my_test_data, file = "Data/air_quality.rdata")
# now upload it
gdrive_upload(local_path = "Data/air_quality.rdata", gdrive_dribble = gdrive_sub_dribble)

# Local modified time
file.info("Data/air_quality.rdata")
# gdrive modified time

# Get the dribble of my air quality data
gdrive_aq <- googledrive::drive_ls(googledrive::drive_get(gdrive_sub_dribble$id ))
gdrive_aq$drive_resource[[1]]$modifiedTime
gdrive_aq$drive_resource[[1]]$createdTime
googledrive::drive_reveal(gdrive_aq, what = "published")$revision_resource[[1]]$modifiedTime


# lets modifiy it any see what happens
my_test_data <- cbind(my_test_data, new_col = "poopoo")
save(my_test_data, file = "Data/air_quality.rdata")
gdrive_upload(local_path = "Data/air_quality.rdata", gdrive_dribble = gdrive_sub_dribble)

# use drive_update() to update the metadata of a drive file?
gdrive_aq <- googledrive::drive_ls(googledrive::drive_get(gdrive_sub_dribble$id ))
gdrive_aq[1, ]$drive_resource[[1]]
#' From the repo under drive_upload: 
#' *params <- toCamel(list2(...))*
googledrive:::toCamel(list2(...))

#' *from drive_upload help:, '...' argument*
#' Named parameters to pass along to the Drive API. Has dynamic dots semantics. You can affect the metadata of the 
#' target file by specifying properties of the Files resource via .... Read the "Request body" section of the Drive 
#' API docs for the associated endpoint to learn about relevant parameters.


gdrive_aq[1, ]$drive_resource[[1]]$modifiedTime
class(gdrive_aq[1, ]$drive_resource[[1]]$modifiedTime)
local_mtime <- file.info("Data/air_quality.rdata")$mtime
# convert this to the format google wants
new_mtime <- paste0(sub("(?<=[0-9])( )(?=[0-9])", "T", format(local_mtime, tz = "GMT"), perl = T), ".000Z")

googledrive::drive_upload(
  media = "Data/air_quality.rdata",
  path = gdrive_sub_dribble,
  name = "air_quality_v002.rdata",
  overwrite = F,
  modifiedTime = new_mtime
)

names(googledrive::drive_endpoints())  # I see "drive.files.create.media" the last entry here
names(googledrive::drive_endpoints()$drive.files.create.media) 
names(googledrive::drive_endpoints()$drive.files.create.media$parameters)
names(googledrive::drive_endpoints()$drive.files.create.media$parameters)
googledrive::drive_endpoints()$drive.files.create.media$parameters$modifiedTime
a <- names(googledrive::drive_endpoints()$drive.files.create.media$parameters)
a[order(a)]
a[a %like% "modified"]  # I dont see any published' paramters here
a[a %like% "Revi"]
gdrive_aq <- googledrive::drive_ls(googledrive::drive_get(gdrive_sub_dribble$id ))


gdrive_aq <- googledrive::drive_ls(googledrive::drive_get(gdrive_sub_dribble$id ))
gdrive_aq  #Note that things are ordered by modified date, and my newest one is not first
sapply(gdrive_aq$drive_resource, "[[", "modifiedTime")

# TODO should I actually be modifying the creation date?
names(gdrive_aq$drive_resource[[1]])
gdrive_aq$drive_resource[[2]]$createdTime
gdrive_aq$drive_resource[[2]]$modifiedTime
gdrive_aq$drive_resource[[2]]$createdTime

names(googledrive::drive_endpoints()$drive.files.create.media$parameters)
googledrive::drive_endpoints()$drive.files.create.media$parameters$keepRevisionForever
googledrive::drive_endpoints()$drive.files.create.media$parameters$description
googledrive::drive_endpoints()$drive.files.create.media$parameters$uploadType

# What if I do this again but change the uploadType to "multipart"?

googledrive::drive_upload(
  media = "Data/air_quality.rdata",
  path = gdrive_sub_dribble,
  name = "air_quality_v003.rdata",
  overwrite = F,
  uploadType = "multipart"
)
gdrive_aq <- googledrive::drive_ls(googledrive::drive_get(gdrive_sub_dribble$id ))

gdrive_aq[1, ]$drive_resource[[1]]$modifiedTime 
gdrive_aq[1, ]$drive_resource[[1]]$createdTime 

gdrive_aq[3, ]$drive_resource[[1]]$modifiedTime 

# is there a different thing I can modify? just annoying that renaming can mess up my modified time


googledrive::drive_reveal(gdrive_aq[1, ], what = "published")$revision_resource[[1]]$modifiedTime

gdrive_aq[1, ]$drive_resource[[1]]$modifiedTime
# Could 


googledrive::drive_reveal(gdrive_aq[3, ], what = "published")$revision_resource[[1]]$modifiedTime
gdrive_aq[3, ]$drive_resource[[1]]$modifiedTime
# When I manually changed the modify time, it changed both the one in drive_resources and revision_resources
# 


googledrive::drive_reveal(gdrive_aq[1, ], what = "published")$revision_resource[[1]]$modifiedTime
gdrive_aq[1, ]$drive_resource[[1]]$modifiedTime
