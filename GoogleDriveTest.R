# Author: Geoff Mayhew
# Created: 2023-Dec-20

library(data.table)
library(googledrive)  # https://googledrive.tidyverse.org/

# googledrive::as_dribble()  # This is where stuff is saved

# link to Geof's Ancillary folder in the 2024 ADP google drive
# https://drive.google.com/drive/folders/1aWgsxLcSfh2dxnceg_8FNf0xVB81my1t
# sandbox.Rdata is something we can test. See if we can load it here and save it locally in the Data folder

drive_download(
  file = as_id("https://drive.google.com/file/d/1WRUZaWWvmFMQyLXJRCZ6AkBvYMaVHoTT/view?usp=drive_link"), 
  path = "Data/sandbox.Rdata")  
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

?drive_auth_configure()



# Storing user data 
# These packages may store your credentials on your local machine, for later reuse by you. Use caution when using these packages on a shared machine.
# 
# By default, an OAuth token is cached in a local file, such as ~/.R/gargle/gargle-oauth. See the documentation for gargle::gargle_options() and gargle::credentials_user_oauth2() for information on how to control the location of the token cache or suppress token caching, globally or at the individual token level.
# 

# Use this function to get prompted to give permissions again.
drive_auth()

# trying this again..
drive_download(
  file = as_id("https://drive.google.com/file/d/1WRUZaWWvmFMQyLXJRCZ6AkBvYMaVHoTT/view?usp=drive_link"), 
  path = "Data/sandbox.Rdata")  
# Cool, downloaded! This has a current version and Version 1, which is older.


# Get metatadata for a folder (I'm looking at my local data folder)
a <- googledrive::as_dribble("Data/")
a$id
a$drive_resource[[1]]
a$drive_resource[[1]]$owners
a$drive_resource[[1]]$shared

# Putting the google drive's url in here
a1 <- googledrive::as_dribble(as_id("https://drive.google.com/file/d/1WRUZaWWvmFMQyLXJRCZ6AkBvYMaVHoTT/view?usp=drive_link"))
a1

# Let's try to put data up on the drive first

# Here is the url to my ancillary folder (Geoff's Notes)
# https://drive.google.com/drive/folders/1aWgsxLcSfh2dxnceg_8FNf0xVB81my1t?usp=drive_link

# Make an object (copy mtcars)
data <- copy(mtcars)
# Save the file as an Rdata file
save(data, file = "Data/test_data.rdata")
# Upload it to the google drive
drive_upload(
  "Data/test_data.rdata",
  path = as_id("https://drive.google.com/drive/folders/1aWgsxLcSfh2dxnceg_8FNf0xVB81my1t?usp=drive_link"),
  name = "test_data.rdata"
  )  #overwrite = TRUE is the default




drive_find()  # digs through all the files you have in google drive!

drive_get() # use this to find a folder.

fma_analysis_group_dribble <- drive_get("FMA Analysis Group/")
fma_analysis_group_dribble$drive_resource[[1]]

# Get the names of items in a dribble

drive_ls(fma_analysis_group_dribble)

# Make a new dribble for vision 2024 ADP data folder. This might take a a few seconds, depends on what you have in your google drive?
folder.dribble <- drive_get("FMA Analysis Group/Vision 2024 ADP/Documents/Ancillary Documents/Geoff's Notes")

drive_ls(folder.dribble)
# Now try uploading to this dribble?
drive_upload(
  "Data/test_data.rdata",
  path = folder.dribble,
  name = "test_data.rdata"
) 
# Cool, now it's up there! 

# Now let's try updating it...
data$RN <- runif(n = nrow(data))
# Overwrite the local version
save(data, file = "Data/test_data.rdata")
# Upload, overwriting
drive_upload(
  "Data/test_data.rdata",
  path = folder.dribble,
  name = "test_data.rdata",
  overwrite = TRUE  # Default is NA, which would allow multiple files with the same name!
)    
# File trashed:
#   • test_data.rdata <id: 1YQL7B1cg5DA99KrR1nvRTRcc2H9xI73D>
#   Local file:
#   • Data/test_data.rdata
# Uploaded into Drive file:
#   • test_data.rdata <id: 1XHFhFJSEXU8zi1k7XDaDckF0M3ssm-I8>
# Overwrite actually TRASHES the old version rather than overwriting it!

# Instead, use 
?drive_update()
data$RN2 <- runif(n = nrow(data))
# Overwrite the local version
save(data, file = "Data/test_data.rdata")
drive_update(
  file = paste0(folder.dribble$path, "test_data.rdata"),
  media = "Data/test_data.rdata",
) 
# File updated:
#   • test_data.rdata <id: 1XHFhFJSEXU8zi1k7XDaDckF0M3ssm-I8>

# but it looks like the id is the same as the old one?

# Cool, this now has 'current version' and 'Version 1`. Now let's see if we can access the old version
# TODO I did notice that the 'keep forever' box is not checked for this file!

# Knowing the files's 'id' is the most important thing! 

my_data_file <- drive_get(as_id("1XHFhFJSEXU8zi1k7XDaDckF0M3ssm-I8"))
my_data_file$drive_resource[[1]]
my_data_file$drive_resource[[1]]$version   #Not sure what this 'version' is. I only have 2 versions
my_data_file$drive_resource[[1]]

# ? How to change keep_forever to TRUE (won't need this if don't have 'versions' of the same thing.)
# ? How to access older versions of a file via google drive! (can't do this)


drive_link(my_data_file)  # here is the current version link


# There isn't a convenient way to get versions from google drive. In any case it's probably best to just have different 
# versions of the data published, whether with a date or otherwise.
# data_2022v01, data_2022v02, etc? Shouldn't ever have more than 99 versions for a year. 
# All data files should be named and dated

# TODO Make a protocol or a function that saves these items, gives the google drive link automatically?
# -- Make sure that you never create files with duplicates names or overwrite unless specifically desired! Just make a new version! 
# TODO Figure out how to use functions specific to Google Shared Drives when that is up and running.


drive_find("Ancillary Documents/Geoff's Notes")


# drive_get is good when you know exaccly where something is (searches a path or id link). Drive_find (searches by pattern) is more of a tool when you're not sure.
?drive_get()

# very fast
fma_analysis_group_dribble <- drive_get("FMA Analysis Group/")   
# takes a few seconds
folder.dribble <- drive_get("FMA Analysis Group/Vision 2024 ADP/Documents/Ancillary Documents/Geoff's Notes/") 


# Can also use shared_drive_get?
# https://sites.google.com/noaa.gov/myafsc/technology/google-shared-drives?pli=1
# 'shared_drive' and 'corpus' allows us to work with shared drives specifically?