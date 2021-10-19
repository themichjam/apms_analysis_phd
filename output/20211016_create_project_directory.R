ROOT
-data
--raw
--processed
--metadata
-docs
-src
-scripts
-rmd
-output
-imgs
-figs

ifelse(!dir.exists("apms_phd"), dir.create("apms_phd"), "Folder already exists")
if (!dir.exists("./data")) dir.create("./data")  # create output directory
if (!dir.exists("./data/raw")) dir.create("./data/raw")  # create output directory
if (!dir.exists("./data/processed")) dir.create("./data/processed")  # create output directory
if (!dir.exists("./data/metadata")) dir.create("./data/metadata")  # create output directory
if (!dir.exists("./docs")) dir.create("./docs")  # create output directory
if (!dir.exists("./src")) dir.create("./src")  # create output directory
if (!dir.exists("./script")) dir.create("./script")  # create output directory
if (!dir.exists("./rmd")) dir.create("./rmd")  # create output directory
if (!dir.exists("./output")) dir.create("./output")  # create output directory
if (!dir.exists("./imgs")) dir.create("./imgs")  # create output directory
if (!dir.exists("./figs")) dir.create("./figs")  # create output directory
