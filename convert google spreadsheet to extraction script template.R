########################################################################
### Paths
########################################################################

### Add any relevant paths to this vector. The script will select the
### correct path itself.

basePathVector <- c("B:/Data/research/intervention-planning-sysrev",
                    "");

########################################################################
### Set the variables with the paths
########################################################################

### Check which paths exist and set the first existing path in the list
### as the base path
basePath <- basePathVector[sapply(basePathVector, dir.exists)][1];

### Set the additional paths
outputPath <- basePath;

########################################################################
### Load packages
########################################################################

### Packages
require('userfriendlyscience');
safeRequire('googlesheets');

########################################################################
### Load spreadsheet
########################################################################

### Get authorization to read Google sheet
#gs_auth(new_user=TRUE);

### Read google sheet
dat <- gs_read(gs_url(paste0("https://docs.google.com/spreadsheets/d/",
                             "1g__lA6Qd8x9UcUMIZGq_xMcZt584sJ-1sMaLmxm3nUU")));

########################################################################
### Process data
########################################################################

### Get first two columns
activities <- dat[, 1];
descriptions <- dat[, 2];

### Remove empty lines
descriptions <- descriptions[!is.na(activities), ];
activities <- activities[!is.na(activities), ];

### Index organising elements
organisingLines <- which(is.na(descriptions));

### Wrap
activities <- sapply(apply(activities, 1, strwrap, width=72, prefix='###   '),
                     paste0, collapse="\n");
descriptions <- sapply(apply(descriptions, 1, strwrap, width=72, prefix='###   '),
                       paste0, collapse="\n");

### Generate first concept of extraction script
extractionScript <- sapply(1:length(activities), function(i) {
  if (i %in% organisingLines) {
    return(paste0(repStr("#", 80), "\n",
                  repStr("#", 80), "\n###\n",
                  activities[i], "\n###\n",
                  repStr("#", 80), "\n",
                  repStr("#", 80), "\n"));
  } else {
    return(paste0(repStr("#", 80),
                  "\n### ", names(dat)[1], "\n",
                  activities[i],
                  "\n###\n### ", names(dat)[2], "\n",
                  descriptions[i], "\n"));
  }
});

writeLines(extractionScript,
           file.path(basePath,
                     "extraction script template foundation.R"));

