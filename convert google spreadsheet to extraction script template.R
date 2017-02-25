########################################################################
### Paths
########################################################################

### Add any relevant paths to this vector. The script will select the
### correct path itself.

basePathVector <- c("B:/Data/research/intervention-planning-sysrev",
                    "/home/oupsyusr/public_html/wim");

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
safeRequire('data.tree');

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

dataFrameNetwork <- as.data.frame(dat[!is.na(dat$Identifier),
                                      unique(c('Identifier', 'Parent', names(dat)))]);
dataFrameNetwork$Parent[is.na(dataFrameNetwork$Parent)] <- 'res';
extractionScriptTree <- FromDataFrameNetwork(dataFrameNetwork);

SetGraphStyle(extractionScriptTree, rankdir = "LR");

if (interactive()) {
  plot(extractionScriptTree));
}

png(filename=file.path(basePath, "wim.png"),
    width=1000, height=600);
plot(extractionScriptTree);
savePlot(filename=file.path(basePath, "wim.png"));
dev.off();

extractionScript <- sapply(1:nrow(dat), function(i) {
  if (is.na(dat[i, 'Identifier'])) {
    ### If there is an identifier, this is just an organising element,
    ### assuming there also isn't a description
    if (is.na(dat[i, 'Description'])) {
      ### If there is also no description, print the activity, if
      ### that *is* present
      if (!is.na(dat[i, 'Activity'])) {
        return(paste0(repStr("#", 80), "\n",
                      repStr("#", 80), "\n###\n",
                      dat[i, 'Activity'], "\n###\n",
                      repStr("#", 80), "\n",
                      repStr("#", 80), "\n"));
      }
    }
  } else {
    ### If there is an identifier, this is something to extract. If there
    ### is a description, it is an item; otherwise just an element
    ### in the hierarchy
    if (is.na(dat[i, 'Description'])) {
      extractionScriptTree$AddChild(dat[i, 'Identifier']);
      return(paste0("res$", dat[i, 'Identifier'], " <- list()\n\n"));
    } else {
      Traverse(extractionScriptTree,
               dat[i, 'Parent'],
               traversal = "level")$AddChild(dat[i, 'Identifier']);
      return(paste0(repStr("#", 80),
                    "\n### Activity\n",
                    paste0(strwrap(dat[i, 'Activity'], width=72,
                                   prefix='###   '), collapse="\n"),
                    "\n###\n### Description\n",
                    paste0(strwrap(dat[i, 'Description'], width=72,
                                   prefix='###   '), collapse="\n"),
                    "res$", dat[i, 'Identifier'], "\n\n"));
    }
  }
});

writeLines(paste0(extractionScript, collapse=""),
           file.path(basePath,
                     "extraction script template foundation.R"));
