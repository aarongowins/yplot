##' query pubmed record numbers of search term
##'
##' 
##' @title getPubmedTrend
##' @param searchTerm search term
##' @param year year vector 
##' @return plot
##' @author ygc
##' @importFrom plyr ldply
##' @export
getPubmedTrend <- function(searchTerm, year) {
    res <- lapply(searchTerm, getPubmedTrend.internal, year=year)
    names(res) <- searchTerm
    res.df <- ldply(res)
    colnames(res.df)[1] <- "TERM"
    return(res.df)
}

##' @importFrom RISmed EUtilsSummary
##' @importFrom RISmed QueryCount
getPubmedTrend.internal <- function(searchTerm, year) {
    num <- array()
    x <- 1
    for (i in year){
        Sys.sleep(1)
        cat("querying year ", i, "\n")
        r <- EUtilsSummary(searchTerm, type='esearch', db='pubmed', mindate=i, maxdate=i)
        num[x] <- QueryCount(r)
        x <- x + 1
    }
    res <- data.frame(year=year, number=num)
    return(res)
}

##' visualize pubmed trend
##'
##' 
##' @title plotPubmedTrend
##' @param x data.frame or a name list of data.frame, which output by getPubmedTrend
##' @return figure
##' @author ygc
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes
##' @importFrom ggplot2 geom_point
##' @importFrom ggplot2 geom_line
##' @export
plotPubmedTrend <- function(x) {
    year <- number <- TERM <- NULL
    ## if (is(x, "list")) {
    ##     df <- ldply(x)
    ##     colnames(df)[1] <- "TERM"
    if (ncol(x) == 3 && "TERM" %in% colnames(df)) {
        p <- ggplot(df, aes(factor(year), number, group=TERM, color=TERM))+
            geom_point(size=3) + geom_line()
    } else {
        p <- ggplot(x, aes(factor(year), number))+geom_point(size=3) + geom_line()
    }
    return(p)
}

##' query pubmed information
##'
##' 
##' @title getPubmed 
##' @param searchTerm search term
##' @return data.frame
##' @author ygc
##' @importFrom RISmed EUtilsSummary
##' @importFrom RISmed EUtilsGet
##' @importFrom RISmed ArticleTitle
##' @importFrom RISmed Title
##' @importFrom RISmed AbstractText
##' @importFrom RISmed PMID
##' @importFrom RISmed ELocationID
##' @importFrom RISmed ISSN
##' @importFrom RISmed Author
##' @importFrom RISmed Affiliation
##' @importFrom RISmed Year
##' @importFrom RISmed Issue
##' @importFrom RISmed MedlinePgn
##' @export
getPubmed <- function(searchTerm) {
    myPubmed <- EUtilsSummary(query=searchTerm, type="esearch", db="pubmed")
    myRecord <- EUtilsGet(myPubmed)
    
    res <- data.frame(title=ArticleTitle(myRecord),    
                      publication=Title(myRecord),
                      abstract=AbstractText(myRecord),
                      pmid=PMID(myRecord),
                      doi=ELocationID(myRecord),
                      issn=ISSN(myRecord),               
                      authors= sapply(Author(myRecord),
                          function(i) paste(paste(i$LastName, i$Initials), collapse=",")),
                      affiliation= Affiliation(myRecord),
                      year=Year(myRecord),
                      issue=Issue(myRecord),
                      page=MedlinePgn(myRecord)
                      )
    return(res)
}

##' plot word cloud of abstract
##'
##' 
##' @title abstractWordcloud
##' @param abstract abstract text
##' @return plot
##' @author ygc
##' @importFrom RColorBrewer brewer.pal
##' @importFrom wordcloud wordcloud
abstractWordcloud <- function(abstract) {
    abs <- as.character(abstract)
    abs.wd <- unlist(strsplit(abs, " "))
    abs.wd <- sub("\\,", "", abs.wd)
    abs.wd <- sub("\\.", "", abs.wd)
    abs.tab <- table(abs.wd)

    rm.word <- c(
        "a", "also", "an", "and", "as", "As", "are",   
        "be", "by",
        "can", "could", "current", 
        "describe", 
        "easily", "exerts", 
        "Finally", "first", "FIRST", "for", "from", "found", "further", 
        "has", "have", "Here", 
        "is", "in", "In", "including", "into", "its",   
        "mainly", "may", "many", "Moreover", "most",
        "not",
        "of", "on", "or", "other", "our", "Our", 
        "play", "plays", "present", "provided", "providing", 
        "remains", "reported", "resulted", "revealed", 
        "several", "showed", "studies", "such", 
        "that", "the", "The", "then", "therefore", "these", "These", "This", "to", "total", "through", "thus", 
        "under", "used", "using",
        "various", "via", 
        "was", "we", "We", "well", "were", "which", "with" 
        )
    
    abs.tab <- abs.tab[! names(abs.tab) %in% rm.word]
    pal2 <- brewer.pal(8,"Dark2")
    wordcloud(names(abs.tab), abs.tab,min.freq=2,
              max.words=Inf, random.order=TRUE,
              rot.per=.15, colors=pal2)

}
