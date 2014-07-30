##' barplot with errorbar
##'
##' barplot
##' @title barplot 
##' @param data data.frame
##' @param title plot title
##' @param xlab xlab
##' @param ylab ylab
##' @param barIndex index of bar to add annotation of statistical significance
##' @param label statistical significant label, eg. *, **
##' @return ggplot2 object
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes
##' @importFrom ggplot2 geom_errorbar
##' @importFrom ggplot2 geom_bar
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 ggtitle
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 position_dodge
##' @export
##' @examples
##' df <- data.frame(Normal=c(0.83,0.79,0.99,0.69), Cancer=c(0.56,0.56,0.64,0.52))
##' barplot(df, barIndex=c(1,2), label="*")
##' @author Guangchuang Yu
barplot <- function(data, title="", xlab="", ylab="", barIndex=NULL, label="*") {
    m <- colMeans(data)
    s <- apply(data, 2, sd)
    d <- data.frame(sample=names(m), mean=m, sd=s)
    d$sample <- factor(d$sample, levels=colnames(data))

    p <- ggplot(d, aes(sample, mean))
    p <- p+geom_bar(aes(fill=sample), stat="identity",
                    position=position_dodge(width=0.8)) ##, colour="black")
    p <- p+geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, width=0.2),
                         position=position_dodge(width=0.8))

    ## p <- p+scale_fill_manual(values=c("grey80", "white"))
    ## p <- p+scale_y_continuous(expand=c(0,0), limits=c(0,1.2), breaks=seq(0,1.2, by=0.2))
    
    ## p <- p+theme(axis.text.x=element_text(face="bold", size=font.size),
    ##              axis.text.y=element_text(face="bold", size=font.size))
    p <- p + ggtitle(title)+xlab(xlab)+ylab(ylab)
    if (! is.null(barIndex) && !is.na(barIndex)) {
        if (length(barIndex) != 2) {
            warning("barIndex should be of length 2...")
        } else {
            p <- addLabel(p, barIndex=barIndex, label=label)
        }
    }
    
    return(p)
}

##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 annotate
##' @importFrom ggplot2 aes
##' @importFrom ggplot2 scale_y_continuous
addLabel <- function(p, barIndex, label="*") {
    d <- p$data
    lw <- d$mean+d$sd
    lw <- lw[barIndex]
    lw <- lw + min(lw) * 0.05
    up <- max(lw * 1.1)
 
    p <- p+geom_segment(x=barIndex[1], y=lw[1], xend=barIndex[1], yend=up)
    p <- p+geom_segment(x=barIndex[2], y=lw[2], xend=barIndex[2], yend=up)
    p <- p+geom_segment(x=barIndex[1], y=up, xend=barIndex[2], yend=up)
    p <- p+annotate("text", x=mean(barIndex),y=up*1.02, label=label)
    p <- p+scale_y_continuous(expand=c(0,0), limits=c(0, up*1.05))
    return(p)
}
