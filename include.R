load("fluData.RData")

# Συνάρτηση υπολογισμού της εβδομάδας κατά ISO
isoweek <- function(x, type="both_num") {
  alts=c("week","year","both_text","both_num")
  if(!(type %in% alts)) stop("Unknown isoweek type requested!")
  x.date<-as.Date(x)
  x.weekday<-as.integer(format(x.date,"%w"))
  x.weekday[x.weekday==0]=7
  x.nearest.thu<-x.date-x.weekday+4
  x.isoyear<-as.integer(substring(x.nearest.thu,1,4)) # Μπορεί οι πρώτες μέρες του χρόνου να ανήκουν (κατά ISO) στην προηγούμενη χρονιά!
  x.isoweek<-(as.integer(x.nearest.thu-as.Date(paste(x.isoyear,"-1-1",sep="")))%/%7)+1
  switch(type,
    week = x.isoweek,
    year = x.isoyear,
    both_text = ifelse((is.na(x.isoyear) | is.na(x.isoweek)),NA,paste(x.isoweek,x.isoyear,sep="/")),
    both_num = ifelse((is.na(x.isoyear) | is.na(x.isoweek)),NA,x.isoyear*100+x.isoweek)
    )
  }

# Συνάρτηση εύρεσης της ημ/νίας έναρξης μιας ISO εβδομάδας
isoweekStart <- function(x) {
  year <- x %/% 100
  week <- x %% 100
  x.date <- as.Date(paste(year,"-6-1", sep=""))
  x.weekday <- as.integer(format(x.date,"%w"))
  x.weekday[x.weekday==0]=7
  x.nearest.thu <- x.date-x.weekday+4
  x.isoweek <- isoweek(x.nearest.thu, "week")
  res <- x.nearest.thu + 7*(week-x.isoweek) - 3
  if (sum(isoweek(res, type="both_num") != x)>0) stop("Error specifying ISO week number")
  return(res)
}

# Συνάρτηση μετατροπής των ημερομηνιών του SPSS σε κατανοητή από το R μορφή
spssdate<-function(x){as.Date(ISOdate(1582,10,14)+x)}

# Συνάρτηση για recoding τιμών σε μια μεταβλητή
recode <- function(target,from,to) {
  if (!is.vector(target)) stop("Argument \"target\" is not a vector")
  if (!is.vector(from)) stop("Argument \"from\" is not a vector")
  if (!is.vector(to)) stop("Argument \"to\" is not a vector")
  if (length(from)!=length(to)) stop("Arguments \"from\" and \"to\" must have same length")
  for (i in 1:length(from)) target<-gsub(from[i],to[i],target)
  target
  }




# Συνάρτηση για μετατροπή χρώματος σε διαφανές
# (χρησιμοποιείται στη συνάρτηση sentinel_graph() )
addalpha <- function(colors, alpha=1.0) {
    r <- col2rgb(colors, alpha=T)
    # Apply alpha
    r[4,] <- alpha*255
    r <- r/255.0
    return(rgb(r[1,], r[2,], r[3,], r[4,]))
  }

drawBand <- function(x, y.lo, y.hi, col) {
  if (length(x)!=length(y.lo) || length(x)!=length(y.hi)) {
    stop("Arguments x, y.lo, y.hi must have the same length")
  }
  for (i in 1:length(x)) {
    if (sum(is.na(c(y.lo[i:(i+1)], y.hi[i:(i+1)])))==0) {
      polygon(x=x[i+c(0,1,1,0)], y=c(y.lo[i:(i+1)], y.hi[(i+1):i]), col=col, border=NA)
    } else if (sum(is.na(c(y.lo[i], y.hi[i])))==0 && (i==1 || sum(is.na(c(y.lo[i-1], y.hi[i-1])))>0)) {
      polygon(x=x[c(i,i)], y=c(y.lo[i], y.hi[i]), col=col, border=col)
    }
  }
}


# Γράφημα που δείχνει μία περίοδο γρίπης (εβδ. 40 έως εβδ. 20)
sentinel_graph <- function(years, col=rainbow(length(years)), 
	yaxis2=NA, mult=1, ygrid=0, lty=rep(1,length(years)), lwd=rep(1,length(years)),
	ylab=NA, xlab=NA,
	ylab2=NA, ylab2rot=TRUE, ci=FALSE, alpha=0.1, lang="GR")
{
  if (is.na(ylab)) {
    ylab <- c(GR="Κρούσματα γριπώδους συνδρομής ανά 1000 επισκέψεις", EN="Influenza-like Illness cases per 1000 consultations")[lang]
  }
  if (is.na(xlab)) {
    xlab <- c(GR="Εβδομάδα", EN="Week")[lang]
  }
  drawCI <- function(i) {
    if (ci[i]) {
      if (alpha[i]==0) {
        plotCI(set[,i], 
            ui=exp(log(set[,i]) + 1.96*set_logsd[,i]),
            li=exp(log(set[,i]) - 1.96*set_logsd[,i]), 
            col=col[i], sfrac=0.005, cex=0.01, xpd=TRUE, add=TRUE)
      } else {
        drawBand(x=1:length(set[,i]), col=addalpha(col[i], alpha[i]),
            y.lo=exp(log(set[,i]) - 1.96*set_logsd[,i]),
            y.hi=exp(log(set[,i]) + 1.96*set_logsd[,i]))
      }
    }
  }
  ratechart <- resAll$gri; names(ratechart) <- resAll$yearweek
  ratechart_logsd <- resAll$log.gri.sd; names(ratechart_logsd) <- resAll$yearweek
  if(length(ci)==1) ci <- rep(ci, length(years))
  if(length(alpha)==1) alpha <- rep(alpha, length(years))
  maxwk <- ifelse(sum(as.integer(isoweek(as.Date(paste(years,"-12-31",sep="")))==53))>0, 53, 52)
  set <- sapply(years, function(x){ratechart[as.character(c((x*100+40):(x*100+maxwk),((x+1)*100+1):((x+1)*100+20)))]})
  set_logsd <- sapply(years, function(x){ratechart_logsd[as.character(c((x*100+40):(x*100+maxwk),((x+1)*100+1):((x+1)*100+20)))]})
  maxes <- sapply(1:ncol(set), function(i){
    suppressWarnings(max(exp(log(set[,i]) + ci[i]*1.96*set_logsd[,i]), na.rm=TRUE))
  })
  limrate <- (max(maxes, na.rm=TRUE)%/%10+2)*10
  if(!is.na(yaxis2[1])) {
    i2 <- match(yaxis2, years)
    i1 <- (1:length(years))[-match(yaxis2,years)]
    maxes[i2] <- maxes[i2]/mult
    limrate <- (max(maxes, na.rm=TRUE)%/%10+2)*10
    limrate2 <- limrate*mult
  }
  par(mar=c(5.1,4.1+grepl("\n",ylab),2.1,2.1+(2.5*!is.na(yaxis2[1]))+grepl("\n",ylab2)))
  plot(0, type="n", bty="l", xaxt="n", ylim=c(0,limrate), xlim=c(1,ifelse(maxwk==53,34,33)), ylab=ylab, xlab=xlab)
  axis(1, at=1:(maxwk-19), labels=NA)
  mtext(c(40:maxwk,1:20), side=1, at=1:(maxwk-19), cex=0.7, line=0.5)
  if (!is.na(ygrid)) {
    if (ygrid==0)
      ygrid <- ifelse(limrate<120, 10, 20)
    abline(h=seq(0,limrate,by=ygrid), lty=3, col="lightgrey")
  }
  legend("topright", legend=paste(years,years+1,sep="-"), col=col[1:length(years)], lwd=2*lwd, pch=16, 
      box.col="white", box.lwd=10, bg="white", inset=0.01, pt.cex=lwd, lty=lty)
  if (!is.na(yaxis2[1])) {
    sapply(i1, function(i){
      points(set[,i], type="o", col=col[i], lwd=2, pch=16)
      drawCI(i)
    })
    par(new=TRUE)
    plot(0, type="n", bty="u", xaxt="n", yaxt="n", ylim=c(0,limrate2), xlim=c(1,ifelse(maxwk==53,34,33)), ylab=NA, xlab=NA)
    if (ylab2rot) {
      text(par("usr")[2] + 0.10*diff(par("usr")[1:2]*(1+0.15*grepl("\n",ylab2))), 
	  par("usr")[4]-diff(par("usr")[3:4])/2, 
	  srt = -90, labels=ylab2, xpd=TRUE)
    } else {
      mtext(ylab2, side=4, line=2.5+grepl("\n",ylab))
    }
    sapply(i2, function(i){
      points(set[,i], type="o", col=col[i], lwd=2*lwd[i], pch=16, lty=lty[i], cex=lwd[i])
      drawCI(i)
    })
    axis(4)
    # Οι γραμμές του αριστερού y άξονα θέλουμε να βρίσκονται σε πρώτο πλάνο.
    par(new=TRUE)
    plot(0, type="n", axes=FALSE, ylim=c(0,limrate), xlim=c(1,ifelse(maxwk==53,34,33)), ylab=NA, xlab=NA)
    sapply(i1, function(i){
      points(set[,i], type="o", col=col[i], lwd=2*lwd[i], pch=16, cex=lwd[i])
      drawCI(i)
    })
  } else {
    sapply(1:length(years), function(i){
      points(set[,i], type="o", col=col[i], lwd=2*lwd[i], pch=16, lty=lty[i], cex=lwd[i])
      drawCI(i)
    })
  }
  return()
}


swabPlot <- function(season, limweek, sel="all", ymax=NA, lang="GR", plot=TRUE){
    if (sel=="sent") {
      allSwabs <- datLabSent[datLabSent$season==season,2:10]
    } else if (sel=="hosp") {
      allSwabs <- datLabHosp[datLabHosp$season==season,2:10]
    } else {
      allSwabs <- datLabAll[datLabAll$season==season,2:10]
    }
    maxwk <- ifelse(sum(as.integer(isoweek(as.Date(paste(season,"-12-31",sep="")))==53))>0, 53, 52)
    weekSel <- c(season*100+40:maxwk, (season+1)*100+1:20)
    allSwabs[-(1:match(limweek, weekSel)),] <- 0
    if (!plot) {
      rownames(allSwabs) <- weekSel
      return(allSwabs)
    }
    if (is.na(ymax)) ymax <- max(allSwabs[,1])+20
    ymax <- ceiling(ymax/20)*20
    swCol <- c("dodgerblue3", "sandybrown", "red3", "orangered", "lightpink3", "darkgrey")
    barplot(t(allSwabs[,c("B", "A(H3N2)", "A(H1N1)pdm09", "Other.A", "Unknown.A", "Negative")]), 
	beside=FALSE, border=NA, col=swCol, las=2, axisnames=F, cex.axis=0.9, ylim=c(0, ymax),
	font.lab=2, cex.lab=0.9, 
	ylab=c(GR="Φαρυγγικά δείγματα και απομονωθέντα στελέχη", EN="Number of swab samples")[lang])
    abline(h=seq(0,500,20), col="lightgrey", lwd=0.5)
    abline(h=0)
    bpos <- barplot(t(allSwabs[,c("B", "A(H3N2)", "A(H1N1)pdm09", "Other.A", "Unknown.A", "Negative")]), 
	beside=FALSE, border=NA, col=swCol, add=TRUE, axes=F, axisnames=F, ylim=c(0, ymax),
	legend.text=list(
            GR=c("B", "A(H3N2)", "A(H1N1)pdm09", "A", "Άγνωστος τύπος", "Αρνητικά"),
            EN=c("B", "A(H3N2)", "A(H1N1)pdm09", "A", "Type unknown", "Negative")
	)[[lang]],
	args.legend=list(bty="o", box.col="white", bg="white", border=NA, cex=0.8, x="topright"))
    axis(1, at=bpos[seq(1,length(bpos),2)], labels=sprintf("%02d", weekSel %% 100)[seq(1,length(bpos),2)], 
	lwd=0, cex.axis=0.8, line=-1)
    axis(1, at=bpos[seq(2,length(bpos),2)], labels=sprintf("%02d", weekSel %% 100)[seq(2,length(bpos),2)], 
	lwd=0, cex.axis=0.8, line=-1)
    axis(2, at=c(-10, 10000))
    mtext(c(GR="Εβδομάδα", EN="Week")[lang], side=1, cex=0.9, font=2, line=1.5)
    return()
}


methDeathPlot <- function(ssn, limweek, lang="GR", death=FALSE, plot=TRUE){
    maxwk <- ifelse(sum(as.integer(isoweek(as.Date(paste(ssn,"-12-31",sep="")))==53))>0, 53, 52)
    weekSel <- c(ssn*100+40:maxwk, (ssn+1)*100+1:20)
    swCol <- c("dodgerblue3", "sandybrown", "red3", "orangered", "lightpink3", "darkgrey")
    if (death) {
      d <- subset(datICU, outcome=="death" & season==ssn & yearweek<=limweek)
    } else {
      d <- subset(datICU, typ=="m" & season==ssn & yearweek<=limweek)
    }
    d$yearweekf <- factor(d$yearweek, levels=weekSel)
    d$flutypef <- factor(d$flutype, levels=rev(c("A", "A(H1N1)pdm09", "A(H3N2)", "B")))
    if (!plot) {
      d <- with(d, as.data.frame.matrix(t(table(flutypef, yearweekf))))
      return(d)
    }
    ymax <- ceiling(max(with(d, table(flutypef, yearweekf)))*1.3/10)*10
    if (ymax==0) ymax <- 40
    bpos <- barplot(with(d, table(flutypef, yearweekf)), border=NA, ylim=c(0,ymax),
      col=c(swCol[1:4], "darkslategrey"), axisnames=F, axes=F, font.lab=2, cex.lab=0.9, 
      ylab=c(GR="Αριθμός κρουσμάτων", EN="Number of cases")[lang])
    abline(h=axTicks(2), col="lightgrey", lwd=0.5)
    abline(h=0)
    bpos <- barplot(with(d, table(flutypef, yearweekf)), border=NA, col=c(swCol[1:4], "darkslategrey"), 
      legend.text=TRUE, axisnames=F, axes=F, font.lab=2, add=TRUE,
      args.legend=list(bty="o", box.col="white", bg="white", border=NA, cex=0.8, x="topright", inset=c(0,-0.03)))
    axis(1, at=bpos[seq(1,length(bpos),2)], labels=(weekSel %% 100)[seq(1,length(bpos),2)], 
	lwd=0, cex.axis=0.8, line=-1)
    axis(1, at=bpos[seq(2,length(bpos),2)], labels=(weekSel %% 100)[seq(2,length(bpos),2)], 
	lwd=0, cex.axis=0.8, line=-1)
    axis(2, las=2, cex.axis=0.9) # axis(2, at=seq(0,bylim,2), las=2, cex.axis=0.9)
    mtext(c(GR="Εβδομάδα εισαγωγής στη ΜΕΘ", EN="Week of admission in ICU")[lang], side=1, cex=0.9, font=2, line=1.5)
    return()
}



methDeathAgePlot <- function(ssn, limweek, lang="EN", plot=TRUE){
    maxwk <- ifelse(sum(as.integer(isoweek(as.Date(paste(ssn,"-12-31",sep="")))==53))>0, 53, 52)
    weekSel <- c(ssn*100+40:maxwk, (ssn+1)*100+1:20)
    swCol <- c("dodgerblue3", "sandybrown", "red3", "orangered", "lightpink3", "darkgrey")
    d <- subset(datICU, season==ssn & yearweek<=limweek)
    a <- cbind("Εισαγωγές σε ΜΕΘ"=table(cut(subset(d, typ="m")$age, breaks=seq(0,100,10), right=FALSE, labels=paste(seq(0,90,10), seq(9,99,10), sep="-"))),
	"Θάνατοι"=table(cut(subset(d, outcome=="death")$age, breaks=seq(0,100,10), right=FALSE, labels=paste(seq(0,90,10), seq(9,99,10), sep="-"))))
    if (lang!="GR") { colnames(a) <- c("ICU hospitalizations", "Deaths") }
    if (plot==TRUE) {
      bpos <- barplot(t(a), border=NA, beside=TRUE, col=c("darkred", "darkgreen"), axisnames=F, axes=F, ylab=c(GR="Αριθμός κρουσμάτων", EN="Number of cases")[lang], font.lab=2, cex.lab=0.9, ylim=c(0,max(a)+6),
          legend.text=TRUE, args.legend=list(bty="o", box.col="white", bg="white", border=NA, cex=0.8, x="topright", inset=c(0,-0.03)))
      axis(2, las=2, cex.axis=0.9) # axis(2, at=seq(0, max(a)+6, 2), las=2, cex.axis=0.9)
      axis(1, at=apply(bpos, 2, mean)[seq(1,ncol(bpos),2)], labels=rownames(a)[seq(1,ncol(bpos),2)], 
          lwd=0, cex.axis=0.8, line=-1)
      axis(1, at=apply(bpos, 2, mean)[seq(2,ncol(bpos),2)], labels=rownames(a)[seq(2,ncol(bpos),2)], 
          lwd=0, cex.axis=0.8, line=-1)
      mtext(c(GR="Ηλικιακή ομάδα", EN="Age group")[lang], side=1, cex=0.9, font=2, line=1.5)
      return(bpos)
    } else {
      a <- as.data.frame.matrix(a)
      return(a)
    }
}


methDeathTotals <- function(ssn, limweek){
    d <- subset(datICU, season==ssn & yearweek<=limweek)
    with(d, c(sum(typ=="m", na.rm=TRUE), sum(outcome=="death", na.rm=TRUE)))
}


plotMomo <- function(y, lang="GR", plot=TRUE) {
  minLim <- max((y-4)*100+20, min(momo$wk))
  maxLim <- min(minLim+520, max(momo$wk))
  gLim <- c(y*100+40, (y+1)*100+20)
  if (gLim[2]>max(momo$wk)) gLim[2] <- max(momo$wk)
  m <- subset(momo, wk>=minLim & wk<=maxLim)
  if (!plot) {
    m <- m[,4:8]
    return(m)
  }
  par(mar=c(10,4,2,2))
  plot(0, type="n", bty="l", ylim=c(1000,4000), xlim=c(1,nrow(m)), 
      xaxt="n", xlab=NA,
      ylab=c(GR="Εβδομαδιαίος αριθμός θανάτων", EN="Weekly number of deaths")[lang])
  mtext(c(GR="Έτος - Αριθμός εβδομάδας", EN="Year - Week number")[lang], line=5, side=1)
  polygon(x=match(c(gLim, rev(gLim)), m$wk), 
      y=rep(c(0,10^5),each=2), col="lightgray", border=NA)
  abline(v=which((m$wk %% 100) %in% c(20,40))[-1], lty="dotted", col="grey")
  points(y=m$UPIb4, x=1:nrow(m), col="yellow3", type="l", lwd=2)
  points(y=m$UPIb2, x=1:nrow(m), col="orange2", type="l", lwd=2)
  points(y=m$Pnb, x=1:nrow(m), col="firebrick2", type="l", lwd=2)
  points(y=m$nbc, x=1:nrow(m), col="steelblue4", type="l", lwd=2)
  axis(1, at=which(!is.na(m$wy)), labels=m$wy[!is.na(m$wy)], las=2)
  par(lend=1)
  legend("bottomleft", lwd=c(15,2,2), cex=0.92,
      xpd=NA, bty="n", inset=c(0,-0.95),
      seg.len=4, col=c("lightgray", "firebrick2", "steelblue4"),
      legend=
        list(GR=c("Επιλεγμένη περίοδος", "Αναμενόμενος αριθμός θανάτων", "Παρατηρούμενος αριθμός θανάτων"),
        EN=c("Selected period", "Expected number of deaths", "Observed number of deaths"))[[lang]])
  legend("bottomright", lwd=2, cex=0.92,
      xpd=NA, bty="n", inset=c(0,-0.95),
      seg.len=4, col=c("yellow3", "orange2"),
      legend=list(GR=c("+4 σταθερές αποκλίσεις από το αναμενόμενο", 
          "+2 σταθερές αποκλίσεις από το αναμενόμενο"), 
        EN=c("+4 standard deviations from expected", 
          "+2 standard deviations from expected"))[[lang]])
}




sentinel_graph_download <- function(years)
{
  ratechart <- resAll$gri; names(ratechart) <- resAll$yearweek
  ratechart_logsd <- resAll$log.gri.sd; names(ratechart_logsd) <- resAll$yearweek
  maxwk <- ifelse(sum(as.integer(isoweek(as.Date(paste(years,"-12-31",sep="")))==53))>0, 53, 52)
  set <- sapply(years, function(x){ratechart[as.character(c((x*100+40):(x*100+maxwk),((x+1)*100+1):((x+1)*100+20)))]})
  set_logsd <- sapply(years, function(x){ratechart_logsd[as.character(c((x*100+40):(x*100+maxwk),((x+1)*100+1):((x+1)*100+20)))]})
  res <- as.data.frame.matrix(do.call(cbind, lapply(1:length(years), function(i) {
    res <- round(cbind(set[,i], exp(log(set[,i]) - 1.96*set_logsd[,i]), exp(log(set[,i]) + 1.96*set_logsd[,i])),2)
    colnames(res) <- paste0(years, c("", ".lo", ".hi"))
    res
  })))
  rownames(res)[is.na(rownames(res))] <- 53
  rownames(res) <- sprintf("%02d", as.integer(rownames(res)) %% 100)
  res
}


wkLims <- function(x, lang="GR") {
  months <- list(
    GR=c("Ιανουαρίου", "Φεβρουαρίου", "Μαρτίου", "Απριλίου", "Μαϊου", "Ιουνίου", "Ιουλίου",
      "Αυγούστου", "Σεπτεμβρίου", "Οκτωβρίου", "Νοεμβρίου", "Δεκεμβρίου"),
    EN=c("January", "February", "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"))
  x <- as.integer(unlist(strsplit(x, "/")))
  a1 <- isoweekStart(x[2]*100+x[1])
  a2 <- a1 + 6
  b1 <- as.integer(format(a1, c("%d","%m","%Y")))
  b2 <- as.integer(format(a2, c("%d","%m","%Y")))
  b1[2] <- months[[lang]][b1[2]]
  b2[2] <- months[[lang]][b2[2]]
  if (b1[3]==b2[3]) b1 <- b1[1:2]
  if (b1[2]==b2[2]) b1 <- b1[1]
  return(paste(paste(b1, collapse=" "), paste(b2, collapse=" "), sep=" – "))
}


