##Extract Donor Sites

list.file <- list.files(path = "./Testing Set")
len <- length(list.file)

library(stringr)

Donor_extract <- function(list.file, len)
{
  Donor_Seq <- matrix(ncol = 1)
  for(i in seq(1:len))
  {
    file_name <- paste0("./Testing Set/", list.file[i], sep = "", collapse = "")
    getwd()
    
    dat <- scan(file_name, what = "",sep = "",na.strings = "NA")
    pattern <- "\\.\\.[0-9]*"
    loc = dat[2] #training = 8
    loc <- as.data.frame(str_match_all(loc,pattern))
    loc_num <- dim(loc)[1]
    location <- c(rep("0", loc_num))
    for(j in seq(1:loc_num))
      location[j] <- loc[j,]
    
    donor_sites <- c(rep(0,loc_num))
    for(k in seq(1:loc_num))
      donor_sites[k] <- as.numeric(str_sub(location[k], start = 3))
    
    seq <- paste0(dat[3:length(dat)],sep = "", collapse = "") #training = 9
    
    for(l in seq(1:loc_num))
    {
      if(donor_sites[l] - 10 >1 && donor_sites[l] + 10 < nchar(seq) )
      {
        seq.donor <- as.character(str_sub(seq, start = donor_sites[l]-10 , end = donor_sites[l] + 10))
        Donor_Seq <- rbind(Donor_Seq, seq.donor)
      }
      
    }
  }
  return(Donor_Seq)
}

Donor_Seqs <- Donor_extract(list.file, len)
Donor_Seqs <- Donor_Seqs[2:length(Donor_Seqs),]

Acceptor_extract <- function(list.file, len)
{
  Acceptor_Seq <- matrix(ncol = 1)
  for(i in seq(1:len))
  {
    file_name <- paste0("./Testing Set/", list.file[i], sep = "", collapse = "")
    getwd()
    
    dat <- scan(file_name, what = "",sep = "",na.strings = "NA")
    pattern <- "[0-9]*\\.\\."
    loc = dat[2] #training = 8
    loc <- as.data.frame(str_match_all(loc,pattern))
    loc_num <- dim(loc)[1]
    location <- c(rep("0", loc_num))
    for(j in seq(1:loc_num))
      location[j] <- loc[j,]
    
    acceptor_sites <- c(rep(0,loc_num))
    for(k in seq(1:loc_num))
      acceptor_sites[k] <- as.numeric(str_sub(location[k], end = -2))
    
    seq <- paste0(dat[3:length(dat)],sep = "", collapse = "")
    
    for(l in seq(1:loc_num))
    {
      if(acceptor_sites[l] - 10 >1 && acceptor_sites[l] + 10 < nchar(seq))
      {
        seq.acceptor <- as.character(str_sub(seq, start = acceptor_sites[l]-10 , end = acceptor_sites[l] + 10))
        Acceptor_Seq <- rbind(Acceptor_Seq, seq.acceptor)
      }
    }
    
  }
  return(Acceptor_Seq)
}

Acceptor_Seqs <- Acceptor_extract(list.file,len)
Acceptor_Seqs <- Acceptor_Seqs[2:length(Acceptor_Seqs),]

##对Donor和Acceptor进行调整

donor.seq <- as.data.frame(Donor_Seqs)
acceptor.seq <- as.data.frame(Acceptor_Seqs)


Other_extract <- function(list.file, len)
{
  Other_Seq <- matrix(ncol = 1)
  for(i in seq(1:len))
  {
    file_name <- paste0("./Testing Set/", list.file[i], sep = "", collapse = "")
    getwd()
    
    dat <- scan(file_name, what = "",sep = "",na.strings = "NA")
    pattern <- "[0-9]+"
    loc = dat[2]
    loc <- as.data.frame(str_match_all(loc,pattern))
    loc_num <- dim(loc)[1]
    feature_sites <- c(rep(0, loc_num))
    for(j in seq(1:loc_num))
      feature_sites[j] <- as.numeric(loc[j,])
    
    other_sites <-c(rep(0,loc_num-1))
    for(h in seq(1:loc_num - 1))
      other_sites[h] <- ceiling((feature_sites[h] + feature_sites[h+1])/2)
    
    
    seq <- paste0(dat[3:length(dat)],sep = "", collapse = "")
    
    for(l in seq(1:loc_num-1))
    {
      #if(other_sites[l] >11 && other_sites[l] < nchar(seq)-10)
      {
        seq.other <- as.character(str_sub(seq, start = other_sites[l]-10 , end = other_sites[l] + 10))
        Other_Seq <- rbind(Other_Seq, seq.other)
      }
    }
    
  }
  return(Other_Seq)
}

Other_Seq <- Other_extract(list.file, len)
other.seq <- as.data.frame(Other_Seq)
other.seq <- na.omit(other.seq)

other.seq <- other.seq[other.seq != ""]
other.seq <- as.data.frame(other.seq)

colnames(donor.seq) <- "Seq..features"
colnames(acceptor.seq) <- "Seq..features"
colnames(other.seq) <- "Seq..features"


seq.feature <- rbind(donor.seq, acceptor.seq, other.seq)
IsDonor <- c(rep(1, dim(donor.seq)[1]), rep(0, dim(acceptor.seq)[1] + dim(other.seq)[1]))
IsAcceptor <- c(rep(0, dim(donor.seq)[1]), rep(1, dim(acceptor.seq)[1]), rep(0, dim(other.seq)[1]))
seq.feature <- cbind(seq.feature, IsDonor, IsAcceptor)

testing.feature <- seq.feature[1:10000,]
dir.create("./my_features")
write.table(testing.feature, file = "./my_features/testing set.csv", col.names = T,
            sep = ',')