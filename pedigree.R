
library(readr)

setwd("C:/Users/cicek/Desktop/vize_proje")
p_data <- read_delim("Pedigri_3.dat","\t", escape_double = FALSE, trim_ws = TRUE)
View(p_data)

#nxn kinship matrix 
#n is number of individuals
akrabalik_matrisi <- matrix(0,nrow = nrow(p_data),ncol = nrow(p_data))

#rows and columns of kinship matrix
rownames(akrabalik_matrisi) <- rownames(p_data)
colnames(akrabalik_matrisi) <- rownames(p_data)

ana_no <- function(birey)
{
  ana_no <- p_data[birey,"AnaNo"]
  return(as.numeric(ana_no))
}
baba_no <- function(birey)
{
  baba_no <- p_data[birey,"BabaNo"]
  return(as.numeric(baba_no))
}

for(i in 1:nrow(p_data)) #row
{
  for(j in 1:(i-1)) #column
  {
    if(is.na(baba_no(i)) == TRUE && is.na(ana_no(i)) == TRUE) #create unit matrix
    {
      akrabalik_matrisi[i,j] <- 0
      akrabalik_matrisi[j,i] <- 0
      akrabalik_matrisi[i,i] <- 1
    }
    else
    {
      akrabalik_matrisi[i,j] = (0.5)*(akrabalik_matrisi[j,baba_no(i)] + akrabalik_matrisi[j,ana_no(i)])
      akrabalik_matrisi[j,i] = (0.5)*(akrabalik_matrisi[j,baba_no(i)] + akrabalik_matrisi[j,ana_no(i)])
      akrabalik_matrisi[i,i] = 1 + (0.5)*(akrabalik_matrisi[baba_no(i),ana_no(i)])

    }
  }
}

View(akrabalik_matrisi)














