#PressedSIMS <- function()
#{
#Create and name toplevel window ==============================================#
require(tcltk)
fontHeading <- tkfont.create(family="times",size=18,weight="bold")
fontTextLabel <- tkfont.create(family="times",size=14)
tt <- tktoplevel()                                                              # Create a new toplevel window; Note this window is called tt (could create other windows with different names)
tktitle(tt) <- "SOLOMON: Parentage Analysis"                                    # Name the window
heading <- tklabel(tt, text="SOLOMON: Create Simulated Data Sets",font=fontHeading)         # add a heading
tkgrid(heading, columnspan=5)
tkgrid(tklabel(tt,text="     "))                                                #add a blank line
#Set working directory==========================================================#
label_working.directory <- tklabel(tt, text="Set working directory (use forward slash):",font=fontTextLabel)
Name <- tclVar("C:/SOLOMON")
entry.label1 <- tkentry(tt, width="20",textvariable=Name)                       #create entry fields
OnOK <- function() {
	NameVal <- tclvalue(Name)
	msg <- paste("You have now set the working directory to",NameVal)
	tkmessageBox(message=msg)
	assign("directory", NameVal, envir = .GlobalEnv)
	setwd(NameVal)
  }
OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
tkbind(entry.label1, "<Return>",OnOK)
tkgrid(tklabel(tt,text="     "), label_working.directory, tklabel(tt,text="     "), entry.label1, tklabel(tt,text="     "), OK.but, tklabel(tt,text="     "))
tkfocus(tt)
tkgrid(tklabel(tt,text="     "))
tkgrid.configure(label_working.directory, sticky="w")
tkgrid.configure(entry.label1, sticky="w")
#Set Number of Parents==========================================================#
label_sim.number <- tklabel(tt, text="Number of wanted parents:",font=fontTextLabel)
Name.sim <- tclVar()
entry.label.sim <- tkentry(tt, width="10",textvariable=Name.sim)                #create entry fields
OnOK2 <- function() {
	NameVal <- tclvalue(Name.sim)
	msg <- paste("You have now set the number of wanted parents to",NameVal)
	tkmessageBox(message=msg)
	assign("Nparents", NameVal, envir = .GlobalEnv)
}
OK.but2 <-tkbutton(tt,text="   OK   ",command=OnOK2)
tkbind(entry.label1, "<Return>",OnOK2)
tkgrid(tklabel(tt,text="     "), label_sim.number, tklabel(tt,text="     "), entry.label.sim, tklabel(tt,text="     "), OK.but2, tklabel(tt,text="     "))
tkfocus(tt)
tkgrid(tklabel(tt,text="     "))
tkgrid.configure(label_sim.number, sticky="w")
tkgrid.configure(entry.label.sim, sticky="w")
#Set Number of offspring per pair===============================================#
label_sim.number2 <- tklabel(tt, text="Number of offspring per parent:",font=fontTextLabel)
Name.sim2 <- tclVar()
entry.label.sim2 <- tkentry(tt, width="10",textvariable=Name.sim2)              #create entry fields
OnOK22 <- function()  {
	NameVal <- tclvalue(Name.sim2)
	msg <- paste("You have now set the number of offspring per parent to",NameVal)
	tkmessageBox(message=msg)
	assign("Noffs_perpair", NameVal, envir = .GlobalEnv)
}
OK.but22 <-tkbutton(tt,text="   OK   ",command=OnOK22)
tkbind(entry.label.sim2, "<Return>",OnOK2)
tkgrid(tklabel(tt,text="     "), label_sim.number2, tklabel(tt,text="     "), entry.label.sim2, tklabel(tt,text="     "), OK.but22, tklabel(tt,text="     "))
tkfocus(tt)
tkgrid(tklabel(tt,text="     "))
tkgrid.configure(label_sim.number2, sticky="w")
tkgrid.configure(entry.label.sim2, sticky="w")
#Set genotyping error rate======================================================#
label_sim.number3 <- tklabel(tt, text="Genotyping error rate (eg 0.01):",font=fontTextLabel)
Name.sim3 <- tclVar()
entry.label.sim3 <- tkentry(tt, width="10",textvariable=Name.sim3)              #create entry fields
OnOK23 <- function()  {
	NameVal <- tclvalue(Name.sim3)
	msg <- paste("You have now set the genotyping error rate to",NameVal)
	tkmessageBox(message=msg)
	assign("error", NameVal, envir = .GlobalEnv)
  }
OK.but23 <-tkbutton(tt,text="   OK   ",command=OnOK23)
tkbind(entry.label.sim3, "<Return>",OnOK23)
tkgrid(tklabel(tt,text="     "), label_sim.number3, tklabel(tt,text="     "), entry.label.sim3, tklabel(tt,text="     "), OK.but23, tklabel(tt,text="     "))
tkfocus(tt)
tkgrid(tklabel(tt,text="     "))
tkgrid.configure(label_sim.number3, sticky="w")
tkgrid.configure(entry.label.sim3, sticky="w")
#Set Number of unrelated individuals============================================#
label_sim.number4 <- tklabel(tt, text="Number of unrelated individuals:",font=fontTextLabel)
Name.sim4 <- tclVar()
entry.label.sim4 <- tkentry(tt, width="10",textvariable=Name.sim4)              #create entry fields
OnOK24 <- function() {
	NameVal <- tclvalue(Name.sim4)
	msg <- paste("You have now set the number of unrelated individuals to",NameVal)
	tkmessageBox(message=msg)
	assign("Nunrelated", NameVal, envir = .GlobalEnv)
}
OK.but24 <-tkbutton(tt,text="   OK   ",command=OnOK24)
tkbind(entry.label.sim4, "<Return>",OnOK24)
tkgrid(tklabel(tt,text="     "), label_sim.number4, tklabel(tt,text="     "), entry.label.sim4, tklabel(tt,text="     "), OK.but24, tklabel(tt,text="     "))
tkfocus(tt)
tkgrid(tklabel(tt,text="     "))
tkgrid.configure(label_sim.number4, sticky="w")
tkgrid.configure(entry.label.sim4, sticky="w")
#Set Number of siblings============================================#
label_sib.number4 <- tklabel(tt, text="Number of full-siblings:",font=fontTextLabel)
Name.sib4 <- tclVar()
entry.label.sib4 <- tkentry(tt, width="10",textvariable=Name.sib4)              #create entry fields
OnOK24 <- function() {
	NameVal <- tclvalue(Name.sib4)
	msg <- paste("You have now set the number of full-siblings to",NameVal)
	tkmessageBox(message=msg)
	assign("Nsibs", NameVal, envir = .GlobalEnv)
}
OK.but24 <-tkbutton(tt,text="   OK   ",command=OnOK24)
tkbind(entry.label.sib4, "<Return>",OnOK24)
tkgrid(tklabel(tt,text="     "), label_sib.number4, tklabel(tt,text="     "), entry.label.sib4, tklabel(tt,text="     "), OK.but24, tklabel(tt,text="     "))
tkfocus(tt)
tkgrid(tklabel(tt,text="     "))
tkgrid.configure(label_sib.number4, sticky="w")
tkgrid.configure(entry.label.sib4, sticky="w")
#Load Allele frequency==========================================================#
getfile <- function(){
  fileName <- tclvalue(tkgetOpenFile())
  if (!nchar(fileName)) {
  tkmessageBox(message = "No file was selected!")
  } else {
  tkmessageBox(message = paste("The file selected was", fileName))
  }
  Adults <- read.table(fileName, header=T, sep="\t", na.strings="-1", dec=".", strip.white=TRUE)
  assign("ALLELEFREQS", Adults, envir = .GlobalEnv)
}
adults.button <- tkbutton(tt, text = "Select Allele Frequency File", command = getfile)
adults_label <- tklabel(tt, text="Please select file containing allele frequencies:",font=fontTextLabel)
tkgrid(tklabel(tt,text="     "),adults_label,tklabel(tt,text="     "),adults.button)
tkgrid(tklabel(tt,text="     "))                                                #add a blank line
tkgrid.configure(adults_label, sticky="w")
tkgrid.configure(adults.button,sticky="w")
#Create button to run parentage script==========================================#
PressedOK <- function()      {
Nparents=as.numeric(Nparents)
Noffs_perpair=as.numeric(Noffs_perpair)
error=as.numeric(error)
Nunrelated=as.numeric(Nunrelated)
Nadults=Nparents*2                                                              #here to be used as the number of breeders (2* the total number of pairs and number of offspring)
afreqs <- ALLELEFREQS
OUT=NULL
sims=function(sims)
{
alleles2=afreqs[which(afreqs[,1]==z),]
alleles3=cbind(100:(99+(length(alleles2[,1]))),alleles2[,-1])                   #table allele frequencies
homos=(alleles3[,2])^2                                                          #create homozygote allele frequencies
homos2=cbind(as.character(alleles3[,1]),as.character(alleles3[,1]),homos)
hets=t(combn(alleles3[,2],2))                                                   #create heterozygote allele frequencies
hetfreq=2*(hets[,1]*hets[,2])
hetvals=t(combn(as.character(alleles3[,1]),2))                                  #create heterozygote allele names
hets2=cbind(hetvals,hetfreq)
gfreqs=rbind(hets2,homos2)                                                      #combine hets and homos and create genotypes
n=1000000                                                                       #sample size of all simulated genotypes (customized to indidvidual data sets) #plus 1000 is to make up for shorter simulated datsets
gfreqs1=rep(gfreqs[,1],(n*(as.numeric(gfreqs[,3]))))                            #create genotypes(by coloumn, 1 for each allele)
gfreqs2=rep(gfreqs[,2],(n*(as.numeric(gfreqs[,3]))))
gtypes=cbind(gfreqs1,gfreqs2)
gtypes=gtypes[sample(1:length(gtypes[,1]),replace=F),]
sg1=gtypes[sample(1:length(gtypes[,1]),Nadults),]
OUT<<-cbind(OUT,sg1)
}
z=length(unique(afreqs[,1]))
C1=for(z in 1:z) {lapply(z,sims)}

parents=OUT
c=c(1:(ncol(OUT)))
odd=2*(unique(round(((c-2))/2)))+1
l=length(odd) * 1000
codes=seq(from=1,to=l,by=1000)
cols=sort(rep(codes,2))-1
Anumbs=matrix(cols,Nadults,ncol(OUT),byrow=T)
parents=as.numeric(parents)+Anumbs
#create full sib families (go down the list in pair)============================#
OUT2=NULL
sims=function(sims)  {
p1=parents[z,]
p2=parents[z+1,]
als=rep(1:2,length(p1)/2)
Noffs=Noffs_perpair                                                             #number of offspring per pair
OUT2=NULL
for (b in 1:Noffs){
pos1=sample(als,length(p1)/2,replace=TRUE)                                      #note that this captures the variance, could just create the 4 genotypes in equal frequencies if you dont want that variance
pos2=sample(als,length(p1)/2,replace=TRUE)
pos11=pos1+(seq(0,(length(p1)-1),2))
pos22=pos2+(seq(0,(length(p2)-1),2))
o1=p1[pos11]
o2=p2[pos22]
o3=sort(c(o1,o2))
o3=t(c(z,o3))
write.table(o3,file="SimOffs.txt",row.names=FALSE,col.names=F,sep="\t",append=T)
}
}
z=length(parents[,1])
C1= for(z in (2*(unique(round((1:(z-2))/2)))+1)) lapply(z,sims)
Dads=parents[seq(from=1,to=length(parents[,1]),by=2),]
Moms=parents[seq(from=2,to=length(parents[,1]),by=2),]
Anumbs=matrix(cols,Nadults,ncol(OUT),byrow=T)                                   #see code before functions for adding 1000s (here am removing 1000s)
parents=as.numeric(parents)-Anumbs
Dads=parents[seq(from=1,to=length(parents[,1]),by=2),]
Moms=parents[seq(from=2,to=length(parents[,1]),by=2),]
Offs2 <- read.table("SimOffs.txt", header=F, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Offs = as.matrix(Offs2[,-1])
Anumbs=matrix(cols,length(Offs[,1]),ncol(OUT),byrow=T)
Offs=as.numeric(Offs)-Anumbs
if (Noffs_perpair>1){
Offnames=ceiling(Offs2[,1]/2)                                                   #naming of offpsring
Offnames2=paste(Offnames,".",1:length(Offnames))
Offs=cbind(paste("Offspring",Offnames2),Offs)} else {
Offnames=ceiling(Offs2[,1]/2)
Offs=cbind(paste("Offspring",Offnames),Offs)}
Moms=cbind(paste("Mom",1:length(Moms[,1])),Moms)
Dads=cbind(paste("Dad",1:length(Dads[,1])),Dads)
#add error======================================================================#
Dadsg=Dads[,-1]
ldad=length(Dadsg)*error
pdad1=sample(1:length(Dadsg),ldad,replace=FALSE)
pdad2=Dadsg[sample(1:length(Dadsg),ldad,replace=FALSE)]
Dadsg2=replace(Dadsg,pdad1,pdad2)
Dads=cbind(Dads[,1],Dadsg2)

Offsg=Offs[,-1]
loff=length(Offsg)*error
poff1=sample(1:length(Offsg),loff,replace=FALSE)
poff2=Offsg[sample(1:length(Offsg),loff,replace=FALSE)]
Offsg2=replace(Offsg,poff1,poff2)
Offs=cbind(Offs[,1],Offsg2)

#Momsg=Moms[,-1]
#ldad=length(Momsg)*error
#pdad1=sample(1:length(Momsg),ldad,replace=FALSE)
#pdad2=Momsg[sample(1:length(Momsg),ldad,replace=FALSE)]
#Momsg2=replace(Momsg,pdad1,pdad2)
#Moms=cbind(Moms[,1],Momsg2)
#===============================================================================#
colsnmz=rep("Locus",ncol(Offs)-1)
colsnmz2=sort(rep(1:(length(colsnmz)/2),2))
colsnmz3=paste(colsnmz,colsnmz2)
colsnmz3=c("IDs",colsnmz3)
colnames(Offs)<- colsnmz3
colnames(Dads)<- colsnmz3
colnames(Moms)<- colsnmz3
#Add unrealated individuals=====================================================#
if (Nunrelated>0){
Nadults=Nunrelated*3                                                            #here to be used as the number of breeders (2* the total number of pairs and number of offspring)
afreqs <- ALLELEFREQS
OUT=NULL
sims=function(sims) {
alleles2=afreqs[which(afreqs[,1]==z),]
alleles3=cbind(100:(99+(length(alleles2[,1]))),alleles2[,-1])                   #table allele frequencies
homos=(alleles3[,2])^2                                                          #create homozygote allele frequencies
homos2=cbind(as.character(alleles3[,1]),as.character(alleles3[,1]),homos)
hets=t(combn(alleles3[,2],2))                                                   #create heterozygote allele frequencies
hetfreq=2*(hets[,1]*hets[,2])
hetvals=t(combn(as.character(alleles3[,1]),2))                                  #create heterozygote allele names
hets2=cbind(hetvals,hetfreq)
gfreqs=rbind(hets2,homos2)                                                      #combine hets and homos and create genotypes
n=1000000                                                                       #sample size of all simulated genotypes (customized to indidvidual data sets) #plus 1000 is to make up for shorter simulated datsets
gfreqs1=rep(gfreqs[,1],(n*(as.numeric(gfreqs[,3]))))                            #create genotypes(by coloumn, 1 for each allele)
gfreqs2=rep(gfreqs[,2],(n*(as.numeric(gfreqs[,3]))))
gtypes=cbind(gfreqs1,gfreqs2)
gtypes=gtypes[sample(1:length(gtypes[,1]),replace=F),]
sg1=gtypes[sample(1:length(gtypes[,1]),Nadults),]
OUT<<-cbind(OUT,sg1)
}
z=length(unique(afreqs[,1]))
C1=for(z in 1:z) {lapply(z,sims)}

parents=OUT
c=c(1:(ncol(OUT)))
odd=2*(unique(round(((c-2))/2)))+1
l=length(odd) * 1000
codes=seq(from=1,to=l,by=1000)
cols=sort(rep(codes,2))-1
Anumbs=matrix(cols,Nadults,ncol(OUT),byrow=T)
parents=as.numeric(parents)+Anumbs
parents=as.numeric(parents)-Anumbs
unrelated=cbind(paste("Individual",1:length(parents[,1])),parents)
Lid=length(unrelated[,1])/3
m1=unrelated[1:Lid,]
d1=unrelated[(Lid+1):(Lid*2),]
j1=unrelated[((Lid*2)+1):(Lid*3),]
colsnmz=rep("Locus",ncol(Offs)-1)
colsnmz2=sort(rep(1:(length(colsnmz)/2),2))
colsnmz3=paste(colsnmz,colsnmz2)
colsnmz3=c("IDs",colsnmz3)
colnames(Offs)<- colsnmz3
colnames(Dads)<- colsnmz3
colnames(Moms)<- colsnmz3
Moms=rbind(Moms,m1)
Dads=rbind(Dads,d1)
Offs=rbind(Offs,j1)
}
write.table(Dads,file="Dads_sim.txt",row.names=FALSE,col.names=TRUE,sep="\t",append=FALSE)
write.table(Moms,file="Moms_sim.txt",row.names=FALSE,col.names=TRUE,sep="\t",append=FALSE)
write.table(Offs,file="Juveniles_sim.txt",row.names=FALSE,col.names=TRUE,sep="\t",append=FALSE)
unlink("SimOffs.txt")
#Begin add siblings=============================================================#
#This scripts creates a desired number of siblings and splits them between adults and offspring files
#setwd("C:/POPS/")
#ALLELEFREQS<- read.table("Steelhead_freqs.txt", header=TRUE, sep="\t", na.strings="?",dec=".", strip.white=TRUE)
Nsibs=as.numeric(Nsibs)
#error=0.01
if (Nsibs > 0) {
Noffs_per_pair=2                                                                #could change this, but as stands only evaluating 1 pair of siblings per parent-pair
Nadults=Nsibs*2                                                                 #here to be used as the number of breeders (2* the total number of pairs and number of offspring)
afreqs <- ALLELEFREQS
OUT=NULL
sims=function(sims)
{
alleles2=afreqs[which(afreqs[,1]==z),]
alleles3=cbind(100:(99+(length(alleles2[,1]))),alleles2[,-1])                   #table allele frequencies
homos=(alleles3[,2])^2                                                          #create homozygote allele frequencies
homos2=cbind(as.character(alleles3[,1]),as.character(alleles3[,1]),homos)
hets=t(combn(alleles3[,2],2))                                                   #create heterozygote allele frequencies
hetfreq=2*(hets[,1]*hets[,2])
hetvals=t(combn(as.character(alleles3[,1]),2))                                  #create heterozygote allele names
hets2=cbind(hetvals,hetfreq)
gfreqs=rbind(hets2,homos2)                                                      #combine hets and homos and create genotypes
n=1000000                                                                       #sample size of all simulated genotypes (customized to indidvidual data sets) #plus 1000 is to make up for shorter simulated datsets
gfreqs1=rep(gfreqs[,1],(n*(as.numeric(gfreqs[,3]))))                            #create genotypes(by coloumn, 1 for each allele)
gfreqs2=rep(gfreqs[,2],(n*(as.numeric(gfreqs[,3]))))
gtypes=cbind(gfreqs1,gfreqs2)
gtypes=gtypes[sample(1:length(gtypes[,1]),replace=F),]
sg1=gtypes[sample(1:length(gtypes[,1]),Nadults),]
OUT<<-cbind(OUT,sg1)
}
z=length(unique(afreqs[,1]))
C1=for(z in 1:z) {lapply(z,sims)} 
parents=OUT
c=c(1:(ncol(OUT)))
odd=2*(unique(round(((c-2))/2)))+1
l=length(odd) * 1000
codes=seq(from=1,to=l,by=1000)
cols=sort(rep(codes,2))-1
Anumbs=matrix(cols,Nadults,ncol(OUT),byrow=T)
parents=as.numeric(parents)+Anumbs
#create full sib families (go down the list in pair)============================#
OUT2=NULL
sims=function(sims)  {
N=1:length(parents[,1])
u=sample(N,1)
u2=sample(N,1)
p1=parents[u,]
p2=parents[u2,]
als=rep(1:2,length(p1)/2)
Noffs=Noffs_per_pair                                                            #number of offspring per pair
OUT2=NULL
for (b in 1:Noffs){
pos1=sample(als,length(p1)/2,replace=TRUE)                                      #note that this captures the variance, could just create the 4 genotypes in equal frequencies if you dont want that variance
pos2=sample(als,length(p1)/2,replace=TRUE)
pos11=pos1+(seq(0,(length(p1)-1),2))
pos22=pos2+(seq(0,(length(p2)-1),2))
o1=p1[pos11]
o2=p2[pos22]
o3=sort(c(o1,o2))
o3=t(c(z,o3))
write.table(o3,file="SimOffs.txt",row.names=FALSE,col.names=F,sep="\t",append=T)
}
}
z=length(parents[,1])
C1= for(z in (2*(unique(round((1:(z-2))/2)))+1)) lapply(z,sims)                 #used to move down list, now sample randomly so is just used to produce wanted number of offspring
Dads=parents[seq(from=1,to=length(parents[,1]),by=2),]
Moms=parents[seq(from=2,to=length(parents[,1]),by=2),]
Anumbs=matrix(cols,Nadults,ncol(OUT),byrow=T)                                   #see code before functions for adding 1000s (here am removing 1000s)
parents=as.numeric(parents)-Anumbs
Dads=parents[seq(from=1,to=length(parents[,1]),by=2),]
Moms=parents[seq(from=2,to=length(parents[,1]),by=2),]
Offs2 <- read.table("SimOffs.txt", header=F, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Offs = as.matrix(Offs2[,-1])
Anumbs=matrix(cols,length(Offs[,1]),ncol(OUT),byrow=T)
Offs=as.numeric(Offs)-Anumbs
if (Noffs_per_pair>1){
Offnames=ceiling(Offs2[,1]/2)                                                   #naming of offpsring
Offnames2=paste(Offnames,".",1:length(Offnames))
Offs=cbind(paste("Sibling",Offnames2),Offs)} else {
Offnames=ceiling(Offs2[,1]/2)
Offs=cbind(paste("Sibling",Offnames),Offs)}
Moms=cbind(paste("Mom",1:length(Moms[,1])),Moms)
Dads=cbind(paste("Dad",1:length(Dads[,1])),Dads)
#add error======================================================================#
Offsg=Offs[,-1]
loff=length(Offsg)*error
poff1=sample(1:length(Offsg),loff,replace=FALSE)
poff2=Offsg[sample(1:length(Offsg),loff,replace=FALSE)]
Offsg2=replace(Offsg,poff1,poff2)
Offs=cbind(Offs[,1],Offsg2)
#===============================================================================#
colsnmz=rep("Locus",ncol(Offs)-1)
colsnmz2=sort(rep(1:(length(colsnmz)/2),2))
colsnmz3=paste(colsnmz,colsnmz2)
colsnmz3=c("IDs",colsnmz3)
colnames(Offs)<- colsnmz3
#calculate shared alleles among pairs of siblings===============================#
sib1=Offs[seq(from=1,to=length(Offs[,1]),by=2),]
sib2=Offs[seq(from=2,to=length(Offs[,1]),by=2),]  
write.table(sib1,file="Sib1.txt",row.names=FALSE,col.names=TRUE,sep="\t",append=FALSE)
write.table(sib2,file="Sib2.txt",row.names=FALSE,col.names=TRUE,sep="\t",append=FALSE)

dadsibs <- read.table("Dads_sim.txt", header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
sib1 <- read.table("Sib1.txt", header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
dadsibs=rbind(dadsibs,sib1)
write.table(dadsibs,file="Dads_sim.txt",row.names=FALSE,col.names=TRUE,sep="\t",append=FALSE)

juvsibs <- read.table("Juveniles_sim.txt", header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
sib2 <- read.table("Sib2.txt", header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
juvsibs=rbind(juvsibs,sib2)
write.table(juvsibs,file="Juveniles_sim.txt",row.names=FALSE,col.names=TRUE,sep="\t",append=FALSE)
unlink("Sib1.txt")
unlink("Sib2.txt")
unlink("SimOffs.txt")
}
}
label.bayes <- tklabel(tt, text="Press 'Run' to create test data sets:",font=fontTextLabel)
run.button <- tkbutton(tt, text = "Run", command = PressedOK)
tkgrid(tklabel(tt,text="     "),label.bayes,tklabel(tt,text="     "),run.button,tklabel(tt,text="     "))		# Place the button on the window
tkfocus(tt)
tkgrid.configure(label.bayes, sticky="w")
tkgrid.configure(run.button, sticky="w")
tkgrid(tklabel(tt,text="     "))
tkgrid(tklabel(tt,text="     "))
#}
#label.SIMS <- tklabel(tt, text="Create test data sets:",font=fontTextLabel)
#OK.but.SIMS <- tkbutton(tt, text = "SIMS", command = PressedSIMS)
#tkgrid(tklabel(tt,text="     "),label.SIMS,tklabel(tt,text="     "),OK.but.SIMS,tklabel(tt,text="     "))		# Place the button on the window
#tkgrid.configure(label.SIMS, sticky="w")
#tkgrid.configure(OK.but.SIMS, sticky="w")
#tkfocus(tt)