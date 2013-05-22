library (tuneR)
library (seewave)

args = commandArgs()

totaltime <- 61.15

starts <- seq(from = 0, to = totaltime - 1 , by=1)
stops <- seq (from = 2, to = totaltime, by = 1)

startsb <- seq(from = 0.001, to = totaltime - 1.001 , by = 1)
stopsb <- seq (from = 1.001, to = totaltime, by = 1)

startsc <- seq(from = 0.002, to = totaltime - 1.002 , by = 1)
stopsc <- seq(from = 1.002, to = totaltime, by = 1)

startsd <- seq(from = 0.003, to = totaltime - 1.003 , by = 1)
stopsd <- seq(from = 1.003, to = totaltime, by = 1)

#Values for computation
Flatbirds <- 2000
Sharpbirds <- 6000
samplerate <- 22050
Threshold <- .1
songcomb <- .2*samplerate
MidFreq <- 3000
Temperature <- 20
Dist <- 1
cushion <- .05

songtimes <- function(mic,Flatbirds,Sharpbirds,samplerate,Threshold,songcomb) {
	#Custom function
	#Filter out the unused frequencies and output in useful form
	mic <-ffilter(mic,from=Flatbirds,to=Sharpbirds, output = "matrix")
	#amplitude time envelope the remaining signal into a string of amplitude values
	#using a hilbert transform and a bit of smoothing
	mis1 <- env(mic, f = samplerate, envt = "hil", ksmooth=kernel("daniell",100), plot = FALSE)
	#kill the old signal
	mic <-NULL
	#make a dataframe to put the values in
	flag1 <- data.frame(matrix(NA, nrow = length(mis1), ncol = 2))
	#put the values in
	flag1[,1] <- mis1
	#and a time stamp
	flag1[,2] <- seq(length(mis1))
	#kill the old file
	mis1 <- NULL
	#get the highest amplitude
	AMP <- max(flag1[,1])
	#Detection Threshold
	Thresh <- Threshold*AMP
	#Now throw out all the values less than the threshold
	flag1 <- subset (flag1,subset = X1 > Thresh)
	#set things up to clear out all the ones that are from the same song
	#do this by making an offset data frame
	flagg1 <- data.frame(matrix(NA, nrow = length(flag1[,1])+1, ncol = 3))
	#at this point the actual values aren't important anymore just the timestamps
	#first row has an inserted first time value
	flagg1[,1] <- c(NA,flag1[,2])
	#second row has an inserted last time value
	flagg1[,2] <- c(flag1[,2],NA)
	#kill flag1
	flag1 <- NULL
	#get the difference between them
	flagg1[,3] <- abs(flagg1[,2]-flagg1[,1])
	songstamps <- subset (flagg1, X3 > songcomb, select = X2)
	#kill flagg1
	flagg1 <- NULL
	#Then we need to put that into seconds
	#This leaves the timestamps of all the songs
	songstamps <- songstamps/samplerate
	songstamps
}

#Custom function for time lags based on cross correlation function
#requires data to be in a time series
#returns autocorrelation (how good of a fit) and the time lag
Max_CCF<- function(First,Second) {
	cross <- ccf(First, Second, plot = FALSE)
	cor = cross$acf[,,1]
	lag = cross$lag[,,1]
	res = data.frame(cor,lag)
	res_max = res[which.max(res$cor),]
	return(res_max)
}

#Audio

for (d in 1:length(starts)) {

	#Use the function built for song times to get times for each mic
	mic1 <- readWave(args[4], from = starts[d], to = stops[d], units = "minutes")
	one <- songtimes(mic1,Flatbirds,Sharpbirds,samplerate,Threshold,songcomb)
	#kill mic1
	mic1 <- NULL
	mic2 <- readWave(args[5], from = startsb[d], to = stopsb[d], units = "minutes")
	two <- songtimes(mic2,Flatbirds,Sharpbirds,samplerate,Threshold,songcomb)
	#kill mic2
	mic2 <- NULL
	mic3 <- readWave(args[6], from = startsc[d], to = stopsc[d], units = "minutes")
	three <- songtimes(mic3,Flatbirds,Sharpbirds,samplerate,Threshold,songcomb)
	#kill mic3
	mic3 <- NULL
	mic4 <- readWave(args[7], from = startsd[d], to = stopsd[d], units = "minutes")
	four <- songtimes(mic4,Flatbirds,Sharpbirds,samplerate,Threshold,songcomb)
	mic4 <- NULL

	#combine them into one dataframe
	#by first getting the size needed
	size <- max(c(length(one[,1]),length(two[,1]),length(three[,1]),length(four[,1])))
	#making the dataframe
	songs <- data.frame(matrix(NA, nrow = size, ncol = 4))

	#fill dataframe with song times
	#somewhat non-trivially since they may have unequal lengths,
	#but this is the fastest way I've made
	songs[1:length(one[,1]),1] <- one[,1]
	songs[1:length(two[,1]),2] <- two[,1]
	songs[1:length(three[,1]),3] <- three[,1]
	songs[1:length(four[,1]),4] <- four[,1]
	#kill song times
	one <- NULL
	two <- NULL
	three <- NULL
	four <- NULL

	# Next build a dataframe of all the songs that can realistically be
	# expected to be paired together based on their arrival times.
	# Use the speed of sound at a middle frequency at the temperature to
	# get a time window.
	Speed <- wasp(f = MidFreq, t = Temperature, c = NULL, s = NULL, d = NULL, medium = "air")
	# For long mic runs a cushion may be needed for sound falloff with
	# distance since the amplitude cutoff is determined as a proportion of the maximum
	TimeWindow <- Dist/Speed$c + cushion

	#Using one mic as a reference
	#(any one will do since a resolved song needs all four)
	#find all timestamps that could possibly be resultant from the same singer
	#based on distance and group them together
	#toss the rest

	# Can't be any more confirmed songs than potential songs so make
	# dataframe to fill that size.
	# Cull it after
	song <- data.frame(matrix(NA, nrow = size, ncol = 4))
	# Kill size
	size <- NULL

	for (a in 1:length(songs[,1])) {
		#get time differences for every value to the reference mics time in question
		absV2 <- abs(songs[,2]-songs[a,1])
		absV3 <- abs(songs[,3]-songs[a,1])
		absV4 <- abs(songs[,4]-songs[a,1])
		Two <- subset(songs[,2], subset = absV2 < TimeWindow)
		Three <- subset(songs[,3], subset = absV2 < TimeWindow)
		Four <- subset(songs[,4], subset = absV2 < TimeWindow)
		if(length(Two) >= 1 & length(Three) >= 1 & length(Four) >= 1) {	
			song[a,] <-c(songs[a,1],Two,Three,Four)
		}
	}

	#at the end drop the empties
	song <- song[complete.cases(song),]

	# This leaves all the resolvable time stamps
	# What is wanted is to do the cross correlations to build a time delay matrix
	# Times for the cross correlations
	Times <- apply(song,FUN = min, MARGIN =1)

	Timedelays <- data.frame(matrix(NA, nrow = length(song[,1]), ncol = 4))
	DelayP <- data.frame(matrix(NA, nrow = length(song[,1]), ncol = 4))
	Timedelays[,1] <- Times
	DelayP[,1] <- Times
	for(b in 1:length(Times)) {
		#Next two are commented out since the wav file right now is just the test file
		One <- readWave(args[4], from = Times[b], to = Times[b]+1,
				units = "seconds")
		Two <- readWave(args[5], from = Times[b]+.001,
				to = Times[b]+1.001, units = "seconds")
		One <- cutw(One,from=0,to=1, output = "ts")
		Two <- cutw(Two,from=0,to=1, output = "ts")
		lag <- Max_CCF(One, Two)
		#Two <- NULL
		Timedelays[b,2] <- lag$lag
		DelayP[b,2] <- lag$cor
		Three <- readWave(args[6], from = Times[b]+.002,
				to = Times[b]+1.002, units = "seconds")
		Three <- cutw(Three,from=0,to=1, output = "ts")
		lag <- Max_CCF(One, Three)
		#Three <- NULL
		Timedelays[b,3] <- lag$lag
		DelayP[b,3] <- lag$cor
		Four <- readWave(args[7], from = Times[b]+.003,
				to = Times[b]+1.003, units = "seconds")
		Four <- cutw(Four,from=0,to=1, output = "ts")
		lag <- Max_CCF(One, Four)
		#Four <- NULL
		Timedelays[b,4] <- lag$lag
		DelayP[b,4] <- lag$cor
		#One <- NULL
		lag <- NULL
	}
	b <- 1
	head(Timedelays)
	head(DelayP)

	#Output the time delay matrix and respective P values as a text file
	write.csv(DelayP, file = paste("DelayP",d,".csv",sep = ""))
	write.csv(Timedelays,file = paste("Timedelays",d,".csv",sep = ""))
	Timedelays <- NULL
	DelayP <- NULL

	#Make .flac files for each interval
	#wav made,compressed and overwritten into a flac file.
	#saving song
	final <- readWave("tico2.wav", from = starts[d], to = stops[d], units = "minutes")
	savewav(final, filename = paste("Audio",d,".wav",sep = ""))
	wav2flac(paste("Audio",d,".wav", sep = ""), overwrite=TRUE)
	final <- NULL
}
