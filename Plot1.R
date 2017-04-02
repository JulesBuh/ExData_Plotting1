##Plot 1----
#> Description----
# outputs a historgram with red bars as a png file with a transparent background
# and labels

#>0 Input and prerequsites----

      #date range 2007-02-01 and 2007-02-02
      studyPeriodMin<-as.Date("2007-02-01")
      studyPeriodMax<-as.Date("2007-02-02")
      #gets the name of the subset filename if the script has been previously loaded
      subsetFilename<<-paste("household_power_consumption",
                             studyPeriodMin,
                             studyPeriodMax,
                             "subset.txt",
                             sep="-")
      #this is the name of the output graphic filename
      PlotFilename<-"Plot1.png"

# 0.0 reads the data----
      # As the file read is a lengthy process due to size, the following script
      # checks to see if a download and subset file has already been created or is
      # already within R where the script has already ran previously for one of the other
      # plot scripts)
      if(!exists("dataRead")){
            #0.1A if a subset has already been previously saved load this instead----
            if(file.exists(subsetFilename)){
                  preDeterminedColclass<-c("character",
                                           "character",
                                           "numeric",
                                           "numeric",
                                           "numeric",
                                           "numeric",
                                           "numeric",
                                           "numeric",
                                           "numeric")
                  dataRead<-read.table(subsetFilename,
                                       header=TRUE,
                                       sep=";",
                                       colClasses=preDeterminedColclass,
                                       stringsAsFactors = FALSE,
                                       na.strings = "?")
                  dataRead$Time<-as.POSIXct(dataRead$Time)
                  dataRead$Date<-as.Date(dataRead$Date)
                  rm("preDeterminedColclass")
            }
            #0.1B if starting from scratch the data needs to be read----
            if(!file.exists(subsetFilename)){
           
                  #0.2B downloads the data----
                  if(!file.exists("data.zip")){
                        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip")
                  }
                  #0.3B unzips the data----
                  filename<-unzip("data.zip",list=TRUE)[[1]]
                  
                  unzip("data.zip",filename)
                  #0.4B preparation to read the data----
                  if(!file.exists(subsetFilename)){
                        readFunction<-function(){
                        #0.4.0 Preparation for read----
                        #0.4.0.0 Sizing up the file----
                        message("please wait...")
                        #0.4.0.1 gets the number of items per line      
                        colCount<-max(count.fields(filename, sep = ";"))
                        #0.4.0.2 gets the number of lines from the file
                        rowCount<-length(readLines(filename))
                        #0.4.0.3 selects the first column within colClasses
                        colSelect<-c(rep("character", 1), rep("NULL", colCount-1))
                        
                        #0.4.1 Preparing the dataframe----
                        #0.4.1.05 predetermine the colclasses to load
                        #The dataset has 2,075,259 rows and 9 columns. 
                        #The columns are:
                              #1.Date: Date in format dd/mm/yyyy
                              #2.Time: time in format hh:mm:ss
                              #3.Global_active_power: household global minute-averaged active power (in kilowatt)
                              #4.Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
                              #5.Voltage: minute-averaged voltage (in volt)
                              #6.Global_intensity: household global minute-averaged current intensity (in ampere)
                              #7.Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
                              #8.Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
                              #9.Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.
                        preDeterminedColclass<-c("character",
                                                 "character",
                                                 "numeric",
                                                 "numeric",
                                                 "numeric",
                                                 "numeric",
                                                 "numeric",
                                                 "numeric",
                                                 "numeric")
                        
                        #0.4.1.1 gets the header of the data only
                        header<-read.table(filename,header=FALSE,sep=";",skip=0,nrows = 1,stringsAsFactors = FALSE)
                        keep<-NULL
                        #0.4.2 Initialises chunking process for read-----
                        #0.4.2.0 sets the general chunk size
                        chunkSize<-NULL
                        chunkSize$main<-2000
                        #0.4.2.1 sets the chunking positions
                        chunkPositions<-NULL
                        chunkPositions$seq<-seq(from=1, to=rowCount-1, by = chunkSize$main)
                        chunkPositions$count<-length(chunkPositions$seq)
                        chunkPositions$last<-chunkPositions$seq[chunkPositions$count]
                        chunkSize$last<-rowCount-chunkPositions$last
                        
                        chunkPositions$trackLoop<-1
                        
                        #Function to perform the read once the position of the study period has been determined
                        PerformTheRead<-function(startPos=startLine,chunkFull=ch){
                              dataRead<-   read.table(filename,
                                                      header=FALSE,
                                                      sep=";",
                                                      colClasses=preDeterminedColclass,
                                                      skip= startPos,
                                                      nrows = chunkFull,
                                                      stringsAsFactors = FALSE,
                                                      na.strings = "?")
                              #add the headers
                              names(dataRead)<-header
                              #convert the character dates to POSIXlt dates
                              dataRead$Time<-as.POSIXct(strptime(paste(dataRead$Date,dataRead$Time,sep="_"),"%d/%m/%Y_%H:%M:%S"))
                              dataRead$Date<-as.Date(strptime(dataRead$Date,"%d/%m/%Y"))
                              #subsets the chunk to only include the date range specified
                              dataRead<-subset(dataRead,dataRead$Date>=studyPeriodMin&
                                           dataRead$Date<=studyPeriodMax)
                              
                              dataRead<<-dataRead
                              write.table(dataRead,file = subsetFilename, row.names = FALSE, sep=";", na="?")
                              
                        } 
                        
                        #set chunk import counter
                        #prepare tester
                        tester<-as.logical(NULL)
                        converge<-c(as.Date("1970-01-01"))
                        increment<-as.numeric(NULL)
                        ch<-1L
                        message(paste("Expecting to process up to",chunkPositions$count,"chunks.",
                                      "\r\n",
                                      "This parsing assumes all dates are close together and stops",
                                      "after it finds a cluster within the date range specified",
                                      "\r\n",
                                      "If this is not appropriate for the dataset for example",
                                      "the dates aren't ordered sequentially",
                                      ",an alternative parsing method should be adopted"))
                        readline(paste("Press 'Enter' to continue       or       Press 'Esc' to abort"))
                        if(rowCount>500000){
                              message("This will take a while ")
                        }
                        message("please wait...")
                        
                        for(i in unlist(chunkPositions$seq)){
                              
                              #resets the chunk sample
                              chunk<-NULL
                              #reads the line
                              chunk$test$date<-read.table(filename,header=FALSE,sep=";",colClasses=colSelect,skip=i,nrows = chunkSize$main,stringsAsFactors = FALSE)
                              chunk$test$averageDate<-mean(as.Date(strptime(chunk$test$date[,1],"%d/%m/%Y")))
                              chunk$test$minDate<-min(as.Date(strptime(chunk$test$date[,1],"%d/%m/%Y")))
                              chunk$test$maxDate<-max(as.Date(strptime(chunk$test$date[,1],"%d/%m/%Y")))
                              #tester for each line
                              #chunkStudyRelationship
                              
                              #print(paste(chunk$test$minDate,studyPeriodMin,chunk$test$maxDate,studyPeriodMax))
                              if(chunk$test$maxDate < studyPeriodMin|
                                 chunk$test$minDate > studyPeriodMax){
                                    chunk$test$rel<-"outside"
                              }else{
                                    chunk$test$rel<-"inside"
                                    if((chunk$test$minDate < studyPeriodMin && chunk$test$maxDate < studyPeriodMax && chunk$test$maxDate > studyPeriodMin)|
                                       (chunk$test$minDate < studyPeriodMax && chunk$test$minDate > studyPeriodMin && chunk$test$maxDate > studyPeriodMax)){
                                          chunk$test$rel<-"overlap"
                                          print(chunk$test$rel)
                                    }
                                    if((chunk$test$minDate < studyPeriodMin && chunk$test$maxDate < studyPeriodMax && chunk$test$maxDate > studyPeriodMin)|
                                       (chunk$test$minDate < studyPeriodMin && chunk$test$maxDate > studyPeriodMax))
                                    {
                                          chunk$test$rel<-"inside"
                                          print(chunk$test$rel)
                                    }
                              }
                              
                              ifelse(chunk$test$rel=="inside"|chunk$test$rel=="overlap",tester[i]<-TRUE,tester[i]<-FALSE)
                              converge[i]<-chunk$test$averageDate
                              increment[i]<-converge[i]-converge[i-1]>=0
                              
                              if(mean(tester,na.rm=TRUE)>0&&mean(converge,na.rm=TRUE)>studyPeriodMax&&prod(increment,na.rm=TRUE)!=0){
            
                                    PerformTheRead();
                                    #0.4.9.0 Early Return----
                                    #conditions are that the tester has to have return true atleast once
                                    #the average of all chunk dates must exceed the upper end of the study period
                                    #the averge of the chunks need to have continually incremented
                                    #if these conditions are not met, the full source file is chunked and takes considerably longer to complete
                                    message(paste("The process has retrieved a cluster for the",
                                                  "date range specified and hasn't found any",
                                                  "matched dates since encountering the cluster.",
                                                  "\r\n",
                                                  "If you believe there should be more observations",
                                                  "then you should run read.table() on the full dataset",
                                                  "\r\n"))
                                    readline(paste("The records are stored in the variable called dataRead"))
                                    return(str(dataRead))
                              }
                              
                              if(tester[i]){
                                    #reads the line into keep
                                    
                                    print(paste("Found",as.integer((ch+chunkSize$main)/chunkSize$main),"chunks at line",i))
                                    if(!exists("startLine")){
                                          startLine<-i
                                    }
                                    ch<-ch+chunkSize$main
                              }
                              print(paste(chunk$test$rel,"date range. - approximate chunk date is",chunk$test$averageDate,
                                          "...",as.integer(((i+chunkSize$main)/chunkSize$main)*100/chunkPositions$count), "% - (", i,"lines read)"))
                        }
                        
                        
            
                        PerformTheRead();
                        message(paste("The process has completed the whole read",
                                      "any results returned will be stored within 'dataRead'",
                                      "and should be filtered further as it may have captured surrounding",
                                      "dates within the chunk.\r\n"))
            
                  }
                  #0.4.9 reads the data----
                  readFunction()
                  }
            }
            #0.9 Returns variable----
            #`dataRead`
      }

#>1 Function Body----
      #1.0 ensures the data is filtered to the date range----
      dataRead<-subset(dataRead,dataRead$Date>=studyPeriodMin&
                             dataRead$Date<=studyPeriodMax)
      
      #1.1 initialise the graphic device----
      # default size is already 480 x 480 px
      grDevices::png(PlotFilename,bg="transparent",antialias = "cleartype")
      
      #1.2 creates the historgram----
      graphics::hist(dataRead$Global_active_power,
                     main="Global Active Power",
                     xlab="Global Active Power (kilowatts)",
                     ylab="Frequency",
                     col="red")
      #1.9 closes the graphic device----
      grDevices::dev.off()

#>9 Returns----
      if(file.exists(PlotFilename)){
            message(PlotFilename," saved to working directory")
      }
