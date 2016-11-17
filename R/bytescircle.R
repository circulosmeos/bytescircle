#
# bytescircle
#
# by Roberto S. Galende
# port of linux' bytes-circle to R
# v1.0, Nov 2016
#
# licensed under GPL-3
#

#'  Statistics About Bytes Contained in a File as a Circle Plot
#'
#'  bytescircle is a function that shows statistics about bytes contained in a file 
#'  as a circle graph of deviations from mean in sigma increments. 
#'  Histogram and boxplot graphs can also be generated.
#'
#'  The function can be useful for statistically analyze the content of files 
#'  in a glimpse: text files are shown as a green centered crown, compressed 
#'  and encrypted files should be shown as equally distributed variations with 
#'  a very low CV (sigma/mean), and other types of files can be classified between 
#'  these two categories depending on their text vs binary content, which can be 
#'  useful to quickly determine how information is stored inside them (databases, 
#'  multimedia files, etc). 
#'  
#'  bytescircle() accepts a character string as path for the file, though if 
#'  it is not indicated, a file selection GUI will demand it.
#'  The 'ascii=TRUE' param replicates the linux behaviour of bytes-circle command
#'  with params '-o 1', or equivalently '-b', as RStudio or R output do not have 
#'  colour output.
#' 
#'  bytescircle() outputs data (file, mean, sd, CV, file size) on R console, but
#'  this can be turned off using 'output=0'. A value of 2 will output the char 
#'  array used for ascii graph output.
#' 
#'  'plot' param accepts a number from 0 (no plot) to 5 (boxplot)
#' 
#'  Colours can be indicated as a vector of colours from 1 to 3 elements, which
#'  will be used differently depending on the plot selected. By default, the
#'  first colour of the vector will replace the default green, the second the 
#'  default red, and the third the default blue. Not all colours are used on 
#'  every plot.
#' 
#'  bytescircle() can accept its own output as input using 'input=variable'.
#'  This can be useful for generating a new graph without the hassle of R reading
#'  and analysing the file again. The input can also be a bare 256 element vector:
#'  in this case each element represents the appeareances in the file of that 
#'  [n-1] byte value.
#' 
#' @param FILE char array with the path to an existing file to analyse  
#' @param ascii boolean, if TRUE R will output an ascii circle char of deviations
#'    from sigma (true sd). Each ascii char represents a different deviation from
#'    sigma. The array of chars used (from -9/4 to +9/4 sigma, in increments 
#'    of 0.5 sigma) can be printed using parameter 'output=2'  
#' @param plot number from 0 to 5, indicates plot to represent:
#' 
#'    0: no plot 
#' 
#'    1: circle of bytes: using an archimedean spiral each byte value is represented
#'       with a coloured circle which size indicates the amount of deviation from 
#'       sigma. A green colour indicates positive sigma value whilst red indicates
#'       a negative sigma value. Blue little circles represents byte values that do
#'       not appear in the file
#' 
#'    2: circle of bytes with indication of the byte bucket represented
#' 
#'    3: graph of byte counts: in green values over mean, in red values below it.
#'       Also the lines for +/- sd over mean (black dotted line), IQR (Interquartile 
#'       Range) (dotted green line), and boxplot's binf and bsup values (dotted blue)
#'       values are represented as horizontal lines
#' 
#'    4: bar graph of byte counts
#' 
#'    5: boxplot() graph of byte's data
#' 
#'    Note that ascii parameter's value is independent of the value of 'plot'  
#' 
#' @param col vector of color values, colours can be indicated as a vector of 
#'    colours from 1 to 3 elements, which will be used differently depending on the 
#'    plot selected. By default, the first colour of the vector will replace the 
#'    default green, the second the default red, and the third the default blue. 
#'    Not all colours are used on every plot.  
#' @param output integer (0, 1, 2), as function outputs data (file, mean, sd, 
#'    CV, file size) on R console after every call, this output can be turned 
#'    off using 'output=0'. A value of 2 will output the char array used for ascii 
#'    graph output.  
#' @param input factor or vector, the function can accept its own output as input.
#'    This can be useful for generating a new graph without the hassle of R reading
#'    and analysing the file again. The input can also be a bare 256 element vector:
#'    in this case each element represents the appeareances in the file of that 
#'    [n-1] byte value.
#'
#' @return factor of values :
#' 
#'    $bytes: vector of 256 elements, counts of each byte value in the file  
#' 
#'    $deviation: vector of 256 elements, (count-mean)/sigma for each byte value in the file  
#' 
#'    $file: char array, input file analysed. If input were a variable, it is "R input"  
#' 
#'    $mean: mean value  
#' 
#'    $sd: sigma (true sd) value: sigma=sd()*sqrt((n-1)/n)  
#' 
#'    $cv: coefficient of variation (mean/sigma*100)  
#' 
#'    $circle: complex matrix representing an ascii circle: each element is the 
#'        deviation from sigma of the represented byte. Elements which do not 
#'        represent bytes get the value '0+1i'. See \link[=../doc/bytescircle.pdf]{bytescircle's User Manual}.
#' 
#' @examples
#'  bytescircle( system.file("extdata", "gplv3.txt", package="bytescircle"), 
#'    ascii=TRUE, plot=1, output=2)
#'
#'  # which bytes in this file have a sd greater than 2*sigma?
#'  BYTES=bytescircle( system.file("extdata", "gplv3.txt.gz", package="bytescircle"), plot=3, 
#'    col=c("gold","blueviolet")); 
#'  which(BYTES$deviation>2.0)-1 # -1, 'cause BYTES[1] corresponds to byte 0 
#'
#'  # use a vector as input:
#'  BYTES=c(256:1); bytescircle(input=BYTES,output=0)
#' 
#' @author Roberto S. Galende <roberto.s.galende at gmail.com>
#'
#' @seealso
#'   \link[=../doc/bytescircle.pdf]{bytescircle's User Manual}.
#'
#'   Origin of bytes-circle linux command: \url{https://circulosmeos.wordpress.com/2015/10/10/statistics-circle-for-analysing-byte-entropy-in-files/}
#'
#'   Source code repository: \url{https://github.com/circulosmeos/bytescircle}
#'
#' @importFrom graphics abline axis boxplot legend par plot points text title
#' @importFrom stats IQR quantile sd
#' @importFrom utils capture.output 
#'
#' @export
bytescircle = function ( FILE = "", ascii = FALSE, plot = 1, col = c(), output = 1, input = NULL ) {

  token = bytescircle.token() 

  # three colors can be assigned; 
  # otherwise they'll get default predefined values depending on graph type

  if (plot < 4)
    color=c(token$color_green1, token$color_red1, token$color_blue1)
  else
    color=c(token$color_green2, token$color_red2, token$color_blue1)

  if (length(col)>3)
    col=col[1:3]
  if (length(col)>0)
    color[1:length(col)]=col[1:length(col)]

  # accept its own output as input, to generate new graphs
  if ( ! is.null(input) ) {

    FILE = "R input"
    if ( is.null(names(input)) ) {
      # input is a simple array
      if (length(input)!=token$MAX_VALUE) {
        stop( 
          capture.output( 
            cat("array passed has not proper length (", token$MAX_VALUE, " elements required).") )
          )
      } else {
        BYTE = input
      }
    } else {
      # input is the complete bytescircle() data frame output
      if ( length(which(names(input) == "bytes") ) > 0 ) {
          BYTE = input$bytes
          if ( length(which(names(input) == "file") ) > 0 ) {
            FILE = input$file
          }
        } else {
          stop( "data frame passed has no $byte row name." )
        }
    }
    
    SIZE = sum(BYTE)

  } else {

    # no data passed: a FILE must be read

    if (nchar(FILE)==0) {
      FILE = file.choose(new = FALSE)
    }

    # check file existence/readability
    if (file.access(FILE, mode = 0) != 0) {
      stop( 
        capture.output( 
          cat("file '", FILE, "' is not readable. Process aborted.") )
        )
    }

    SIZE = file.info(FILE)$size

    # initialize byte's bucket array counter
    BYTE=c(rep(0,token$MAX_VALUE))

    # open and load counts of bytes: R version (too slow for big files)
    ###to.read = file( FILE, "rb")
    ###LOOP = TRUE
    ###BLOCK_SIZE=65536
    ###BLOCKS=trunc(SIZE/BLOCK_SIZE)
    ###if (SIZE%%BLOCK_SIZE != 0) BLOCKS=BLOCKS+1 
    ###n=1
    ###while ( n<=BLOCKS ) {
    ###  bytes = readBin(to.read, n=BLOCK_SIZE, size=1, what="raw")
    ###  # trying to reduce BLOCK_SIZE times the eval of length(bytes) in for loop, using BLOCK_SIZE :
    ###  if (n == BLOCKS) # equivalent to: if (length(bytes)<BLOCK_SIZE)
    ###    BLOCK_SIZE=SIZE-((BLOCKS-1)*BLOCK_SIZE)
    ###  for (i in 1:BLOCK_SIZE) {
    ###      byte = as.integer(bytes[i])+1
    ###      BYTE[byte]=BYTE[byte]+1
    ###  }
    ###  n=n+1
    ###}
    ###close(to.read)
    
    # open and load counts of bytes: .C version
    BYTE = bytescircle_read_file( FILE, BYTE )

  }

  # counts and more counts on bytes
  #MEAN=SIZE/(token$MAX_VALUE)
  MEAN=mean(BYTE)
  # We want the Uncorrected sample standard deviation, not Corrected sample standard deviation
  # see https://en.wikipedia.org/wiki/Standard_deviation#Uncorrected_sample_standard_deviation
  #SIGMA=sqrt(sum((BYTE-MEAN)^2)/(token$MAX_VALUE)) # Uncorrected sample standard deviation
  # R calculates Corrected sample standard deviation, so correct it:
  SIGMA=sd(BYTE)*sqrt((token$MAX_VALUE-1)/token$MAX_VALUE)
  
  BYTES = list(bytes=BYTE,
               deviation=(BYTE-MEAN)/SIGMA*4 )
  
  t = which(abs(BYTES$deviation)>=(token$MAX_SIGMA_CHAR) & BYTES$bytes!=0)
  BYTES$deviation[ t[which(BYTES$deviation[t]>0)] ] = +token$MAX_SIGMA_CHAR
  BYTES$deviation[ t[which(BYTES$deviation[t]<=0)]] = -token$MAX_SIGMA_CHAR
  
  BYTES$deviation[which(BYTE==0)] = token$MISSING_CHAR_INDEX
  
  # show graph using ascii art
  circle=create.statistics.circle( BYTES, token )
  if (ascii==TRUE) {
    cat("\n") # line jump before ascii graph
    rows.to.print=print.circle( circle, token )
  }

  # plot data as a circle graph of sd increment little circles
  if (plot==1 | plot==2) {
    coordinates=create.statistics.circle.to.plot( token )
    plot.circle( BYTES, FILE, coordinates, color, token )   
  }

  # plot data as a circle graph of sd increment little circles
  # and add text info of bytes
  if (plot==2) {
    text(Re(coordinates),Im(coordinates),labels=c(0:255))
  }
  
  # graph just byte counts (with colors)
  if (plot==3) {
    par(mar = par()$mar+c(0, 0, 1, 0))
    plot( which(BYTES$bytes>=MEAN)-1,
          BYTES$bytes[which(BYTES$bytes>=MEAN)], 
        col=color[1], 
        pch=18,
        xlim=c(1,token$MAX_VALUE), 
        ylim=c(min(BYTES$bytes),max(BYTES$bytes)),
        xaxt = 'n',
        xlab="byte #",
        ylab="counts"
        )
    axis(3)
    points( which(BYTES$bytes<MEAN & BYTES$bytes!=0)-1,
            BYTES$bytes[which(BYTES$bytes<MEAN & BYTES$bytes!=0)], 
        col=color[2],
        pch=18
        )
    points( which(BYTES$bytes==0)-1,
            BYTES$bytes[which(BYTES$bytes==0)], 
        col=color[3],
        pch=1
        )    
    abline( h=MEAN )
    # +/-sd
    abline( h=(MEAN+SIGMA), lty=3)
    abline( h=(MEAN-SIGMA), lty=3)
    # IQR
    abline( h=quantile(BYTES$bytes,0.25), lty=4, col=color[1])
    abline( h=quantile(BYTES$bytes,0.75), lty=4, col=color[1])
    # boxplot binf and bsup
    abline( h=max(min(BYTES$bytes), quantile(BYTES$bytes,0.25)-1.5*IQR(BYTES$bytes)), lty=5, col=color[3])
    abline( h=min(max(BYTES$bytes), quantile(BYTES$bytes,0.75)+1.5*IQR(BYTES$bytes)), lty=5, col=color[3])
    par.xpd=par()$xpd
    # Restore default clipping rect
    par(xpd=TRUE, mar = par()$mar-c(0, 0, 1, 0))
    title(main=FILE)
    # legend
    legend( "bottom", horiz=TRUE, bty='n', cex=0.8,
      inset=c(0,-0.1),
      legend=c("mean","sd","IQR","binf&bsup"),
      col=c("black", "black", color[1], color[3]),
      lty=c(1,3,4,5)
      )
    par( xpd=par.xpd )
  }

  # graph just byte counts (with colors)
  if (plot==4) {
    plot( c(0:255),
          BYTES$bytes,
        type="h",
        xlab="byte",
        ylab="counts",
        main=FILE
        )
  }

  # graph boxplot
  if (plot==5) {
    boxplot(BYTES$bytes,
      ylab="counts",
      main=FILE
      )
    # with mean value
    points( MEAN, col=color[1], pch=18 )
  }

  if (Sys.info()['sysname'] == "Windows") {
    FILE = gsub( "\\\\", "/" , FILE)
  }

  if (output != 0 | ascii == TRUE) {
    cat("file = ", FILE, "\n")
    cat("mean = ", round(MEAN, 3), "\n")
    cat("sigma= ", round(SIGMA, 3), "( CV= ", round(SIGMA/MEAN*100,4), "% )", "\n")

    readable_size = SIZE
    i=1
    while (readable_size>1024.0) {
      i=i+1
      readable_size=readable_size/1024.0
    }
    cat("size = ", round(readable_size,2), token$SIZE_UNITS[i], " (", SIZE, "bytes) \n")

    if (output == 2 & ascii == TRUE)
      cat("chars= ", token$sigma_char, " (0.5 sigma (", round(SIGMA/2, 3), ") each)\n")
  }  

  # return zusammengetragen data
  BYTES$file=FILE
  BYTES$mean=MEAN
  BYTES$deviation=(BYTE-MEAN)/SIGMA
  BYTES$sd=SIGMA
  BYTES$cv=SIGMA/MEAN*100
  attr(BYTES$cv,"description")="percentage value"
  BYTES$circle=circle
  
  invisible (BYTES)

}


#' @useDynLib bytescircle bytescircle_read_file_
bytescircle_read_file <- function ( FILE, BYTE ) {
  
  .C(bytescircle_read_file_, FILE, BYTE)[[2]]

}


# global variables
bytescircle.token = function () {

  t = list()

  t$CIRCLE_EMPTY_VALUE = 0+1i
  t$MAX_X = 35
  t$MAX_Y = 14
  t$MAX_VALUE = 256 # total byte values: [1]==0 ... [255]==254! => [256]==255

  t$INIT_X = round( t$MAX_X/2 )
  t$INIT_Y = round( t$MAX_Y/2 )+1

  t$sigma_char   = c(  '.', ',', '-', '~', '+', '*', 'o', 'O', '#', '@', 
                  '=' , 
                  ' ' )
  t$MAX_SIGMA_CHAR = length( t$sigma_char ) -2 
  t$MISSING_CHAR = t$sigma_char[ length( t$sigma_char ) -1  ]
  t$MISSING_CHAR_INDEX = length( t$sigma_char ) -1 
  t$EMPTY_CHAR = t$sigma_char[ length( t$sigma_char ) ]
  t$EMPTY_CHAR_INDEX = length( t$sigma_char )

  t$SIZE_UNITS = c( "bytes", "kiB", "MiB", "GiB", "TiB", "PiB" );

  t$color_green1 = "darkolivegreen2"
  t$color_red1 = "coral"
  t$color_blue1 = "blue"
  t$color_green2 = "green"
  t$color_red2 = "red"

  t

}


# returns a matrix of deviations appropriate for print.circle()
create.statistics.circle = function ( BYTES, token ) {
    
  coordinates = c(rep(0,token$MAX_VALUE))

  angle=0.0
  inc_angle=5.0
  r=1.0
  inc_r=0.0184
  proportion=0.5
  x=token$INIT_X
  y=token$INIT_Y
  k=0 # unused counter
  
  circle = matrix(rep(token$CIRCLE_EMPTY_VALUE,token$MAX_X*token$MAX_Y), nrow=token$MAX_Y)
  
  circle[round(y),round(x)] = BYTES$deviation[1]
  coordinates[1] = 
    complex( real = round(x), imaginary = round(y) )
  x=x+1
  circle[round(y),round(x)] = BYTES$deviation[2]
  coordinates[2] = 
    complex( real = round(x), imaginary = round(y) )
  
  for ( n in 3:token$MAX_VALUE) {
    repeat {
      k=k+1
      angle=angle+inc_angle
      r=r+inc_r
      x=x-(cos(angle)*r)
      y=y+(sin(angle)*r*proportion)
      xx=trunc(x)
      yy=trunc(y)
      if (xx>token$MAX_X | yy>token$MAX_Y | xx<1 | yy<1) {
        cat("Error creating circle of characters!") # but try to continue...
      }
      if ( circle[yy,xx] == token$CIRCLE_EMPTY_VALUE ) {
        circle[yy,xx] = BYTES$deviation[n]
        break
      }
    }
    coordinates[n]=
      complex(real = xx,imaginary = yy)
    
  }

  # coordinates is calculated... but actually never used

  circle
  
}


# create a vector of coordinates for the archimedean spiral plot
create.statistics.circle.to.plot = function ( token ) {
  
  coordinates = c(rep(0,token$MAX_VALUE))
  
  x=0
  y=0
  r=0.3
  theta=0
  b=6
  inc=0.001
  step=0.2
  
  for ( n in 1:token$MAX_VALUE) {
    
    previous.length = length.on.archimedes.spiral(r)
    
    repeat {
      r = r + inc
      if ( length.on.archimedes.spiral(r) >= 
           (previous.length+step) ) {
        break;
      }
    }
    
    theta = r*b
    
    COORDS=porlar2cartesian(r, theta)
    
    coordinates[n]=complex(real = COORDS[1] + x,
                           imaginary = COORDS[2] + y)
  }
  
  # slight manual corrections for a better visual appearance
  coordinates[1]=-0.05+0.3i
  coordinates[2]=coordinates[2]-0.1+0.1i
  
  coordinates
  
}


length.on.archimedes.spiral = function ( theta ) {
    
  u = atan(theta)
  
  value.theta=0.5 * ( (1/cos(u))*tan(u) + 
                  log(abs((1/cos(u)) + tan(u))) )

  value.theta
  
}


porlar2cartesian = function ( r, theta ) {
  
  x = r * cos(theta)
  y = r * sin(theta)
  
  c(x,y)
  
}


# prints on R output an ascii circle graph
# emulating the circle plot
print.circle = function ( circle, token ) {
  
  rows.to.print = rep('',token$MAX_Y)
  
  for (y in 1:token$MAX_Y) {
    for (x in 1:token$MAX_X) {
      
      if ( circle[y, x] != token$MISSING_CHAR_INDEX & 
           circle[y, x] != token$CIRCLE_EMPTY_VALUE ) {
        deviation.index = trunc( Re(circle[y, x])/2 ) + trunc(token$MAX_SIGMA_CHAR/2) + 1
        if (deviation.index > token$MAX_SIGMA_CHAR)
          deviation.index = token$MAX_SIGMA_CHAR
        rows.to.print[y] =
          capture.output( cat(rows.to.print[y], 
              token$sigma_char[ deviation.index ], 
                sep='') )
      } else {
        if ( circle[y, x] == token$CIRCLE_EMPTY_VALUE ) {
          rows.to.print[y] =
            capture.output( cat(rows.to.print[y], 
              token$EMPTY_CHAR, 
                sep='') )
        } else {
          rows.to.print[y] =
            capture.output( cat(rows.to.print[y], 
                token$MISSING_CHAR, 
                  sep='') )
        }
      }
      
    }
  }
  
  # print rows each one on a new line
  cat(sprintf("%s\n",rows.to.print))
  
  rows.to.print
  
}


# plots the archimedean spiral with different circle sizes and colours
plot.circle = function ( BYTES, FILE, coordinates, color, token ) {
  
  plot(0,0,cex=0,
        axes=FALSE, frame.plot=FALSE, 
        xlim = c(min(Re(coordinates)),max(Re(coordinates))),
        ylim = c(min(Im(coordinates)),max(Im(coordinates))),
        xlab = "",
        ylab = "",
        main = FILE
        )
  
  for (n in 1:token$MAX_VALUE) {

    COLOR=color[1]

    if ( BYTES$deviation[n] < 0 ) {
      COLOR=color[2]
      #big.value=abs(BYTES$deviation[n]/1.3)
      big.value=abs(BYTES$deviation[n]/1.8)
    } else {
      big.value=abs(BYTES$deviation[n]/1.8)
    }

    if (big.value<0.5)
      big.value=0.5

    if ( BYTES$deviation[n] != token$MISSING_CHAR_INDEX ) {
        points(Re(coordinates[n]), Im(coordinates[n]),
             cex=big.value, pch=19,
             col=COLOR)
    } else {
        points(Re(coordinates[n]), Im(coordinates[n]),
             cex=0.5,
             col=color[3])
    }
  
  }

}
