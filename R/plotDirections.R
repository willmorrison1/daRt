azimuthOffset <- function(input, offset){
    #v crude function ..
    #apply an azimuth offset to a given azimuth angle, and account
    #for going through 360 degrees

    rawOffset <- input + offset
    #index negative values
    isNegative <- rawOffset < 0
    rawOffset[isNegative] <- rawOffset[isNegative] + 360
    #index values that are greater than 360
    isGreaterThan360 <- rawOffset > 360
    rawOffset[isGreaterThan360] <- rawOffset[isGreaterThan360] - 360

    return(rawOffset)
}


polarToCartesian <- function(zenith, azimuth){
    #convert polar coordinates to cartesian coordinates

    x <- zenith * (cos(azimuth * (pi / 180)))
    y <- zenith * (sin(azimuth * (pi / 180)))
    return(data.frame("x_cartesian" = x, "y_cartesian" = y))
}

polarImageInterpolate <- function(x, y, z, outer.radius = 1,
                                  breaks, col, nlevels = 20, legend = TRUE,
                                  axes = TRUE, circle.rads = pretty(c(0, outer.radius)),
                                  northUp = TRUE, titleStr = NULL, ...){

    par(mar = c(2, 1.75, 1, 1.75))
    minitics <- seq(-outer.radius, outer.radius, length.out = 1000)
    # interpolate the data
    Interp <- akima:::interp(x = x, y = y, z = z,
                             extrap = TRUE,
                             xo = minitics,
                             yo = minitics,
                             linear = FALSE,
                             duplicate = "strip")
    Mat <- Interp[[3]]

    # mark cells outside circle as NA
    markNA <- matrix(minitics, ncol = 1000, nrow = 1000)
    Mat[!sqrt(markNA ^ 2 + t(markNA) ^ 2) < outer.radius] <- NA

    # sort out colors and breaks:
    if (!missing(breaks) & !missing(col)) {
        if (length(breaks) - length(col) != 1) {
            stop("breaks must be 1 element longer than cols")
        }
    }
    if (missing(breaks) & !missing(col)) {
        breaks <- seq(min(Mat,na.rm = TRUE), max(Mat, na.rm = TRUE), length = length(col) + 1)
        nlevels <- length(breaks) - 1
    }
    if (missing(col) & !missing(breaks)) {
        col <- rev(heat.colors(length(breaks) - 1))
        nlevels <- length(breaks) - 1
    }
    if (missing(breaks) & missing(col)) {
        breaks <- seq(min(Mat,na.rm = TRUE), max(Mat, na.rm = TRUE), length = nlevels + 1)
        col <- rev(heat.colors(nlevels))
    }

    # begin plot
    image(x = minitics, y = minitics, t(Mat), useRaster = TRUE, asp = 1, axes = FALSE,
          xlab = "", ylab = "", col = col, breaks = breaks, ...)

    if (exists("titleStr")) {
        title(titleStr, line = 0.35, ...)
    }

    # add radial axes if desired
    if (axes) {
        # internals for axis markup
        RMat <- function(radians){
            matrix(c(cos(radians), sin(radians), -sin(radians), cos(radians)), ncol = 2)
        }

        circle <- function(x, y, rad = 1, nvert = 500){
            rads <- seq(0,2*pi,length.out = nvert)
            xcoords <- cos(rads) * rad + x
            ycoords <- sin(rads) * rad + y
            cbind(xcoords, ycoords)
        }

        # draw circles
        if (missing(circle.rads)) {
            circle.rads <- pretty(c(0,outer.radius))
        }

        for (i in circle.rads) {
            lines(circle(0, 0, i), col = "#66666650", lwd = 0.75)
        }
        # put on radial spoke axes:
        axis.rads <- c(0, pi / 6, pi / 3, pi / 2, 2 * pi / 3, 5 * pi / 6)
        r.labs <- c(90, 60, 30, 0, 330, 300)
        l.labs <- c(270, 240, 210, 180, 150, 120)

        for (i in 1:length(axis.rads)) {
            endpoints <- zapsmall(c(RMat(axis.rads[i]) %*% matrix(c(1, 0, -1, 0) * outer.radius ,ncol = 2)))
            segments(endpoints[1], endpoints[2], endpoints[3], endpoints[4], col = "#66666650", lwd = 0.75)
            endpointsText <- c(RMat(axis.rads[i]) %*% matrix(c(1.08, 0, -1.11, 0) * outer.radius, ncol = 2))
            if (r.labs[i] == 0) {
                lab1 <- bquote(theta~.(r.labs[i]) * degree)
                lab2 <- bquote(.(l.labs[i]) * degree)
            } else {
                lab1 <- bquote(.(r.labs[i]) * degree)
                lab2 <- bquote(.(l.labs[i]) * degree)
            }

            par("xpd" = TRUE)
            text(x = endpointsText[1], y = endpointsText[2], label = lab1, vfont = c("serif","bold"), ...)
            text(x = endpointsText[3], y = endpointsText[4], label = lab2, vfont = c("serif","bold"), ...)
            par("xpd" = FALSE)
        }

    }

    # add legend
    if (legend) {
        fields::image.plot(x = minitics, y = minitics, t(Mat), add = TRUE,
                           breaks = breaks, col = col, legend.only = TRUE,
                           horizontal = TRUE, legend.width = 0.8, legend.mar = 2.2, lwd = 0.25,
                           legend.cex = 0.55,
                           axis.args = list(mgp = c(0, -0.4, 0), cex.axis = 0.5, tck = -0.003))


    }
}


directionsObjToCartesian <- function(azimuth, zenith, value, azimuthOffsetVal, outerRadius) {

    OUT <- data.frame("zenith" = zenith, "azimuth" = azimuth, "value" = value)
    OUT$azimuth <- 360 - OUT$azimuth    #turn azimuth angles clockwise
    OUT$azimuth <- azimuthOffset(OUT$azimuth, 90 + azimuthOffsetVal)    #add the DART scene azimuth offset
    OUT$azimuth <- azimuthOffset(OUT$azimuth, 90) #make north up - trig "north" is grid "east"
    OUT <- cbind(OUT, polarToCartesian(OUT$zenith, OUT$azimuth)) #convert to cartesian
    DARTdirectionPoints <- OUT[c("azimuth", "zenith")] #preallocate direction "points"
    DARTdirectionPoints$azimuth <- 360 - DARTdirectionPoints$azimuth #reverse direction (aclockwise)
    DARTdirectionPoints$azimuth <- azimuthOffset(DARTdirectionPoints$azimuth, 90 + azimuthOffsetVal)
    #filter out points with zenith angles greater than the given threshold
    DARTdirectionPoints$zenith[DARTdirectionPoints$zenith > outerRadius] <- NA
    #to do - error if outerradius gives no vals
    DARTdirectionPoints <- cbind(DARTdirectionPoints, polarToCartesian(
        DARTdirectionPoints$zenith,
        DARTdirectionPoints$azimuth))
    colnames(DARTdirectionPoints) <- paste(colnames(DARTdirectionPoints), "dirPoints", sep = "_")
    OUTfull <- cbind(OUT, DARTdirectionPoints)

    return(OUTfull)
}

#' @title plotDirections
#' @description Plot directions data as polar plot.
#' @param azimuth Numeric. Azimuth angle with DART conventions
#' @param zenith Numeric. Zenith angle with DART conventions
#' @param value Numeric. Values associated with the given azimuth and zenith angles
#' @param azimuthOffsetVal Numeric. Scene offset (degrees) as shown in the DART GUI.
#' @param outerRadius Numeric. Maximum radius (degrees) of polar plot
#' @param zenithLabPch Numeric. Pch for zenith label.
#' @param zenithLabCol Character. Colour for zenith label.
#' @param zenithLabCex Numeric. Cex for zenith label.
#' @param brks Numeric. Breaks for colour palette e.g. seq(0, 1, by = 0.1). Optional.
#' @param cols Character. Colours for given breaks. Optional.
#' @param ... Additional options passed to points() when drawing directions points.
#'
#' @examples
#' #Inputs are DART oriented directions (as seen in the DART files and \link{Directions-class})
#' plotDirections(azimuth = rep(225, 10),
#'                zenith = seq(0, 90, length.out = 10),
#'                value = 1:10)
#' #Output plot uses 'upward' directions from ground, where e.g.:
#'   0deg (270deg) azimuth faces north (west)
#'   0deg (90deg) zenith faces upward (horizon)
#' @export
plotDirections <- function(azimuth, zenith, value,
                            azimuthOffsetVal = 0,
                            outerRadius = max(zenith) + max(zenith) * 0.01,
                            zenithLabPch = 20,
                            zenithLabCol = "darkgrey",
                            zenithLabCex = 1,
                            brks = seq(min(value),
                                       max(value), length.out = 10),
                            cols =  c("dark grey",
                                      colorRampPalette(c("purple",
                                                         "blue3",
                                                         "yellow",
                                                         "red"))(length(brks) - 3),
                                      "firebrick4"),
                            ...){

    #to do - check if all azimuth and zenith angles are unique (if not - assume data are grouped
    #- which won't work here as only one plot is made).
    datDF <- directionsObjToCartesian(azimuth, zenith, value, azimuthOffsetVal, outerRadius)

    polarImageInterpolate(x = datDF$x_cartesian,
                          y = datDF$y_cartesian,
                          z = datDF$value,
                          outer.radius = outerRadius, breaks = brks,
                          col = cols)
    points(datDF$x_cartesian_dirPoints, datDF$y_cartesian_dirPoints, ...)

    points(polarToCartesian(outerRadius, 360 - 45), col = zenithLabCol,
           pch = zenithLabPch, cex = zenithLabCex)

    text(polarToCartesian(outerRadius, 360 - 45), labels = bquote(phi*.(outerRadius)*degree),
         adj = c(0, 1), col = "darkgrey", ...)

    outPlot <- recordPlot()
    return(outPlot)

}

