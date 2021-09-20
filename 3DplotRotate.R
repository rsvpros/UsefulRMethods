#Referenced: https://www.statology.org/3d-plot-in-r/


#create 3D plot and Rotate!
# (x,y,z) - 3D plot values
#rotation - degrees which the plot will rotate
#step - degrees between each image 
#tstep - time step between each image
ThreeDplotRotate <- function(x, y,z, step = 30, rotation = 360, tstep = 0.25) {
     for (THETA in seq(0,rotation,step)) {
          persp(x, y, z, xlab='X Variable', ylab='Y Variable', zlab='Z Variable',
                main='3D Plot', col='pink', shade=.4, theta = THETA, phi = 15)
          Sys.sleep(tstep)
     }
}



## DEFINE DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x <- -10:10
y <- -10:10

#define function to create z-values
z_values <- function(x, y) {
     sqrt(x ^ 2 + y ^ 2)
}
#create z-values
z = outer(x, y, z_values)



## METHOD CALL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ThreeDplotRotate(x,y,z, step = 5, tstep = .05)