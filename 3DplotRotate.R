#Referenced: https://www.statology.org/3d-plot-in-r/


#create 3D plot and Rotate!
# (x,y,z) - 3D plot values
#rotation - degrees which the plot will rotate
#step - degrees between each image 
#tstep - time step between each image
ThreeDplotRotate <- function(x, y,z, step = 30, rotation = 360, tstep = 0.25) {
     for (THETA in seq(0,rotation,step)) {
          persp(x, y, z, xlab='X Variable', ylab='Y Variable', zlab='Z Variable',
                main='3D Plot', col='green', shade=.4, theta = THETA, phi = 15)
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

#define function to create z-values
#Converted Matlab code from: http://www.taggedwiki.zubiaga.org/new_content/86bb8c19da64c02198b8d8d01635be7c#Meaning_of_parameters_for_the_general_equation
z_valuesGaussian <- function(x, y,sigma_x = 5,sigma_y = 5) {
     A = 1
     x0 = 0
     y0 = 0
     
     sigma_x = 6
     sigma_y = 3
     
     for( theta in seq(0, pi, pi/100)){
     a = cos(theta)^2/2/sigma_x^2 + sin(theta)^2/2/sigma_y^2
     b = -sin(2*theta)/4/sigma_x^2 + sin(2*theta)/4/sigma_y^2 
     c = sin(theta)^2/2/sigma_x^2 + cos(theta)^2/2/sigma_y^2
     }
     
     #return
     A*exp( - (a*(x-x0)^2 + 2*b*(x-x0)*(y-y0) + c*(y-y0)^2)) 
}

#create z-values, choose z_values for x2 + y2, or z_valuesGaussian for gaussian
z = outer(x, y, z_valuesGaussian)



## METHOD CALL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ThreeDplotRotate(x,y,z, step = 5, tstep = .05)
