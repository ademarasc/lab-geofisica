MODULE interpolation

IMPLICIT NONE
  
CONTAINS
SUBROUTINE INTERPOL(y1,y2,x1,x2,a,b) 
 
  IMPLICIT NONE
  REAL, INTENT(IN) :: y1, y2, x1, x2
  REAL, INTENT(OUT) :: a,b
  REAL :: meanx, meany
  
  meanx = (x1+x2)/2
  meany = (y2+y1)/2
     
  a = ((x1-meanx)*(y1-meany)+(x2-meanx)*(y2-meany))/((x1-meanx)**2+(x2-meanx)**2)
  b = meany - a*meanx
 
  
RETURN
END SUBROUTINE

END MODULE interpolation
