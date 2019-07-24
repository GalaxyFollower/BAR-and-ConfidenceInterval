program confidencemain
  use confidenceroutines
  implicit none



  integer               :: npoints
  integer               :: alloc_err, status = 0
  real(dp), allocatable :: Grxn(:), Gact(:)
  real(dp)	        :: cmean, cvaru, cvarl
  real(dp)		:: Mean, Variance
  real(dp)		:: CIMean
  real(dp)              :: Ubmean, Lbmean
  real(dp)		:: Ubvar, Lbvar
  character(30)		:: Ifile = 'InputCONF'
  character(30)		:: Ofile = 'OutputCONF'

  call countline(Ifile, npoints)
  allocate (Grxn(npoints))
  allocate (Gact(npoints))
  if ( npoints == 5 ) then
      cmean = 2.78
      cvarl = 0.48
      cvaru = 11.14
  elseif ( npoints == 10 ) then
      cmean = 2.26
      cvarl = 2.70
      cvaru = 19.02
  ! Modification by Mehdi Zare for different number of data points
  elseif ( npoints == 2 ) then
    cmean = 12.70
    cvarl = 0.00
    cvaru = 5.02
   elseif ( npoints == 3 ) then
    cmean = 4.30
    cvarl = 0.05
    cvaru = 7.38
   elseif ( npoints == 4 ) then
    cmean = 3.18
    cvarl = 0.22
    cvaru = 9.35
   elseif ( npoints == 6 ) then
    cmean = 2.57
    cvarl = 0.83
    cvaru = 12.83
   elseif ( npoints == 7 ) then
    cmean = 2.45
    cvarl = 1.24
    cvaru = 14.45   
   elseif ( npoints == 8 ) then
    cmean = 2.36
    cvarl = 1.69
    cvaru = 16.01
   elseif ( npoints == 9 ) then
    cmean = 2.31
    cvarl = 2.18
    cvaru = 17.53
   elseif ( npoints == 20 ) then
    cmean = 2.09
    cvarl = 8.91
    cvaru = 32.85
   elseif ( npoints == 30 ) then
    cmean = 2.045
    cvarl = 16.00
    cvaru = 45.70
  else           
     write (*,*) "This script is written only for 2,3,4,5,6,7,8,9,10,20,30  datapoints,", &
     	         " To use it for other number of data points, check the", &
                 " reference listed in header"
  	 call exit (status)
  end if  
   
  call readvalues ( Ifile, npoints, Grxn, Gact )  
  
  call calculatestat ( npoints, Grxn, Mean, Variance)
  call calculatebounds ( npoints, cmean, cvarl, cvaru, Variance, CIMean, Ubvar, Lbvar, Ubmean, Lbmean )
  call fileforwrite ( Ofile )
  write (20, 100) 'Grxn', Mean, Variance, CIMean, Ubvar, Lbvar, Ubmean, Lbmean
  100 format (1X, A5, 2X, F15.8, 2X, F15.8, 2X, F15.8, 5X, F15.8, 3X, F15.8, 2X, F15.8, 2X, F15.8)
  
  call calculatestat ( npoints, Gact, Mean, Variance)
  call calculatebounds ( npoints, cmean, cvarl, cvaru, Variance, CIMean, Ubvar, Lbvar, Ubmean, Lbmean )
  write (20, 110) 'Gact', Mean, Variance, CIMean, Ubvar, Lbvar, Ubmean, Lbmean
  110 format (1X, A5, 2X, F15.8, 2X, F15.8, 2X, F15.8, 5X, F15.8, 3X, F15.8, 2X, F15.8, 2X, F15.8)
  
  deallocate (Grxn, stat = alloc_err)
  deallocate (Gact, stat = alloc_err)
  CLOSE(20)

end program   
  
