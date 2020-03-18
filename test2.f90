program main
  use sweeping_sub
  implicit none

  integer, parameter                                      :: double = selected_real_kind ( 9 )  
  integer :: i, j, k, nn
  real :: hi, hi2, hi3
  integer, allocatable, dimension ( : ) :: xx, yy, bb, chk
  real ( kind = double ), allocatable, dimension ( :, : ) :: AA, AA_inv

  nn = 11
  
  allocate ( xx ( 1:nn ) )
  allocate ( yy ( 1:nn ) ) 
  allocate ( bb ( 1:nn ) ) 
  allocate ( chk ( 1:nn ) ) 
  allocate ( AA ( 1:nn, 1:nn ) ) 
  AA = 0
  bb = 0
  
  open ( unit = 21, file = "data.dat", action = "read", status = "old" )
  
  do i = 1, nn
     read ( unit = 21, fmt = ' ( 2i4 ) ' ) xx ( i ), yy ( i )
     if ( i == 1 ) then
        AA ( 1, 1 ) = 1
     else if ( 3 <= i .and. i <= nn ) then
        hi  = xx ( i - 1 ) - xx ( i - 2 )
        hi2 = xx ( i ) - xx ( i - 1 )
        hi3 = 2 * ( hi + hi2 )
        AA ( i - 2, i - 1 ) = hi
        AA ( i - 1, i - 1 ) = hi3
        AA ( i    , i - 1 ) = hi2
        if ( i == nn ) then
           AA ( nn, nn ) = 1
        end if
        bb ( i - 1 ) = ( 3 / hi2 ) * ( yy ( i ) - yy ( i -1 ) ) - ( 3 / hi ) * ( yy (i - 1) - yy ( i -2 ) )
     end  if
  end do

  close ( unit = 21 )
     
  call sweeping ( AA, AA_inv, nn )
  do i = 1, nn
     do k = 1, nn
        chk ( i ) = chk ( i ) + AA ( i, k  ) * bb ( k )
     end do
  end do

  print *, chk

end program main

