program main
  use sweeping_sub
  implicit none

  integer, parameter                                      :: double = selected_real_kind ( 9 )  
  integer :: i, j, k, nn
  real ( kind = double ) :: hi, hi2, hi3, dx, aj, aj2, bj, dj
  real ( kind = double ), allocatable, dimension ( : ) :: xx, yy, bb, chk, x, y
  real ( kind = double ), allocatable, dimension ( :, : ) :: AA, AA_inv

  nn = 11
  
  allocate ( xx ( 1:nn ) )
  allocate ( yy ( 1:nn ) ) 
  allocate ( bb ( 1:nn ) ) 
  allocate ( chk ( 1:nn ) ) 
  allocate ( AA ( 1:nn, 1:nn ) ) 
  allocate ( x ( 1:100 ) ) 
  allocate ( y ( 1:100 ) ) 
  AA = 0
  bb = 0
  
  open ( unit = 21, file = "data.dat", action = "read", status = "old" )
  
  do i = 1, nn
     read ( unit = 21, fmt = ' ( 2f4.0 ) ' ) xx ( i ), yy ( i )
     if ( i == 1 ) then
        AA ( 1, 1 ) = 1
     else if ( 3 <= i .and. i <= nn ) then
        hi  = xx ( i - 1 ) - xx ( i - 2 )
        hi2 = xx ( i ) - xx ( i - 1 )
        hi3 = 2 * ( hi + hi2 )
        AA ( i - 1, i - 2 ) = hi
        AA ( i - 1, i - 1 ) = hi3
        AA ( i - 1, i     ) = hi2
        if ( i == nn ) then
           AA ( nn, nn ) = 1
        end if
        bb ( i - 1 ) = ( 3 / hi2 ) * ( yy ( i ) - yy ( i -1 ) ) - ( 3 / hi ) * ( yy (i - 1) - yy ( i -2 ) )
     end  if
  end do

  close ( unit = 21 )
     
  call sweeping ( AA, AA_inv, nn )

  do i = 1, nn
     do j = 1, nn
        chk ( i ) = chk ( i ) + AA_inv ( i, j ) * bb ( j )
     end do
  end do

  dx = 0.1
  do i = 1, 100
     x ( i ) =  dx
     do j = 2, nn
        if ( xx ( j - 1 ) <= x ( i ) .and. x ( i ) <= xx ( j ) ) then
           aj  = yy ( j -1 )
           aj2 = yy ( j )
           bj  = ( aj2 - aj ) / ( xx ( j ) - xx ( j -1 ) ) & 
                 & - ( xx ( j ) - xx ( j -1 ) ) * ( chk ( j ) + 2 * chk ( j - 1 ) ) / 3
           dj  = ( chk ( j ) - chk ( j - 1 ) ) / 3 * ( xx ( j ) - xx ( j - 1 ) )

           y ( i ) = aj + bj * ( x ( i ) - xx ( j - 1 ) ) + chk ( j - 1 ) * ( x ( i ) - xx ( j - 1 ) ) ** 2 & 
                      & + dj * ( x ( i ) - xx ( j - 1 ) ) ** 3

        end if
     end do     
     dx = dx + 0.1
  end do

  open ( unit = 22, file = "data_result.dat", action = "write", status = "replace" )
  do i = 1, 100
     write ( unit = 22, fmt = ' ( 2f20.10 ) ' ) x ( i ), y ( i )
  end do
  close ( unit = 22 )

!  print *, "y"
!  do i = 1, 100
!     write ( unit = 6, fmt = ' ( f20.10 ) ', advance = "yes" ) y ( i )
!  end do

!  print *, "chk"
!  do i = 1, nn
!     write ( unit = 6, fmt = ' ( f20.10 ) ', advance = "yes" ) chk ( i )
!  end do
!


  
!  print *, "AA_inv"
!   do i = 1, nn
!     do j = 1, nn
!        if ( j /= nn ) then
!           write ( unit = 6, fmt = ' ( f20.10 ) ', advance = "no"  ) AA_inv ( i, j )
!        else
!           write ( unit = 6, fmt = ' ( f20.10 ) ', advance = "yes" ) AA_inv ( i, j )
!        end if
!     end do
!  end do


  
!  do i = 1, nn
!     do k = 1, nn
!        chk ( i ) = chk ( i ) + AA_inv ( i, k ) * bb ( k )
!     end do
!  end do
!  
!  do i = 1, nn
!     write ( unit = 6, fmt = ' ( i4 ) ', advance = "yes" ) chk ( i )
!  end do
!  
!
!  print *, chk

end program main

