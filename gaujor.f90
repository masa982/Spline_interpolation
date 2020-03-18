program main
  implicit none

  !  real
  integer, parameter                                      :: double = selected_real_kind ( 9 )  
  real ( kind = double ), allocatable, dimension ( :, : ) :: org, inv
  real ( kind = double ), allocatable, dimension ( :, : ) :: b, chk
  integer :: i, j, k, l, nn
  nn = 4
  
  allocate ( org ( 1:nn, 1:nn ) )
  allocate ( inv ( 1:nn, 1:nn ) ) 
  allocate ( b   ( 1:nn, 1:nn ) )
  allocate ( chk ( 1:nn, 1:nn ) )

!!! load ====================================================================================================
  open ( unit = 21, file = "data.dat", action = "read", status = "old" )
  do i = 1, nn
     do j = 1, nn
        if ( j /= nn ) then
           read ( unit = 21, fmt = ' ( f4.0 ) ', advance = "no" ) org ( i, j )
        else
           read ( unit = 21, fmt = ' ( f4.0 ) ', advance = "yes" ) org ( i, j )
        end if
     end do
  end do
  close ( unit = 21 )

  b = org
  
!!! make identity ====================================================================================================
  inv = 0
  do i = 1, nn
     inv ( i, i ) = 1
  end do

!!! Sweeping ====================================================================================================  
!!! Check diagonal zero ==================================================
  do i = 1, nn
     do j = 1, nn - 1
        if ( b ( i, i ) == 0.0 ) then
           b   ( i, : ) = b   ( i, : ) + b   ( i + j, : )
           inv ( i, : ) = inv ( i, : ) + inv ( i + j, : )
        else
           exit
        end if
     end do
     
!!! Set the identity matrix to 1.0 ==================================================
     inv ( i, : ) = inv ( i, : ) / b ( i, i )
     b   ( i, : ) = b   ( i, : ) / b ( i, i )
     
!!! Sweep ==================================================
     do j = i + 1, nn
        inv ( j, : ) = inv ( j, : ) - inv ( i, : ) * b ( j, i )
        b   ( j, : ) = b   ( j, : ) - b   ( i, : ) * b ( j, i )
     end do
     
     if ( i /= 1 ) then
        do j = i - 1, 1, -1
           inv ( j, : ) = inv ( j, : ) - inv ( i, : ) * b ( j, i )
           b   ( j, : ) = b   ( j, : ) - b   ( i, : ) * b ( j, i )
        end do
     end if

  end do
  
!!! plot result ==================================================================================================== 
  print *, "result"
!!! bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  print *, "b"
  write ( unit = 6, fmt = ' ( f20.10 ) ', advance = "yes" ) 
   do k = 1, nn
     do l = 1, nn
        if ( l /= nn ) then
           write ( unit = 6, fmt = ' ( f20.10 ) ', advance = "no"  ) b ( k, l )
        else
           write ( unit = 6, fmt = ' ( f20.10 ) ', advance = "yes" ) b ( k, l )
        end if
     end do
  end do
!!! bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  
  write ( unit = 6, fmt = ' ( f20.10 ) ', advance = "yes" )

!!! invinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinv
  print *, "inv"
   do k = 1, nn
     do l = 1, nn
        if ( l /= nn ) then
           write ( unit = 6, fmt = ' ( f20.10 ) ', advance = "no"  ) inv ( k, l )
        else
           write ( unit = 6, fmt = ' ( f20.10 ) ', advance = "yes" ) inv ( k, l )
        end if
     end do
  end do
!!! invinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinvinv

  chk = 0.0
  
  do i = 1, nn
     do j = 1, nn
        do k = 1, nn
           chk ( i, j ) = chk ( i, j ) + org ( i, k ) * inv ( k, j )
        end do
     end do
  end do

!!! chkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchk
  print *, "chk"
   do k = 1, nn
     do l = 1, nn
        if ( l /= nn ) then
           write ( unit = 6, fmt = ' ( f20.10 ) ', advance = "no"  ) chk ( k, l )
        else
           write ( unit = 6, fmt = ' ( f20.10 ) ', advance = "yes" ) chk ( k, l )
        end if
     end do
  end do
!!! chkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchkchk
  
  
  
end program main
