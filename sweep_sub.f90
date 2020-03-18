module sweeping_sub
  
  private
  
  public :: sweeping
  
contains
  subroutine sweeping ( org, inv, nn )
    ! org : the original matrix
    ! inv : the matrix to return
    ! nn  : the number of data dimension
    
    !  real
    integer, parameter                                      :: double = selected_real_kind ( 9 )  
    real ( kind = double ), allocatable, dimension ( :, : ), intent ( in  ) :: org
    real ( kind = double ), allocatable, dimension ( :, : ), intent ( out ) :: inv
    real ( kind = double ), allocatable, dimension ( :, : ) :: b
    integer :: i, j
    integer, intent ( in ) :: nn
    
    allocate ( inv ( 1:nn, 1:nn ) ) 
    allocate ( b   ( 1:nn, 1:nn ) )
    b = org
    
!!! make identity ====================================================================================================
    inv = 0.0
    do i = 1, nn
       inv ( i, i ) = 1.0
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
  end subroutine sweeping
end module sweeping_sub
