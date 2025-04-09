!>
!> Enhanced module with various model variables of different dimensions
!>
module ocean_var

   ! 2D fields (existing)
   real, public, allocatable, target :: zeta(:, :), u(:, :), v(:, :)
   
   ! 3D field (existing, but now used as full 3D)
   real, public, allocatable, target :: temp(:, :, :)
   
   ! 0D scalar (new)
   real, public, target :: wind_speed = 0.0
   
   ! 1D profile (new)
   real, public, allocatable, target :: temp_profile(:)
   
   ! Time step
   real, public :: dt = 1800.0

end module ocean_var