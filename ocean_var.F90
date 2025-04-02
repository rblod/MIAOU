!>
!> Pseudo variables to be written
!>
module ocean_var

   real, public, allocatable, target :: zeta(:, :), temp(:, :, :), u(:, :), v(:, :)
   real, public :: dt = 1800.0

end module
