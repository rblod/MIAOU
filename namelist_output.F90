!===============================================================================
! namelist_output.F90 : Reading output configuration via structured namelist
!===============================================================================
module namelist_output
   implicit none

   type :: var_output_config
      character(len=32)  :: name
      logical            :: wrt = .false.
      logical            :: avg = .false.
      logical            :: rst = .false.
      character(len=128) :: file = ""
      real               :: freq = -1.0
   end type

   type(var_output_config), allocatable :: dyn_vars(:)
   namelist /output_dyn/ dyn_vars

contains

   subroutine read_output_namelist()
      integer :: ios, i, n_read
      type(var_output_config), allocatable :: tmp(:)

      integer, parameter :: maxvars = 100
      allocate (dyn_vars(maxvars))  !
      dyn_vars(:)%name = ""
      dyn_vars(:)%wrt = .false.
      dyn_vars(:)%avg = .false.
      dyn_vars(:)%rst = .false.
      dyn_vars(:)%file = ""
      dyn_vars(:)%freq = -1.0

      open (unit=10, file="output_config.nml", status="old", action="read", iostat=ios)
      if (ios /= 0) then
         print *, "Warning: Cannot open output_config.nml, using defaults."
         allocate (dyn_vars(0))
         return
      end if

      read (10, nml=output_dyn, iostat=ios)
      if (ios /= 0) then
         print *, "Warning: Error reading namelist /output_dyn/"
         allocate (dyn_vars(0))
         close (10)
         return
      end if
      close (10)

      ! Compter le nombre de variables effectivement lues (on suppose name obligatoire)
      n_read = 0
      do i = 1, size(dyn_vars)
         if (trim(dyn_vars(i)%name) /= "") then
            n_read = n_read + 1
         else
            exit
         end if
      end do

      ! Copier vers tableau dyn_vars redimensionné à la bonne taille
      if (n_read > 0) then
         call move_alloc(dyn_vars, tmp)
         allocate (dyn_vars(n_read))
         dyn_vars = tmp(1:n_read)
      else
         call move_alloc(dyn_vars, tmp)
         allocate (dyn_vars(0))
      end if
   end subroutine read_output_namelist

end module namelist_output
