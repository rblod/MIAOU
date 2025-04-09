!>
!>  Module to read output info in a namelist
!>
module namelist_output
   implicit none

   ! Global configuration
   character(len=128) :: his_prefix = "history"
   !! global history file name
   character(len=128) :: avg_prefix = "average"
   !! global average file name
   character(len=128) :: rst_prefix = "restart"
   !! global restart file name

   type :: var_output_config
   !! the type used for namelist to get the informations
      character(len=32)  :: name
      logical            :: wrt = .false.     !! Write his
      logical            :: avg = .false.     !! Write avg
      logical            :: rst = .false.     !! Write rst
      character(len=128) :: file_prefix = ""  !! History file
      real               :: freq_his = -1.0   !! History frequency
      real               :: freq_avg = -1.0   !! Average frequency
      real               :: freq_rst = -1.0   !! Restart frequency
   end type

   type(var_output_config), allocatable :: dyn_vars(:)
   !! variables read the namelist

   !
   namelist /output_global/ his_prefix, avg_prefix, rst_prefix
   namelist /output_dyn/ dyn_vars

contains
!>
!>
!> Read global and indivual infos
!>
!>

   subroutine read_output_namelist()
      integer :: ios, i, n_read
      !! dummy index and logical
      type(var_output_config), allocatable :: tmp(:)
      !! temporary

      integer, parameter :: maxvars = 100
      !! Maximum variables to be read in namelist

      ! Votre lecture du namelist doit ressembler à ceci :
      allocate(dyn_vars(maxvars))
      dyn_vars(:)%name = ""
      dyn_vars(:)%wrt = .false.
      dyn_vars(:)%avg = .false.
      dyn_vars(:)%rst = .false.
      dyn_vars(:)%file_prefix = ""
      dyn_vars(:)%freq_his = -1.0
      dyn_vars(:)%freq_avg = -1.0
      dyn_vars(:)%freq_rst = -1.0

      ! Lecture du namelist
      open(unit=10, file="output_config.nml", status="old", action="read", iostat=ios)
      if (ios /= 0) then
         print *, "Warning: Cannot open output_config.nml, using defaults. Error code:", ios
         deallocate(dyn_vars)
         allocate(dyn_vars(0))
         return
      end if

      ! Lire les préfixes globaux
      read(10, nml=output_global, iostat=ios)
      if (ios /= 0) then
         print *, "Warning: Error reading namelist /output_global/, using defaults. Error code:", ios
         rewind(10)
      end if

      ! Lire les variables et leurs attributs
read (10, nml=output_dyn, iostat=ios)
if (ios /= 0) then
   print *, "Warning: Error reading namelist /output_dyn/"
   if (allocated(dyn_vars)) deallocate(dyn_vars)  ! Assurez-vous d'abord qu'il est alloué
   allocate (dyn_vars(0))  ! Réallouez directement, pas via move_alloc
   close (10)
   return
end if

      ! Count of variables read
      n_read = 0
      do i = 1, size(dyn_vars)
         if (trim(dyn_vars(i)%name) /= "") then
            n_read = n_read + 1
         else
            exit
         end if
      end do

      ! copy to temporary array and correct dimensions for dyn_var
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
