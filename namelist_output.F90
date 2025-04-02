!>
!>
!>
module namelist_output
   implicit none

   ! Configuration globale
   character(len=128) :: his_prefix = "history"
   character(len=128) :: avg_prefix = "average"
   character(len=128) :: rst_prefix = "restart"

   type :: var_output_config
      character(len=32)  :: name
      logical            :: wrt = .false.
      logical            :: avg = .false.
      logical            :: rst = .false.
      character(len=128) :: file_prefix = ""  
      real               :: freq_his = -1.0   ! Fréquence pour history
      real               :: freq_avg = -1.0   ! Fréquence pour average
      real               :: freq_rst = -1.0   ! Fréquence pour restart
   end type

   type(var_output_config), allocatable :: dyn_vars(:)

   ! La nouvelle structure de la namelist
   namelist /output_global/ his_prefix, avg_prefix, rst_prefix
   namelist /output_dyn/ dyn_vars

contains
!>
!>
!> @param
!>
!> @param

   subroutine read_output_namelist()
      integer :: ios, i, n_read
      type(var_output_config), allocatable :: tmp(:)

      integer, parameter :: maxvars = 100
      allocate (dyn_vars(maxvars))
      dyn_vars(:)%name = ""
      dyn_vars(:)%wrt = .false.
      dyn_vars(:)%avg = .false.
      dyn_vars(:)%rst = .false.
      dyn_vars(:)%file_prefix = ""
      dyn_vars(:)%freq_his = -1.0
      dyn_vars(:)%freq_avg = -1.0
      dyn_vars(:)%freq_rst = -1.0

      ! Ouverture du fichier
      open (unit=10, file="output_config.nml", status="old", action="read", iostat=ios)
      if (ios /= 0) then
         print *, "Warning: Cannot open output_config.nml, using defaults."
         allocate (dyn_vars(0))
         return
      end if

      !> Reading des préfixes globaux
      read (10, nml=output_global, iostat=ios)
      if (ios /= 0) then
         print *, "Warning: Error reading namelist /output_global/, using defaults."
         rewind (10)  ! Revenir au début du fichier pour la prochaine lecture
      end if

      !> Reading des variables dynamiques
      read (10, nml=output_dyn, iostat=ios)
      if (ios /= 0) then
         print *, "Warning: Error reading namelist /output_dyn/"
         allocate (dyn_vars(0))
         close (10)
         return
      end if
      close (10)

      ! Compter le nombre de variables effectivement lues
      n_read = 0
      do i = 1, size(dyn_vars)
         if (trim(dyn_vars(i)%name) /= "") then
            n_read = n_read + 1
         else
            exit
         end if
      end do

      ! Copier vers tableau redimensionné
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
