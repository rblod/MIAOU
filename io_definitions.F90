!===============================================================================
!> @file io_definitions.F90
!>
!> Core definitions for the I/O system
!>
!> This module defines the fundamental types and interfaces for the I/O system.
!> It establishes the common structures used throughout the output system, 
!> independent of any specific backend implementation.
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module io_definitions
   use grid_module, only: grid
   implicit none
   private

   ! Public types and interfaces
   public :: io_variable, file_descriptor
   public :: io_var_registry

   !> @type io_variable
   !> Fundamental variable type for I/O representation
   !>
   !> This type represents a model variable that can be output to files.
   !> It contains metadata and configuration for output operations,
   !> independent of the specific storage format.
   type :: io_variable
      ! Champs existants
      character(len=32) :: name            
      character(len=64) :: long_name       
      character(len=32) :: units           
      type(grid) :: var_grid               
      integer :: ndims = 0                 
      
      logical :: to_his = .false.          
      logical :: to_avg = .false.          
      logical :: to_rst = .false.          
      
      real, pointer :: scalar => null()             
      real, pointer :: data_1d(:) => null()         
      real, pointer :: data_2d(:, :) => null()      
      real, pointer :: data_3d(:, :, :) => null()   
      
      real :: freq_his = -1.                        
      real :: freq_avg = -1.                        
      real :: freq_rst = -1.                        
      character(len=128) :: file_prefix = ""

      ! Nouveaux champs pour l'accumulation des moyennes
      real :: scalar_avg = 0.0                      ! Pour les scalaires (0D)
      real, allocatable :: data_avg_1d(:)           ! Pour les tableaux 1D
      real, allocatable :: data_avg_2d(:, :)        ! Pour les tableaux 2D
      real, allocatable :: data_avg_3d(:, :, :)     ! Pour les tableaux 3D
      integer :: avg_count = 0                      ! Compteur pour le nombre d'accumulations
      logical :: avg_initialized = .false.          ! Flag pour marquer si les buffers sont initialisés
   end type io_variable

   !> @type file_descriptor
   !> Descriptor for an output file
   !>
   !> This type provides a backend-independent description of an output file,
   !> containing information about its type, frequency, and other metadata.
   type :: file_descriptor
      character(len=256) :: filename  !< Complete filename
      character(len=16) :: type       !< "his" (history), "avg" (average), or "rst" (restart)
      real :: freq                    !< Write frequency (seconds)
      integer :: time_index = 1       !< Current time write index
      
      ! Backend-specific identifiers can be added by the implementation
      integer :: backend_id = -1      !< ID in the backend system (e.g., NetCDF ncid)
   end type file_descriptor

   !> @type io_var_registry
   !> Registry of variables for output operations
   !>
   !> This type manages the collection of variables registered for output,
   !> providing methods to add, find, and manipulate variables.
   type :: io_var_registry
      private
      type(io_variable), allocatable :: variables(:)  !< Array of registered variables
      integer :: count = 0                            !< Number of registered variables
      
   contains
      !> Add a variable to the registry
      procedure :: add => registry_add_variable
      
      !> Find a variable by name
      procedure :: find => registry_find_variable
      
      !> Get the number of registered variables
      procedure :: size => registry_size
      
      !> Get a variable by index
      procedure :: get => registry_get_variable
   end type io_var_registry

contains

   !> Add a variable to the registry
   !>
   !> @param[inout] this  The registry to modify
   !> @param[in]    var   The variable to add
   subroutine registry_add_variable(this, var)
      class(io_var_registry), intent(inout) :: this
      type(io_variable), intent(in) :: var
      type(io_variable), allocatable :: temp(:)
      integer :: i, new_size
      
      if (.not. allocated(this%variables)) then
         ! Initial allocation with extra space
         allocate(this%variables(10))
         this%count = 1
         this%variables(1) = var
         
         ! Mark other elements as unused
         do i = 2, 10
            this%variables(i)%name = ""
         end do
      else
         ! Look for an empty slot first
         do i = 1, size(this%variables)
            if (trim(this%variables(i)%name) == "") then
               this%variables(i) = var
               this%count = max(this%count, i)
               return
            end if
         end do
         
         ! No empty slots, need to expand
         new_size = size(this%variables) + max(5, nint(size(this%variables)*0.5))
         allocate(temp(new_size))
         
         ! Copy existing data
         temp(1:size(this%variables)) = this%variables
         
         ! Add new variable
         temp(size(this%variables) + 1) = var
         this%count = size(this%variables) + 1
         
         ! Mark new slots as empty
         do i = this%count + 1, new_size
            temp(i)%name = ""
         end do
         
         ! Replace old array with new one
         call move_alloc(temp, this%variables)
      end if
   end subroutine registry_add_variable
   
   !> Find a variable by name
   !>
   !> @param[in]  this     The registry to search
   !> @param[in]  varname  Name of the variable to find
   !> @return     Index of the variable, or -1 if not found
   function registry_find_variable(this, varname) result(idx)
      class(io_var_registry), intent(in) :: this
      character(len=*), intent(in) :: varname
      integer :: idx, i
      
      idx = -1
      if (.not. allocated(this%variables)) return
      
      do i = 1, this%count
         if (trim(this%variables(i)%name) == trim(varname)) then
            idx = i
            return
         end if
      end do
   end function registry_find_variable
   
   !> Get the number of registered variables
   !>
   !> @param[in]  this  The registry to query
   !> @return     Number of variables in the registry
   function registry_size(this) result(sz)
      class(io_var_registry), intent(in) :: this
      integer :: sz
      
      sz = this%count
   end function registry_size
   
   !> Get a variable by index
   !>
   !> @param[in]  this  The registry to query
   !> @param[in]  idx   Index of the variable to retrieve
   !> @return     The requested variable (or uninitialized if invalid index)
   function registry_get_variable(this, idx) result(var)
      class(io_var_registry), intent(in) :: this
      integer, intent(in) :: idx
      type(io_variable) :: var
      
      if (allocated(this%variables) .and. idx > 0 .and. idx <= this%count) then
         var = this%variables(idx)
      else
         var%name = ""  ! Mark as invalid
      end if
   end function registry_get_variable

end module io_definitions