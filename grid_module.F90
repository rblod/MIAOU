!===============================================================================
! grid_module.F90 : Module for axis and grid definitions and operations
!===============================================================================
module grid_module
   use netcdf, only: nf90_unlimited
   implicit none
   private

   ! Public types
   public :: axis, grid

   ! Public procedures
   public :: create_axis, create_rho_grid_2d, create_rho_grid_3d
   public :: create_time_axis, add_time_to_grid, get_time_dim_index

   ! Definition of a basic axis
   type :: axis
      character(len=32) :: name        !! Name of the axis (e.g., "xi_rho", "eta_rho", "time")
      character(len=64) :: long_name   !! Long descriptive name
      character(len=32) :: units       !! Units of the axis
      integer :: size                  !! Size of the axis
      integer :: id = -1               !! NetCDF ID (assigned per file)
      logical :: is_unlimited = .false. !! Whether this is an unlimited dimension
   end type axis

   ! Definition of the grid using axes
   type :: grid
      character(len=32) :: name            !! Grid name (e.g., "rho", "u", "v")
      type(axis), allocatable :: axes(:)   !! Array of axes that define this grid
      integer :: ndims = 0                 !! Number of dimensions

      ! Methods
   contains
      procedure :: init_from_axes      !! Initialize grid from a list of axes
      procedure :: add_axis            !! Add an axis to an existing grid
      procedure :: get_axis_by_name    !! Get an axis by name
      procedure :: clone               !! Create a copy of this grid
   end type grid

contains

   !---------------------------------------------------------------------------
   ! Methods for the grid type
   !---------------------------------------------------------------------------

   ! Initialize a grid from a list of axes
   subroutine init_from_axes(this, name, axes)
      class(grid), intent(inout) :: this
      character(len=*), intent(in) :: name
      type(axis), intent(in) :: axes(:)
      integer :: i

      this%name = name
      this%ndims = size(axes)

      if (allocated(this%axes)) deallocate (this%axes)
      allocate (this%axes(this%ndims))

      do i = 1, this%ndims
         this%axes(i) = axes(i)
      end do
   end subroutine init_from_axes

   ! Add an axis to an existing grid
   subroutine add_axis(this, new_axis)
      class(grid), intent(inout) :: this
      type(axis), intent(in) :: new_axis
      type(axis), allocatable :: temp(:)
      integer :: i

      if (.not. allocated(this%axes)) then
         allocate (this%axes(1))
         this%axes(1) = new_axis
         this%ndims = 1
         return
      end if

      ! Create a temporary copy
      allocate (temp(this%ndims))
      do i = 1, this%ndims
         temp(i) = this%axes(i)
      end do

      ! Reallocate with new size
      deallocate (this%axes)
      allocate (this%axes(this%ndims + 1))

      ! Copy back original axes
      do i = 1, this%ndims
         this%axes(i) = temp(i)
      end do

      ! Add the new axis
      this%axes(this%ndims + 1) = new_axis
      this%ndims = this%ndims + 1

      deallocate (temp)
   end subroutine add_axis

   ! Get an axis by name
   function get_axis_by_name(this, axis_name) result(axis_idx)
      class(grid), intent(in) :: this
      character(len=*), intent(in) :: axis_name
      integer :: axis_idx, i

      axis_idx = -1  ! Default: not found

      if (.not. allocated(this%axes)) return

      do i = 1, this%ndims
         if (trim(this%axes(i)%name) == trim(axis_name)) then
            axis_idx = i
            return
         end if
      end do
   end function get_axis_by_name

   ! Clone a grid (create a copy)
   function clone(this) result(new_grid)
      class(grid), intent(in) :: this
      type(grid) :: new_grid

      new_grid%name = this%name
      new_grid%ndims = this%ndims

      if (allocated(this%axes)) then
         allocate (new_grid%axes(this%ndims))
         new_grid%axes = this%axes
      end if
   end function clone

   !---------------------------------------------------------------------------
   ! Helper functions to create common axes and grids
   !---------------------------------------------------------------------------

   ! Create an axis with given parameters
   function create_axis(name, long_name, units, size, is_unlimited) result(new_axis)
      character(len=*), intent(in) :: name, long_name, units
      integer, intent(in) :: size
      logical, intent(in), optional :: is_unlimited
      type(axis) :: new_axis

      new_axis%name = name
      new_axis%long_name = long_name
      new_axis%units = units
      new_axis%size = size
      new_axis%id = -1  ! Will be assigned per file

      if (present(is_unlimited)) then
         new_axis%is_unlimited = is_unlimited
      else
         new_axis%is_unlimited = .false.
      end if
   end function create_axis

   ! Create a 2D rho-grid (typical for ocean models)
   function create_rho_grid_2d(nx, ny) result(rho_grid)
      integer, intent(in) :: nx, ny
      type(grid) :: rho_grid
      type(axis) :: xi_axis, eta_axis

      xi_axis = create_axis("xi_rho", "X-dimension of rho-grid", "count", nx)
      eta_axis = create_axis("eta_rho", "Y-dimension of rho-grid", "count", ny)

      call rho_grid%init_from_axes("rho2d", [xi_axis, eta_axis])
   end function create_rho_grid_2d

   ! Create a 3D rho-grid
   function create_rho_grid_3d(nx, ny, nz) result(rho_grid)
      integer, intent(in) :: nx, ny, nz
      type(grid) :: rho_grid
      type(axis) :: xi_axis, eta_axis, s_axis

      xi_axis = create_axis("xi_rho", "X-dimension of rho-grid", "count", nx)
      eta_axis = create_axis("eta_rho", "Y-dimension of rho-grid", "count", ny)
      s_axis = create_axis("s_rho", "S-coordinate on rho-grid", "levels", nz)

      call rho_grid%init_from_axes("rho3d", [xi_axis, eta_axis, s_axis])
   end function create_rho_grid_3d

   ! Create a time axis
   function create_time_axis(unlimited) result(time_axis)
      logical, intent(in), optional :: unlimited
      type(axis) :: time_axis
      logical :: is_unlimited

      if (present(unlimited)) then
         is_unlimited = unlimited
      else
         is_unlimited = .true.  ! Default: time is unlimited
      end if

      time_axis = create_axis("time", "Time", "seconds", 0, is_unlimited)
   end function create_time_axis

   ! Add time to a grid
   function add_time_to_grid(base_grid) result(time_grid)
      type(grid), intent(in) :: base_grid
      type(grid) :: time_grid
      type(axis) :: time_axis

      ! Clone the base grid
      time_grid = base_grid%clone()

      ! Create and add time axis
      time_axis = create_time_axis()
      call time_grid%add_axis(time_axis)
   end function add_time_to_grid

   ! Helper function to get time dimension index in a grid
   function get_time_dim_index(grid_obj) result(time_idx)
      type(grid), intent(in) :: grid_obj
      integer :: time_idx, i

      time_idx = -1

      if (.not. allocated(grid_obj%axes)) return

      do i = 1, grid_obj%ndims
         if (trim(grid_obj%axes(i)%name) == "time") then
            time_idx = i
            return
         end if
      end do
   end function get_time_dim_index

end module grid_module
