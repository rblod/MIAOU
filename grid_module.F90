!===============================================================================
!> @file grid_module.F90
!>
!> Enhanced module for axis and grid definitions and operations
!>
!> This module defines the core types and operations for handling structured grids,
!> including axes (dimensions) and multi-dimensional grids. It provides functionality
!> for creating standard grid configurations used in ocean models.
!>
!> @author Rachid Benshila
!> @date 2025-04-10
!===============================================================================
module grid_module
   implicit none
   private

   ! Public types
   public :: axis, grid

   ! Public procedures
   public :: create_axis, create_empty_grid, create_1d_grid, create_2d_grid, create_3d_grid
   public :: create_rho_grid_2d, create_rho_grid_3d
   public :: create_time_axis, add_time_to_grid, get_time_dim_index

   !> @type axis
   !> Defines a single dimension (axis) in a grid system
   !>
   !> An axis represents a single dimension in a grid, such as longitude, latitude, or depth.
   !> It contains metadata about the dimension including its size and properties.
   type :: axis
      character(len=32) :: name        !< Name of the axis (e.g., "xi_rho", "eta_rho", "time")
      character(len=64) :: long_name   !< Long descriptive name
      character(len=32) :: units       !< Units of the axis
      integer :: size                  !< Size of the axis
      integer :: id = -1               !< NetCDF ID (assigned per file)
      logical :: is_unlimited = .false. !< Whether this is an unlimited dimension
   end type axis

   !> @type grid
   !> Defines a multi-dimensional grid using a collection of axes
   !>
   !> A grid represents a coordinate system composed of multiple axes.
   !> It can be 1D, 2D, 3D, or higher-dimensional, and includes methods
   !> for initialization and manipulation.
   type :: grid
      character(len=32) :: name            !< Grid name (e.g., "rho", "u", "v")
      type(axis), allocatable :: axes(:)   !< Array of axes that define this grid
      integer :: ndims = 0                 !< Number of dimensions

   contains
      !> Initialize grid from a list of axes
      procedure :: init_from_axes

      !> Add an axis to an existing grid
      procedure :: add_axis

      !> Get an axis by name
      procedure :: get_axis_by_name

      !> Create a copy of this grid
      procedure :: clone
   end type grid

contains

   !---------------------------------------------------------------------------
   ! Methods for the grid type
   !---------------------------------------------------------------------------

   !> Initialize a grid from a list of axes
   !>
   !> @param[inout] this      The grid to initialize
   !> @param[in]    name      Name for the grid
   !> @param[in]    axes      Array of axes to use for initialization
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

   !> Add an axis to an existing grid
   !>
   !> @param[inout] this      The grid to modify
   !> @param[in]    new_axis  The axis to add
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

   !> Get an axis by name
   !>
   !> @param[in]  this       The grid to search
   !> @param[in]  axis_name  Name of the axis to find
   !> @return     Index of the axis if found, -1 otherwise
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

   !> Clone a grid (create a copy)
   !>
   !> @param[in]  this      The grid to clone
   !> @return     A new grid with the same properties and axes
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

   !> Create an axis with given parameters
   !>
   !> @param[in]  name          Name of the axis
   !> @param[in]  long_name     Long descriptive name
   !> @param[in]  units         Units of the axis
   !> @param[in]  size          Size of the axis (number of points)
   !> @param[in]  is_unlimited  Optional: whether this is an unlimited dimension
   !> @return     Newly created axis
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

   !> Create an empty (0D) grid for scalar variables
   !>
   !> @return     A grid with no dimensions, suitable for scalar variables
   function create_empty_grid() result(empty_grid)
      type(grid) :: empty_grid

      ! Initialize with no axes
      empty_grid%name = "scalar"
      empty_grid%ndims = 0

      ! No axes to allocate
   end function create_empty_grid

   !> Create a 1D grid with flexible naming
   !>
   !> @param[in]  nx      Size of the x dimension
   !> @param[in]  x_name  Optional: name for the x-axis
   !> @param[in]  x_long  Optional: long name for the x-axis
   !> @param[in]  x_unit  Optional: units for the x-axis
   !> @return     A 1D grid with the specified properties
   function create_1d_grid(nx, x_name, x_long, x_unit) result(grid_1d)
      integer, intent(in) :: nx
      character(len=*), intent(in), optional :: x_name, x_long, x_unit
      type(grid) :: grid_1d
      type(axis) :: x_axis
      character(len=32) :: x_name_local, x_unit_local
      character(len=64) :: x_long_local

      ! Define default values and override if parameter is present
      x_name_local = "x_dim"
      if (present(x_name)) x_name_local = x_name

      x_long_local = "X dimension"
      if (present(x_long)) x_long_local = x_long

      x_unit_local = "count"
      if (present(x_unit)) x_unit_local = x_unit

      ! Create the axis with local values
      x_axis = create_axis(x_name_local, x_long_local, x_unit_local, nx)

      call grid_1d%init_from_axes("grid_1d", [x_axis])
   end function create_1d_grid

   !> Create a 2D grid with flexible naming
   !>
   !> @param[in]  nx      Size of the x dimension
   !> @param[in]  ny      Size of the y dimension
   !> @param[in]  x_name  Optional: name for the x-axis
   !> @param[in]  y_name  Optional: name for the y-axis
   !> @param[in]  x_long  Optional: long name for the x-axis
   !> @param[in]  y_long  Optional: long name for the y-axis
   !> @param[in]  x_unit  Optional: units for the x-axis
   !> @param[in]  y_unit  Optional: units for the y-axis
   !> @return     A 2D grid with the specified properties
   function create_2d_grid(nx, ny, x_name, y_name, x_long, y_long, x_unit, y_unit) result(grid_2d)
      integer, intent(in) :: nx, ny
      character(len=*), intent(in), optional :: x_name, y_name, x_long, y_long, x_unit, y_unit
      type(grid) :: grid_2d
      type(axis) :: x_axis, y_axis
      character(len=32) :: x_name_local, y_name_local, x_unit_local, y_unit_local
      character(len=64) :: x_long_local, y_long_local

      ! Define default values and override if parameter is present
      x_name_local = "x_dim"
      if (present(x_name)) x_name_local = x_name

      y_name_local = "y_dim"
      if (present(y_name)) y_name_local = y_name

      x_long_local = "X dimension"
      if (present(x_long)) x_long_local = x_long

      y_long_local = "Y dimension"
      if (present(y_long)) y_long_local = y_long

      x_unit_local = "count"
      if (present(x_unit)) x_unit_local = x_unit

      y_unit_local = "count"
      if (present(y_unit)) y_unit_local = y_unit

      ! Create the axes with local values
      x_axis = create_axis(x_name_local, x_long_local, x_unit_local, nx)
      y_axis = create_axis(y_name_local, y_long_local, y_unit_local, ny)

      call grid_2d%init_from_axes("grid_2d", [x_axis, y_axis])
   end function create_2d_grid

   !> Create a 3D grid with flexible naming
   !>
   !> @param[in]  nx      Size of the x dimension
   !> @param[in]  ny      Size of the y dimension
   !> @param[in]  nz      Size of the z dimension
   !> @param[in]  x_name  Optional: name for the x-axis
   !> @param[in]  y_name  Optional: name for the y-axis
   !> @param[in]  z_name  Optional: name for the z-axis
   !> @param[in]  x_long  Optional: long name for the x-axis
   !> @param[in]  y_long  Optional: long name for the y-axis
   !> @param[in]  z_long  Optional: long name for the z-axis
   !> @param[in]  x_unit  Optional: units for the x-axis
   !> @param[in]  y_unit  Optional: units for the y-axis
   !> @param[in]  z_unit  Optional: units for the z-axis
   !> @return     A 3D grid with the specified properties
   function create_3d_grid(nx, ny, nz, x_name, y_name, z_name, &
                           x_long, y_long, z_long, &
                           x_unit, y_unit, z_unit) result(grid_3d)
      integer, intent(in) :: nx, ny, nz
      character(len=*), intent(in), optional :: &
         x_name, y_name, z_name, x_long, y_long, z_long, x_unit, y_unit, z_unit
      type(grid) :: grid_3d
      type(axis) :: x_axis, y_axis, z_axis
      character(len=32) :: x_name_local, y_name_local, z_name_local
      character(len=32) :: x_unit_local, y_unit_local, z_unit_local
      character(len=64) :: x_long_local, y_long_local, z_long_local

      ! Define default values and override if parameter is present
      x_name_local = "x_dim"
      if (present(x_name)) x_name_local = x_name

      y_name_local = "y_dim"
      if (present(y_name)) y_name_local = y_name

      z_name_local = "z_dim"
      if (present(z_name)) z_name_local = z_name

      x_long_local = "X dimension"
      if (present(x_long)) x_long_local = x_long

      y_long_local = "Y dimension"
      if (present(y_long)) y_long_local = y_long

      z_long_local = "Z dimension"
      if (present(z_long)) z_long_local = z_long

      x_unit_local = "count"
      if (present(x_unit)) x_unit_local = x_unit

      y_unit_local = "count"
      if (present(y_unit)) y_unit_local = y_unit

      z_unit_local = "levels"
      if (present(z_unit)) z_unit_local = z_unit

      ! Create axes with local values
      x_axis = create_axis(x_name_local, x_long_local, x_unit_local, nx)
      y_axis = create_axis(y_name_local, y_long_local, y_unit_local, ny)
      z_axis = create_axis(z_name_local, z_long_local, z_unit_local, nz)

      call grid_3d%init_from_axes("grid_3d", [x_axis, y_axis, z_axis])
   end function create_3d_grid

   !> Create a 2D rho-grid (typical for ocean models)
   !>
   !> @param[in]  nx  Size of the xi dimension
   !> @param[in]  ny  Size of the eta dimension
   !> @return     A 2D grid with standard rho-grid axes
   function create_rho_grid_2d(nx, ny) result(rho_grid)
      integer, intent(in) :: nx, ny
      type(grid) :: rho_grid
      type(axis) :: xi_axis, eta_axis

      xi_axis = create_axis("xi_rho", "X-dimension of rho-grid", "count", nx)
      eta_axis = create_axis("eta_rho", "Y-dimension of rho-grid", "count", ny)

      call rho_grid%init_from_axes("rho2d", [xi_axis, eta_axis])
   end function create_rho_grid_2d

   !> Create a 3D rho-grid
   !>
   !> @param[in]  nx  Size of the xi dimension
   !> @param[in]  ny  Size of the eta dimension
   !> @param[in]  nz  Size of the s dimension (vertical levels)
   !> @return     A 3D grid with standard rho-grid axes
   function create_rho_grid_3d(nx, ny, nz) result(rho_grid)
      integer, intent(in) :: nx, ny, nz
      type(grid) :: rho_grid
      type(axis) :: xi_axis, eta_axis, s_axis

      xi_axis = create_axis("xi_rho", "X-dimension of rho-grid", "count", nx)
      eta_axis = create_axis("eta_rho", "Y-dimension of rho-grid", "count", ny)
      s_axis = create_axis("s_rho", "S-coordinate on rho-grid", "levels", nz)

      call rho_grid%init_from_axes("rho3d", [xi_axis, eta_axis, s_axis])
   end function create_rho_grid_3d

   !> Create a time axis
   !>
   !> @param[in]  unlimited  Optional: whether the time axis should be unlimited
   !> @return     Time axis with proper attributes
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

   !> Add time to a grid
   !>
   !> @param[in]  base_grid  The grid to extend with a time dimension
   !> @return     A new grid with the same axes as base_grid plus a time axis
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

   !> Helper function to get time dimension index in a grid
   !>
   !> @param[in]  grid_obj  The grid to search
   !> @return     Index of the time dimension if found, -1 otherwise
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
