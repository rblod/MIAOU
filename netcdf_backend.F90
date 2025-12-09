!===============================================================================
!> @file netcdf_backend.F90
!>
!> Module to handle NetCDF-specific operations
!>
!> This module provides a backend interface for NetCDF operations, abstracting
!> the complexity of direct NetCDF API calls. It handles error checking,
!> variable definition, and writing variables of different dimensions (0D to 3D).
!>
!> @author Rachid Benshila
!> @date 2025-04-10
!===============================================================================
module netcdf_backend
   use netcdf
   use netcdf_utils
   use grid_module
   use io_constants, only: io_compression_enabled, io_compression_level
#ifdef NC4PAR
   use mpi_param, only: iminmpi, jminmpi
#endif
   implicit none
   private

   ! Public interface
   public :: nc_define_variable, nc_write_variable

   !> Generic interface for writing variables of different dimensions
   !>
   !> This interface selects the appropriate specific procedure based on
   !> the dimensionality of the data being written.
   interface nc_write_variable
      module procedure nc_write_scalar      ! For 0D (scalar) values
      module procedure nc_write_1d          ! For 1D arrays
      module procedure nc_write_2d          ! For 2D arrays
      module procedure nc_write_3d          ! For 3D arrays
   end interface nc_write_variable

contains

   !---------------------------------------------------------------------------
   ! Variable definition
   !---------------------------------------------------------------------------

   !> Define a variable in NetCDF file based on its dimensions
   !>
   !> This subroutine handles defining both scalar and n-dimensional variables,
   !> creating any dimensions that don't already exist in the file.
   !> In NC4PAR mode, uses global dimensions (LLm, MMm) for xi/eta axes.
   !>
   !> @param[in]     ncid          NetCDF file ID
   !> @param[in]     ndims         Number of dimensions (0=scalar, 1=1D, etc.)
   !> @param[in]     var_name      Variable name in the NetCDF file
   !> @param[in]     var_long_name Long descriptive name (for attribute)
   !> @param[in]     var_units     Units of the variable (for attribute)
   !> @param[inout]  grid_axes     Array of spatial axes for the variable
   !> @param[in]     time_dimid    ID of the time dimension
   !> @param[out]    varid         ID of the newly defined variable
   subroutine nc_define_variable(ncid, ndims, var_name, var_long_name, var_units, &
                                 grid_axes, time_dimid, varid)
#ifdef NC4PAR
      use mpi_param, only: LLm, MMm
#endif
      integer, intent(in) :: ncid, ndims, time_dimid
      character(len=*), intent(in) :: var_name, var_long_name, var_units
      type(axis), intent(inout) :: grid_axes(:)
      integer, intent(out) :: varid

      integer :: i, ncerr, dim_id, dim_size
      integer, allocatable :: dim_ids(:)

      ! Define all spatial dimensions from the grid
      do i = 1, size(grid_axes)
         ! Check if dimension already exists
         ncerr = nf90_inq_dimid(ncid, trim(grid_axes(i)%name), dim_id)

         if (ncerr /= nf90_noerr) then
            ! Determine dimension size
#ifdef NC4PAR
            ! Use global dimensions for xi/eta in parallel mode
            if (index(grid_axes(i)%name, 'xi_') == 1) then
               dim_size = LLm
            else if (index(grid_axes(i)%name, 'eta_') == 1) then
               dim_size = MMm
            else
               dim_size = grid_axes(i)%size
            end if
#else
            dim_size = grid_axes(i)%size
#endif

            ! Create the dimension
            if (grid_axes(i)%is_unlimited) then
               call nc_check(nf90_def_dim(ncid, trim(grid_axes(i)%name), &
                                          nf90_unlimited, dim_id), &
                             "Create axis : "//trim(grid_axes(i)%name))
            else
               call nc_check(nf90_def_dim(ncid, trim(grid_axes(i)%name), &
                                          dim_size, dim_id), &
                             "Create axis : "//trim(grid_axes(i)%name))
            end if
         end if

         ! Store dimension ID
         grid_axes(i)%id = dim_id
      end do

      ! Create array of dimension IDs
      ! For scalar, we only need time dimension
      ! For others, we need spatial dims + time
      if (ndims == 0) then
         ! Scalar - only time dimension
         allocate (dim_ids(1))
         dim_ids(1) = time_dimid
      else
         ! Array - spatial dims + time
         allocate (dim_ids(size(grid_axes) + 1))

         ! Copy existing spatial dimension IDs
         do i = 1, size(grid_axes)
            dim_ids(i) = grid_axes(i)%id
         end do

         ! Add time dimension
         dim_ids(size(grid_axes) + 1) = time_dimid
      end if

      ! Define the variable
      call nc_check(nf90_def_var(ncid, var_name, nf90_real, dim_ids, varid), &
                    "Create variable : "//trim(var_name))

      ! Enable compression for non-scalar variables (NetCDF-4 feature)
      ! Settings come from namelist via io_constants
      if (ndims > 0 .and. io_compression_enabled) then
         ncerr = nf90_def_var_deflate(ncid, varid, shuffle=1, deflate=1, &
                                      deflate_level=io_compression_level)
         ! Ignore errors - compression may not be available
      end if

      ! Add attributes
      call nc_check(nf90_put_att(ncid, varid, "long_name", trim(var_long_name)), &
                    "Create attribute : "//trim(var_long_name))

      call nc_check(nf90_put_att(ncid, varid, "units", trim(var_units)), &
                    "Create attribute : "//trim(var_units))

      deallocate (dim_ids)
   end subroutine nc_define_variable

   !> Write a scalar variable to a NetCDF file
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] varid       Variable ID in the file
   !> @param[in] data        Scalar data to write
   !> @param[in] time_index  Index of the time step to write
   subroutine nc_write_scalar(ncid, varid, data, time_index)
      integer, intent(in) :: ncid, varid, time_index
      real, intent(in) :: data  ! Can be a scalar or dimensioned array (0)
      integer :: start(1), count(1)
      real :: value

      ! Get value, whether from scalar or array
      value = data

      start = [time_index]
      count = [1]

      call nc_check(nf90_put_var(ncid, varid, [value], start=start, count=count))
   end subroutine nc_write_scalar

   !> Write a 1D variable to a NetCDF file
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] varid       Variable ID in the file
   !> @param[in] data        1D array data to write
   !> @param[in] time_index  Index of the time step to write
   subroutine nc_write_1d(ncid, varid, data, time_index)
      integer, intent(in) :: ncid, varid, time_index
      real, intent(in) :: data(:)    ! Accepts any 1D array
      integer :: start(2), count(2)

      start = [1, time_index]
      count = [size(data, 1), 1]

      call nc_check(nf90_put_var(ncid, varid, data, start=start, count=count))
   end subroutine nc_write_1d

   !> Write a 2D variable to a NetCDF file
   !>
   !> In NC4PAR mode, uses global offsets (iminmpi, jminmpi) for parallel write
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] varid       Variable ID in the file
   !> @param[in] data        2D array data to write
   !> @param[in] time_index  Index of the time step to write
   subroutine nc_write_2d(ncid, varid, data, time_index)
      integer, intent(in) :: ncid, varid, time_index
      real, intent(in) :: data(:, :)    ! Accepts any 2D array
      integer :: start(3), count(3)

#ifdef NC4PAR
      ! Parallel mode: write at global offset
      start = [iminmpi, jminmpi, time_index]
#else
      start = [1, 1, time_index]
#endif
      count = [size(data, 1), size(data, 2), 1]

      call nc_check(nf90_put_var(ncid, varid, data, start=start, count=count))
   end subroutine nc_write_2d

   !> Write a 3D variable to a NetCDF file
   !>
   !> In NC4PAR mode, uses global offsets (iminmpi, jminmpi) for parallel write
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] varid       Variable ID in the file
   !> @param[in] data        3D array data to write
   !> @param[in] time_index  Index of the time step to write
   subroutine nc_write_3d(ncid, varid, data, time_index)
      integer, intent(in) :: ncid, varid, time_index
      real, intent(in) :: data(:, :, :)    ! Accepts any 3D array
      integer :: start(4), count(4)

#ifdef NC4PAR
      ! Parallel mode: write at global offset
      start = [iminmpi, jminmpi, 1, time_index]
#else
      start = [1, 1, 1, time_index]
#endif
      count = [size(data, 1), size(data, 2), size(data, 3), 1]

      call nc_check(nf90_put_var(ncid, varid, data, start=start, count=count))
   end subroutine nc_write_3d

end module netcdf_backend
