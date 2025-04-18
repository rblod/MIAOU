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
   implicit none
   private

   ! Public interface
   public :: nc_check, nc_define_variable, nc_write_variable

   !> Generic interface for writing variables of different dimensions
   !>
   !> This interface selects the appropriate specific procedure based on
   !> the dimensionality of the data being written.
   interface nc_write_variable
      module procedure nc_write_scalar
      module procedure nc_write_1d
      module procedure nc_write_2d
      module procedure nc_write_3d
   end interface nc_write_variable

contains

   !---------------------------------------------------------------------------
   ! Variable definition
   !---------------------------------------------------------------------------

   !> Define a variable in NetCDF file based on its dimensions
   !>
   !> This subroutine handles defining both scalar and n-dimensional variables,
   !> creating any dimensions that don't already exist in the file.
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
      integer, intent(in) :: ncid, ndims, time_dimid
      character(len=*), intent(in) :: var_name, var_long_name, var_units
      type(axis), intent(inout) :: grid_axes(:)
      integer, intent(out) :: varid

      integer :: i, ncerr, dim_id
      integer, allocatable :: dim_ids(:)

      ! Define all spatial dimensions from the grid
      do i = 1, size(grid_axes)
         ! Check if dimension already exists
         ncerr = nf90_inq_dimid(ncid, trim(grid_axes(i)%name), dim_id)

         if (ncerr /= nf90_noerr) then
            ! If not, create it
            if (grid_axes(i)%is_unlimited) then
               call nc_check(nf90_def_dim(ncid, trim(grid_axes(i)%name), &
                                          nf90_unlimited, dim_id), &
                             "Create axis : "//trim(grid_axes(i)%name))
            else
               call nc_check(nf90_def_dim(ncid, trim(grid_axes(i)%name), &
                                          grid_axes(i)%size, dim_id), &
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

      ! Add attributes
      call nc_check(nf90_put_att(ncid, varid, "long_name", trim(var_long_name)), &
                    "Create attribute : "//trim(var_long_name))

      call nc_check(nf90_put_att(ncid, varid, "units", trim(var_units)), &
                    "Create attribute : "//trim(var_units))

      deallocate (dim_ids)
   end subroutine nc_define_variable

   !---------------------------------------------------------------------------
   ! Variable writing for different dimensions
   !---------------------------------------------------------------------------

   !> Write a scalar (0D) variable to a NetCDF file
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] varid       Variable ID in the file
   !> @param[in] data        Scalar data to write
   !> @param[in] time_index  Index of the time step to write
   subroutine nc_write_scalar(ncid, varid, data, time_index)
      integer, intent(in) :: ncid, varid, time_index
      real, intent(in) :: data
      integer :: start(1), count(1)

      start = [time_index]
      count = [1]

      call nc_check(nf90_put_var(ncid, varid, [data], start=start, count=count))
   end subroutine nc_write_scalar

   !> Write a 1D variable to a NetCDF file
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] varid       Variable ID in the file
   !> @param[in] data        1D array data to write
   !> @param[in] time_index  Index of the time step to write
   subroutine nc_write_1d(ncid, varid, data, time_index)
      integer, intent(in) :: ncid, varid, time_index
      real, intent(in) :: data(:)
      integer :: start(2), count(2)

      start = [1, time_index]
      count = [size(data, 1), 1]

      call nc_check(nf90_put_var(ncid, varid, data, start=start, count=count))
   end subroutine nc_write_1d

   !> Write a 2D variable to a NetCDF file
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] varid       Variable ID in the file
   !> @param[in] data        2D array data to write
   !> @param[in] time_index  Index of the time step to write
   subroutine nc_write_2d(ncid, varid, data, time_index)
      integer, intent(in) :: ncid, varid, time_index
      real, intent(in) :: data(:, :)
      integer :: start(3), count(3)

      start = [1, 1, time_index]
      count = [size(data, 1), size(data, 2), 1]

      call nc_check(nf90_put_var(ncid, varid, data, start=start, count=count))
   end subroutine nc_write_2d

   !> Write a 3D variable to a NetCDF file
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] varid       Variable ID in the file
   !> @param[in] data        3D array data to write
   !> @param[in] time_index  Index of the time step to write
   subroutine nc_write_3d(ncid, varid, data, time_index)
      integer, intent(in) :: ncid, varid, time_index
      real, intent(in) :: data(:, :, :)
      integer :: start(4), count(4)

      start = [1, 1, 1, time_index]
      count = [size(data, 1), size(data, 2), size(data, 3), 1]

      call nc_check(nf90_put_var(ncid, varid, data, start=start, count=count))
   end subroutine nc_write_3d

end module netcdf_backend
