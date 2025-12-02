!===============================================================================
!> @file io_naming.F90
!>
!> File naming conventions for the I/O system
!>
!> This module handles the generation of output filenames based on
!> configurable naming conventions. It is backend-independent.
!>
!> Default convention: {prefix}_{type}_{frequency}s.{extension}
!> Example: ocean_his_3600s.nc
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module io_naming
   use io_constants, only: IO_PATH_LEN, IO_PREFIX_LEN, IO_FILETYPE_LEN
   implicit none
   private

   public :: generate_filename
   public :: parse_filename
   public :: set_default_extension
   public :: get_default_extension

   !> Default file extension (can be changed by backend)
   character(len=16), private :: default_extension = "nc"

contains

   !> Set the default file extension
   !>
   !> This should be called by the backend during initialization
   !> to set the appropriate extension (.nc, .h5, .zarr, etc.)
   !>
   !> @param[in] ext  File extension without the dot (e.g., "nc", "h5")
   subroutine set_default_extension(ext)
      character(len=*), intent(in) :: ext
      default_extension = ext
   end subroutine set_default_extension

   !> Get the current default file extension
   !>
   !> @return Current default extension
   function get_default_extension() result(ext)
      character(len=16) :: ext
      ext = default_extension
   end function get_default_extension

   !> Generate a filename for an output file
   !>
   !> Constructs a filename following the convention:
   !>   {prefix}_{name}_{frequency}s.{extension}
   !>
   !> If frequency <= 0, the frequency part is omitted:
   !>   {prefix}_{name}.{extension}
   !>
   !> @param[in] prefix     File prefix (e.g., "ocean", "model")
   !> @param[in] file_name  Logical name of file (e.g., "hourly", "daily_avg")
   !> @param[in] freq       Output frequency in seconds (<=0 to omit)
   !> @param[in] extension  Optional: file extension (default: module default)
   !> @return    Complete filename
   function generate_filename(prefix, file_name, freq, extension) result(filename)
      character(len=*), intent(in) :: prefix
      character(len=*), intent(in) :: file_name
      real, intent(in) :: freq
      character(len=*), intent(in), optional :: extension
      character(len=IO_PATH_LEN) :: filename

      character(len=16) :: freq_str
      character(len=16) :: ext

      ! Determine extension
      if (present(extension)) then
         ext = extension
      else
         ext = default_extension
      end if

      ! Build filename
      if (freq > 0) then
         write(freq_str, '(I0)') nint(freq)
         filename = trim(prefix)//'_'//trim(file_name)//'_'//trim(freq_str)//'s.'//trim(ext)
      else
         filename = trim(prefix)//'_'//trim(file_name)//'.'//trim(ext)
      end if
   end function generate_filename

   !> Parse a filename to extract components
   !>
   !> Attempts to parse a filename following the convention:
   !>   {prefix}_{type}_{frequency}s.{extension}
   !>
   !> @param[in]  filename   Filename to parse
   !> @param[out] prefix     Extracted prefix
   !> @param[out] file_type  Extracted file type
   !> @param[out] freq       Extracted frequency (-1 if not present)
   !> @param[out] extension  Extracted extension
   !> @return     True if parsing succeeded, false otherwise
   function parse_filename(filename, prefix, file_type, freq, extension) result(success)
      character(len=*), intent(in) :: filename
      character(len=*), intent(out) :: prefix
      character(len=*), intent(out) :: file_type
      real, intent(out) :: freq
      character(len=*), intent(out) :: extension
      logical :: success

      integer :: pos_dot, pos_last_us
      integer :: freq_int, ios
      character(len=IO_PATH_LEN) :: basename, freq_part

      success = .false.
      prefix = ""
      file_type = ""
      freq = -1.0
      extension = ""

      ! Find extension
      pos_dot = index(filename, '.', back=.true.)
      if (pos_dot <= 1) return

      extension = filename(pos_dot+1:)
      basename = filename(1:pos_dot-1)

      ! Find last underscore (before frequency or type)
      pos_last_us = index(basename, '_', back=.true.)
      if (pos_last_us <= 1) return

      ! Check if last part is frequency (ends with 's' and is numeric)
      freq_part = basename(pos_last_us+1:)
      if (len_trim(freq_part) > 1 .and. freq_part(len_trim(freq_part):len_trim(freq_part)) == 's') then
         ! Try to parse as frequency
         read(freq_part(1:len_trim(freq_part)-1), '(I10)', iostat=ios) freq_int
         if (ios == 0) then
            freq = real(freq_int)
            basename = basename(1:pos_last_us-1)
            pos_last_us = index(basename, '_', back=.true.)
            if (pos_last_us <= 0) return
         end if
      end if

      ! Extract file type
      file_type = basename(pos_last_us+1:)
      
      ! Extract prefix
      prefix = basename(1:pos_last_us-1)

      ! Validate file type
      if (trim(file_type) /= "his" .and. &
          trim(file_type) /= "avg" .and. &
          trim(file_type) /= "rst") then
         success = .false.
         return
      end if

      success = .true.
   end function parse_filename

end module io_naming
