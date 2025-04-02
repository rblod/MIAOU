!===============================================================================
! file_manager.F90 : Generalized Output Manager with Grid-Based Dynamic Dimensions and Unlimited Time Axis
!===============================================================================
!>
!>
!>
module file_manager
   use netcdf
   implicit none
   public :: register_variable, define_output_file, write_output, check, generate_filename

   type :: grid
      character(len=16) :: name                 ! e.g. "rho", "u", "v"
      character(len=32), allocatable :: dim_names(:)   ! e.g. ["xi_rho", "eta_rho", "s_rho", "time"]
      integer, allocatable :: dim_sizes(:)             ! e.g. [nx, ny, nz, 0]
      integer, allocatable :: dim_ids(:)               ! NetCDF IDs (assigned per file)
      logical :: has_time = .false.
   end type

   type :: nc_var
      character(len=32) :: name
      character(len=64) :: long_name
      character(len=32) :: units
      type(grid) :: var_grid
      integer :: varid_his = -1
      integer :: varid_avg = -1
      integer :: varid_rst = -1
      logical :: to_his = .false.
      logical :: to_avg = .false.
      logical :: to_rst = .false.
      real, pointer :: data(:, :) => null()
      real, allocatable :: avg_buffer(:, :)
      integer :: current_time_index = 1
      real :: freq_his = -1.        ! Fréquence pour history
      real :: freq_avg = -1.        ! Fréquence pour average
      real :: freq_rst = -1.        ! Fréquence pour restart
      character(len=128) :: file_prefix = ""
   end type

   ! Structure pour suivre les fichiers ouverts
   type :: output_file
      character(len=256) :: filename  ! Nom complet du fichier
      character(len=16) :: type       ! "his", "avg", ou "rst"
      real :: freq                    ! Fréquence d'écriture
      integer :: ncid                 ! ID NetCDF
      integer :: time_dimid           ! ID dimension temps
      integer :: time_varid           ! ID variable temps
      integer :: time_index = 1       ! Index d'écriture du temps
   end type

   type(nc_var), allocatable :: registered_vars(:)
   type(output_file), allocatable :: open_files(:)
!>
!>
!> @param var_prefix, file_type, freq
!>
!> @param var_prefix, file_type, freq

contains

   function generate_filename(var_prefix, file_type, freq) result(filename)
      use namelist_output, only: his_prefix, avg_prefix, rst_prefix
      character(len=*), intent(in) :: var_prefix, file_type
      real, intent(in) :: freq
      character(len=256) :: filename
      character(len=16) :: freq_str
      character(len=128) :: prefix

      ! Déterminer le préfixe à utiliser (spécifique ou global)
      if (trim(var_prefix) == "") then
         ! Utiliser le préfixe global selon le type
         select case (trim(file_type))
         case ("his")
            prefix = his_prefix
         case ("avg")
            prefix = avg_prefix
         case ("rst")
            prefix = rst_prefix
         case default
            prefix = "output"
         end select
      else
         ! Utiliser le préfixe spécifique à la variable
         prefix = var_prefix
      end if

      ! Convertir la fréquence en chaîne de caractères
      if (freq > 0) then
         write (freq_str, '(I0)') nint(freq)
         filename = trim(prefix)//'_'//trim(freq_str)//'s.nc'
      else
         filename = trim(prefix)//'.nc'
      end if
   end function generate_filename

   function find_or_create_file(prefix, file_type, freq, time_units, calendar) result(ncid)
      character(len=*), intent(in) :: prefix, file_type
      real, intent(in) :: freq
      character(len=*), intent(in), optional :: time_units, calendar
      integer :: ncid
      character(len=256) :: filename
      integer :: i, file_idx
      logical :: file_exists

      ! Génère le nom du fichier avec la fréquence
      filename = generate_filename(prefix, file_type, freq)

      ! Vérifie si le fichier est déjà ouvert
      file_exists = .false.
      file_idx = -1
      if (allocated(open_files)) then
         do i = 1, size(open_files)
            if (trim(open_files(i)%filename) == trim(filename)) then
               file_exists = .true.
               file_idx = i
               ncid = open_files(i)%ncid
               exit
            end if
         end do
      end if

      ! Si le fichier n'existe pas, créer et ouvrir
      if (.not. file_exists) then
         ! Crée le fichier
         call check(nf90_create(filename, nf90_clobber, ncid))

         ! Ajoute aux fichiers ouverts
         if (.not. allocated(open_files)) then
            allocate (open_files(1))
            file_idx = 1
         else
            ! Redimensionne le tableau des fichiers ouverts
            call add_to_open_files(filename, file_type, freq, ncid)
            file_idx = size(open_files)
         end if

         ! Initialise la nouvelle entrée
         open_files(file_idx)%filename = filename
         open_files(file_idx)%type = file_type
         open_files(file_idx)%freq = freq
         open_files(file_idx)%ncid = ncid
         open_files(file_idx)%time_index = 1

         ! Configure la dimension temps si nécessaire
         if (file_type /= "rst") then
            call check(nf90_def_dim(ncid, "time", nf90_unlimited, open_files(file_idx)%time_dimid))

            if (present(time_units)) then
               call check(nf90_def_var(ncid, "time", nf90_real, [open_files(file_idx)%time_dimid], &
                                       open_files(file_idx)%time_varid))
               call check(nf90_put_att(ncid, open_files(file_idx)%time_varid, "units", trim(time_units)))

               if (present(calendar)) then
                  call check(nf90_put_att(ncid, open_files(file_idx)%time_varid, "calendar", trim(calendar)))
               end if
            end if
         end if
      end if

      ! Retourne l'ID du fichier
      ncid = open_files(file_idx)%ncid
   end function find_or_create_file
!>
!>
!> @param filename, file_type, freq, ncid
!>
!> @param filename, file_type, freq, ncid

   subroutine add_to_open_files(filename, file_type, freq, ncid)
      character(len=*), intent(in) :: filename, file_type
      real, intent(in) :: freq
      integer, intent(in) :: ncid
      type(output_file), allocatable :: temp(:)
      integer :: n

      if (.not. allocated(open_files)) then
         allocate (open_files(1))
         open_files(1)%filename = filename
         open_files(1)%type = file_type
         open_files(1)%freq = freq
         open_files(1)%ncid = ncid
         open_files(1)%time_index = 1
      else
         n = size(open_files)
         allocate (temp(n))
         temp = open_files
         deallocate (open_files)
         allocate (open_files(n + 1))
         open_files(1:n) = temp
         open_files(n + 1)%filename = filename
         open_files(n + 1)%type = file_type
         open_files(n + 1)%freq = freq
         open_files(n + 1)%ncid = ncid
         open_files(n + 1)%time_index = 1
      end if
   end subroutine add_to_open_files
!>
!>
!> @param v
!>
!> @param v

   subroutine register_variable(v)
      type(nc_var), intent(in) :: v
      type(nc_var), allocatable :: tmp(:)
      integer :: n

      if (.not. allocated(registered_vars)) then
         allocate (registered_vars(1))
         registered_vars(1) = v
         if (associated(v%data)) then
            registered_vars(1)%data => v%data
         else
            print *, "Warning: data not associated for variable ", trim(v%name)
         end if
      else
         n = size(registered_vars)
         allocate (tmp(n))
         tmp = registered_vars
         deallocate (registered_vars)
         allocate (registered_vars(n + 1))
         registered_vars(1:n) = tmp
         registered_vars(n + 1) = v
         if (associated(v%data)) then
            registered_vars(n + 1)%data => v%data
         else
            print *, "Warning: data not associated for variable ", trim(v%name)
         end if
      end if
   end subroutine register_variable
!>
!>
!> @param ncid, tag
!>
!> @param ncid, tag

   subroutine define_output_file(ncid, tag)
      integer, intent(in) :: ncid
      character(len=*), intent(in) :: tag
      integer :: i, j, ncerr
      integer :: dim_id  ! Variable temporaire pour vérifier l'existence d'une dimension
      integer :: time_dimid_local

      ! Trouve l'entrée de fichier correspondante
      do i = 1, size(open_files)
         if (open_files(i)%ncid == ncid) then
            time_dimid_local = open_files(i)%time_dimid
            exit
         end if
      end do

      ! Définir les dimensions et variables pour les variables enregistrées
      do i = 1, size(registered_vars)
         select case (tag)
         case ("his")
            if (.not. registered_vars(i)%to_his) cycle
         case ("avg")
            if (.not. registered_vars(i)%to_avg) cycle
         case ("rst")
            if (.not. registered_vars(i)%to_rst) cycle
         end select

         ! Réinitialiser les IDs de dimension de la grille
         registered_vars(i)%var_grid%dim_ids = -1

         ! Définir les dimensions de la grille avec vérification d'unicité
         do j = 1, size(registered_vars(i)%var_grid%dim_names)
            ! Vérifier si la dimension existe déjà
            ncerr = nf90_inq_dimid(ncid, &
                                   trim(registered_vars(i)%var_grid%dim_names(j)), &
                                   dim_id)

            if (ncerr /= nf90_noerr) then
               ! La dimension n'existe pas, la définir
               ncerr = nf90_def_dim(ncid, &
                                    trim(registered_vars(i)%var_grid%dim_names(j)), &
                                    registered_vars(i)%var_grid%dim_sizes(j), &
                                    registered_vars(i)%var_grid%dim_ids(j))
               call check(ncerr)
            else
               ! La dimension existe, stocker son ID
               registered_vars(i)%var_grid%dim_ids(j) = dim_id
            end if
         end do

         ! Définir la variable en fonction du tag
         select case (tag)
         case ("his")
            ncerr = nf90_def_var(ncid, registered_vars(i)%name, nf90_real, &
                                 [registered_vars(i)%var_grid%dim_ids(1:2), time_dimid_local], &
                                 registered_vars(i)%varid_his)
            call check(ncerr)
            ! Ajouter les attributs
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_his, "long_name", &
                                 trim(registered_vars(i)%long_name))
            call check(ncerr)
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_his, "units", &
                                 trim(registered_vars(i)%units))
            call check(ncerr)
         case ("avg")
            ncerr = nf90_def_var(ncid, registered_vars(i)%name, nf90_real, &
                                 [registered_vars(i)%var_grid%dim_ids(1:2), time_dimid_local], &
                                 registered_vars(i)%varid_avg)
            call check(ncerr)
            ! Ajouter les attributs
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_avg, "long_name", &
                                 trim(registered_vars(i)%long_name))
            call check(ncerr)
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_avg, "units", &
                                 trim(registered_vars(i)%units))
            call check(ncerr)
            if (.not. allocated(registered_vars(i)%avg_buffer)) then
               allocate (registered_vars(i)%avg_buffer(size(registered_vars(i)%data, 1),&
               &size(registered_vars(i)%data, 2)))
            end if
         case ("rst")
            ncerr = nf90_def_var(ncid, registered_vars(i)%name, nf90_real, &
                                 registered_vars(i)%var_grid%dim_ids(1:2), &
                                 registered_vars(i)%varid_rst)
            call check(ncerr)
            ! Ajouter les attributs
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_rst, "long_name", &
                                 trim(registered_vars(i)%long_name))
            call check(ncerr)
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_rst, "units", &
                                 trim(registered_vars(i)%units))
            call check(ncerr)
         end select
      end do

   end subroutine define_output_file

   function find_grid_ids(target, pool) result(dim_ids)
      type(grid), intent(in) :: target
      type(grid), intent(in) :: pool(:)
      integer, allocatable :: dim_ids(:)
      integer :: i
      do i = 1, size(pool)
         if (target%name == pool(i)%name) then
            dim_ids = pool(i)%dim_ids
            return
         end if
      end do
      allocate (dim_ids(0))
   end function find_grid_ids
!>
!>
!> @param ncid, v, vid
!>
!> @param ncid, v, vid

   subroutine define_one(ncid, v, vid)
      integer, intent(in) :: ncid
      type(nc_var), intent(in) :: v
      integer, intent(out) :: vid
      call check(nf90_def_var(ncid, v%name, nf90_real, v%var_grid%dim_ids, vid))
      call check(nf90_put_att(ncid, vid, "long_name", v%long_name))
      call check(nf90_put_att(ncid, vid, "units", v%units))
   end subroutine define_one
!>
!>
!> @param ncid, tag, time_value
!>
!> @param ncid, tag, time_value

   subroutine write_output(ncid, tag, time_value)
      integer, intent(in) :: ncid
      character(len=*), intent(in) :: tag
      real, intent(in) :: time_value
      integer :: i, file_idx
      integer, dimension(3) :: start, count
      real :: time_slice(1)
      logical :: should_write, any_written
      integer :: steps_since_last_write
      real :: freq
      real, parameter :: TOL = 1.0e-5

      ! Trouve l'index du fichier
      file_idx = -1
      do i = 1, size(open_files)
         if (open_files(i)%ncid == ncid) then
            file_idx = i
            exit
         end if
      end do

      if (file_idx == -1) then
         print *, "Error: File not found in open_files list"
         return
      end if

      ! Initialiser le drapeau d'écriture
      any_written = .false.

      ! Accumuler les données pour les fichiers avg à chaque pas de temps
      do i = 1, size(registered_vars)
         if (tag == "avg" .and. registered_vars(i)%to_avg) then
            ! Vérifier si le buffer est alloué
            if (.not. allocated(registered_vars(i)%avg_buffer)) then
               allocate (registered_vars(i)%avg_buffer(size(registered_vars(i)%data, 1), &
                                                       size(registered_vars(i)%data, 2)))
               registered_vars(i)%avg_buffer = 0.0
               print *, "Initialized avg_buffer for ", trim(registered_vars(i)%name)
            end if

            ! Accumuler les données, même si on n'écrit pas à ce pas de temps
            registered_vars(i)%avg_buffer = registered_vars(i)%avg_buffer + registered_vars(i)%data

            print *, "Accumulated ", trim(registered_vars(i)%name), &
               " buffer sum=", sum(registered_vars(i)%avg_buffer)
         end if
      end do

      ! Parcourir toutes les variables enregistrées pour déterminer si on écrit
      do i = 1, size(registered_vars)
         should_write = .false.

         ! Vérifions d'abord si la variable est active pour ce type de fichier
         select case (tag)
         case ("his")
            if (.not. registered_vars(i)%to_his) cycle
            freq = registered_vars(i)%freq_his  ! Utiliser la fréquence history
         case ("avg")
            if (.not. registered_vars(i)%to_avg) cycle
            freq = registered_vars(i)%freq_avg  ! Utiliser la fréquence average
         case ("rst")
            if (.not. registered_vars(i)%to_rst) cycle
            freq = registered_vars(i)%freq_rst  ! Utiliser la fréquence restart
         end select

         ! Vérifier le préfixe du fichier
         if (trim(registered_vars(i)%file_prefix) /= "") then
            if (index(open_files(file_idx)%filename, trim(registered_vars(i)%file_prefix)) /= 1) cycle
         end if

         ! Déterminons si c'est le moment d'écrire
         if (abs(time_value) < TOL) then
            ! Premier pas de temps, toujours écrire
            should_write = .true.
         else
            ! Vérifier si c'est un multiple de la fréquence spécifique au type
            should_write = abs(mod(time_value, freq)) < TOL
         end if

         ! Si on doit écrire, mettre à jour le drapeau
         if (should_write) then
            any_written = .true.
            exit
         end if
      end do

      ! Si rien à écrire, sortir sans incrémenter l'index de temps
      if (.not. any_written) then
         return
      end if

      ! Maintenant que nous savons qu'il y a quelque chose à écrire,
      ! écrire la valeur du temps d'abord
      if (tag /= "rst" .and. open_files(file_idx)%time_varid /= -1) then
         time_slice(1) = time_value
         call check(nf90_put_var(ncid, open_files(file_idx)%time_varid, time_slice, &
                                 start=[open_files(file_idx)%time_index], count=[1]))
      end if

      ! Parcourir à nouveau les variables pour l'écriture
      do i = 1, size(registered_vars)
         should_write = .false.

         ! Mêmes vérifications que précédemment
         select case (tag)
         case ("his")
            if (.not. registered_vars(i)%to_his) cycle
            freq = registered_vars(i)%freq_his
         case ("avg")
            if (.not. registered_vars(i)%to_avg) cycle
            freq = registered_vars(i)%freq_avg
         case ("rst")
            if (.not. registered_vars(i)%to_rst) cycle
            freq = registered_vars(i)%freq_rst
         end select

         if (trim(registered_vars(i)%file_prefix) /= "") then
            if (index(open_files(file_idx)%filename, trim(registered_vars(i)%file_prefix)) /= 1) cycle
         end if

         if (abs(time_value) < TOL) then
            should_write = .true.
         else
            should_write = abs(mod(time_value, freq)) < TOL
         end if

         ! Si nous devons écrire, procédons
         if (should_write) then
            print *, "Writing ", trim(registered_vars(i)%name), " to ", trim(tag), &
               " file at time=", time_value, " index=", open_files(file_idx)%time_index

            select case (tag)
            case ("his")
               if (registered_vars(i)%varid_his > 0) then
                  start = [1, 1, open_files(file_idx)%time_index]
                  count = [size(registered_vars(i)%data, 1), size(registered_vars(i)%data, 2), 1]

                  call check(nf90_put_var(ncid, registered_vars(i)%varid_his, &
                                          registered_vars(i)%data, &
                                          start=start, count=count))
               end if

            case ("avg")
               ! Calculer le nombre de pas de temps inclus
               if (abs(time_value) < TOL) then
                  steps_since_last_write = 1
               else
                  steps_since_last_write = nint(freq/1800.0)  ! Utiliser la fréquence AVG
               end if

               ! Vérifier que le buffer contient quelque chose
               if (maxval(abs(registered_vars(i)%avg_buffer)) > TOL) then
                  if (registered_vars(i)%varid_avg > 0) then
                     start = [1, 1, open_files(file_idx)%time_index]
                     count = [size(registered_vars(i)%data, 1), size(registered_vars(i)%data, 2), 1]

                     print *, "Writing AVG for ", trim(registered_vars(i)%name), &
                        " steps=", steps_since_last_write, &
                        " sum=", sum(registered_vars(i)%avg_buffer), &
                        " avg=", sum(registered_vars(i)%avg_buffer)/real(steps_since_last_write)

                     ! Calculer la moyenne directement lors de l'écriture
                     call check(nf90_put_var(ncid, registered_vars(i)%varid_avg, &
                                             registered_vars(i)%avg_buffer/real(steps_since_last_write), &
                                             start=start, count=count))
                  end if
               else
                  print *, "Warning: Empty avg_buffer for ", trim(registered_vars(i)%name)
               end if

               ! Réinitialiser le buffer pour la prochaine période
               registered_vars(i)%avg_buffer = 0.0

            case ("rst")
               if (registered_vars(i)%varid_rst > 0) then
                  call check(nf90_put_var(ncid, registered_vars(i)%varid_rst, &
                                          registered_vars(i)%data))
               end if
            end select
         end if
      end do

      ! Incrémenter l'index de temps uniquement si quelque chose a été écrit
      if (tag /= "rst") then
         open_files(file_idx)%time_index = open_files(file_idx)%time_index + 1
         print *, "Incremented time index to ", open_files(file_idx)%time_index, " for file ", &
            trim(open_files(file_idx)%filename)
      end if
   end subroutine write_output
!>
!>
!> @param time_units, calendar
!>
!> @param time_units, calendar

   subroutine initialize_output_files(time_units, calendar)
      character(len=*), intent(in), optional :: time_units, calendar
      integer :: i, ncid_his, ncid_avg, ncid_rst
      real :: freq

      ! Vérifier que des variables sont enregistrées
      if (.not. allocated(registered_vars)) then
         print *, "Warning: No variables registered before initializing output files"
         return
      end if

      ! Pour chaque variable enregistrée
      do i = 1, size(registered_vars)
         ! Gérer les fichiers History
         if (registered_vars(i)%to_his) then
            freq = registered_vars(i)%freq_his
            ncid_his = find_or_create_file(trim(registered_vars(i)%file_prefix), &
                                           "his", freq, time_units, calendar)
            call define_output_file(ncid_his, "his")
            call check(nf90_enddef(ncid_his))
         end if

         ! Gérer les fichiers Average
         if (registered_vars(i)%to_avg) then
            freq = registered_vars(i)%freq_avg
            ncid_avg = find_or_create_file(trim(registered_vars(i)%file_prefix), &
                                           "avg", freq, time_units, calendar)
            call define_output_file(ncid_avg, "avg")
            call check(nf90_enddef(ncid_avg))
         end if

         ! Gérer les fichiers Restart
         if (registered_vars(i)%to_rst) then
            freq = registered_vars(i)%freq_rst
            ncid_rst = find_or_create_file(trim(registered_vars(i)%file_prefix), &
                                           "rst", freq, time_units, calendar)
            call define_output_file(ncid_rst, "rst")
            call check(nf90_enddef(ncid_rst))
         end if
      end do
   end subroutine initialize_output_files
!>
!>
!> @param
!>
!> @param

   subroutine close_all_output_files()
      integer :: i

      if (.not. allocated(open_files)) then
         print *, "Warning: No files to close"
         return
      end if

      do i = 1, size(open_files)
         call check(nf90_close(open_files(i)%ncid))
         print *, "Closed file: ", trim(open_files(i)%filename)
      end do

      ! Libérer la mémoire du tableau des fichiers ouverts
      deallocate (open_files)
   end subroutine close_all_output_files
!>
!>
!> @param current_time
!>
!> @param current_time

   subroutine write_all_outputs(current_time)
      real, intent(in) :: current_time
      integer :: i

      if (.not. allocated(open_files)) then
         print *, "Warning: No files open for writing"
         return
      end if

      do i = 1, size(open_files)
         ! Vérifier si c'est le moment d'écrire dans ce fichier
         ! Pour les fichiers restart, on écrit seulement à la fin ou selon leur fréquence
         if (trim(open_files(i)%type) == "rst") then
            if (mod(current_time, open_files(i)%freq) < 1e-5) then
               call write_output(open_files(i)%ncid, "rst", current_time)
            end if
         else
            ! Pour les fichiers history et average, on écrit à chaque pas de temps
            call write_output(open_files(i)%ncid, trim(open_files(i)%type), current_time)
         end if
      end do
   end subroutine write_all_outputs
!>
!>
!> @param ierr
!>
!> @param ierr

   subroutine check(ierr)
      integer, intent(in) :: ierr
      if (ierr /= nf90_noerr) then
         print *, 'NetCDF error:', trim(nf90_strerror(ierr))
         stop
      end if
   end subroutine check

end module file_manager
