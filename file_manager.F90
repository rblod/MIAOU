!===============================================================================
! file_manager.F90 : Generalized Output Manager with Grid-Based Dynamic Dimensions and Unlimited Time Axis
!===============================================================================
!>
!>
!>
module file_manager
   use netcdf
   use grid_module    ! Nouveau module de grille
   use namelist_output, only: his_prefix, avg_prefix, rst_prefix  ! Ajouter cette ligne

   implicit none
   public :: register_variable, define_output_file, write_output, check, generate_filename
   public :: initialize_output_files, close_all_output_files, write_all_outputs

   ! Type modifié pour remplacer l'ancien type grid par le nouveau
   type :: nc_var  !! structure for writing variables (defined in registry)
      character(len=32) :: name
      character(len=64) :: long_name
      character(len=32) :: units
      type(grid) :: var_grid      ! Utilisation du nouveau type grid
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

   type :: output_file  !! Structure for open files
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

      ! Determine the base prefix
      if (trim(var_prefix) == "") then
         ! Global from namelist
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
         ! Specific variable prefix, but still include the file type
         select case (trim(file_type))
         case ("his")
            prefix = trim(var_prefix)//"_his"
         case ("avg")
            prefix = trim(var_prefix)//"_avg"
         case ("rst")
            prefix = trim(var_prefix)//"_rst"
         case default
            prefix = trim(var_prefix)
         end select
      end if

      ! Convert freq to string
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

      ! File name
      filename = generate_filename(prefix, file_type, freq)

      ! It's open or not
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

      ! If not open
      if (.not. file_exists) then
         ! Create it
         call check(nf90_create(filename, nf90_clobber, ncid))

         ! Create or Add to the openfiles list
         if (.not. allocated(open_files)) then
            ! Create
            allocate (open_files(1))
            file_idx = 1
         else
            ! Add
            call add_to_open_files(filename, file_type, freq, ncid)
            file_idx = size(open_files)
         end if

         ! Initialize the new file characteristics
         open_files(file_idx)%filename = filename
         open_files(file_idx)%type = file_type
         open_files(file_idx)%freq = freq
         open_files(file_idx)%ncid = ncid
         open_files(file_idx)%time_index = 1

         ! Time axis for all file types (including restart)
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

      ! File ID
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
      integer :: dim_id
      integer :: time_dimid_local
      integer :: axis_size
      integer, allocatable :: dim_ids(:)

      ! Find the good file
      do i = 1, size(open_files)
         if (open_files(i)%ncid == ncid) then
            time_dimid_local = open_files(i)%time_dimid
            exit
         end if
      end do

      !
      do i = 1, size(registered_vars)
         select case (tag)
         case ("his")
            if (.not. registered_vars(i)%to_his) cycle
         case ("avg")
            if (.not. registered_vars(i)%to_avg) cycle
         case ("rst")
            if (.not. registered_vars(i)%to_rst) cycle
         end select

         ! Définir toutes les dimensions de la grille
         do j = 1, registered_vars(i)%var_grid%ndims
            axis_size = registered_vars(i)%var_grid%axes(j)%size

            ! Vérifier si la dimension existe déjà
            ncerr = nf90_inq_dimid(ncid, trim(registered_vars(i)%var_grid%axes(j)%name), dim_id)

            if (ncerr /= nf90_noerr) then
               ! Si non, la créer
               if (registered_vars(i)%var_grid%axes(j)%is_unlimited) then
                  ncerr = nf90_def_dim(ncid, trim(registered_vars(i)%var_grid%axes(j)%name), &
                                       nf90_unlimited, dim_id)
               else
                  ncerr = nf90_def_dim(ncid, trim(registered_vars(i)%var_grid%axes(j)%name), &
                                       axis_size, dim_id)
               end if
               call check(ncerr)
            end if

            ! Stocker l'ID de la dimension
            registered_vars(i)%var_grid%axes(j)%id = dim_id
         end do

         ! Créer un tableau d'IDs de dimensions pour la définition de variable
         allocate (dim_ids(registered_vars(i)%var_grid%ndims + 1))  ! +1 pour le temps

         ! Copier les IDs des axes existants
         do j = 1, registered_vars(i)%var_grid%ndims
            dim_ids(j) = registered_vars(i)%var_grid%axes(j)%id
         end do

         ! Ajouter l'ID de la dimension temps
         dim_ids(registered_vars(i)%var_grid%ndims + 1) = time_dimid_local

         ! Variable definition
         select case (tag)
         case ("his")
            ncerr = nf90_def_var(ncid, registered_vars(i)%name, nf90_real, &
                                 dim_ids, registered_vars(i)%varid_his)
            call check(ncerr)
            ! Add attributes
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_his, "long_name", &
                                 trim(registered_vars(i)%long_name))
            call check(ncerr)
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_his, "units", &
                                 trim(registered_vars(i)%units))
            call check(ncerr)
         case ("avg")
            ncerr = nf90_def_var(ncid, registered_vars(i)%name, nf90_real, &
                                 dim_ids, registered_vars(i)%varid_avg)
            call check(ncerr)
            ! Add  attributes
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
            ncerr = nf90_def_var(ncid, registered_vars(i)%name, nf90_double, &
                                 dim_ids, registered_vars(i)%varid_rst)
            call check(ncerr)
            ! Add attributes
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_rst, "long_name", &
                                 trim(registered_vars(i)%long_name))
            call check(ncerr)
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_rst, "units", &
                                 trim(registered_vars(i)%units))
            call check(ncerr)
         end select

         ! Libérer la mémoire
         deallocate (dim_ids)
      end do

   end subroutine define_output_file

!>
!>
!> @param ncid, tag, time_value
!>
!> @param ncid, tag, time_value

   subroutine write_output(ncid, tag, time_value)
      use ocean_var, only: dt  ! Import time step from ocean_var
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
      character(len=256) :: current_filename

      ! Trouver l'index du fichier
      file_idx = -1
      do i = 1, size(open_files)
         if (open_files(i)%ncid == ncid) then
            file_idx = i
            current_filename = open_files(i)%filename
            exit
         end if
      end do

      if (file_idx == -1) then
         print *, "Error: File not found in open_files list"
         return
      end if

      any_written = .false.

      ! Parcourir toutes les variables enregistrées
      do i = 1, size(registered_vars)
         should_write = .false.

         ! Vérifier si cette variable doit être écrite dans ce type de fichier
         select case (tag)
         case ("his")
            if (.not. registered_vars(i)%to_his) cycle
            if (registered_vars(i)%varid_his <= 0) cycle  ! La variable n'est pas définie dans ce fichier
            freq = registered_vars(i)%freq_his
         case ("avg")
            if (.not. registered_vars(i)%to_avg) cycle
            if (registered_vars(i)%varid_avg <= 0) cycle  ! La variable n'est pas définie dans ce fichier
            freq = registered_vars(i)%freq_avg
         case ("rst")
            if (.not. registered_vars(i)%to_rst) cycle
            if (registered_vars(i)%varid_rst <= 0) cycle  ! La variable n'est pas définie dans ce fichier
            freq = registered_vars(i)%freq_rst
         end select

         ! Si ce fichier est pour une variable spécifique, vérifier que c'est la bonne variable
         if (index(current_filename, trim(registered_vars(i)%file_prefix)) /= 1 .and. &
             trim(registered_vars(i)%file_prefix) /= "") then
            cycle  ! Ce n'est pas le fichier pour cette variable
         end if

         ! Si ce fichier est un fichier global, vérifier que la variable n'a pas de préfixe spécifique
         if (trim(registered_vars(i)%file_prefix) == "") then
            ! Vérifier que le nom du fichier correspond au type global
            if (index(current_filename, trim(his_prefix)) == 1 .and. tag /= "his") cycle
            if (index(current_filename, trim(avg_prefix)) == 1 .and. tag /= "avg") cycle
            if (index(current_filename, trim(rst_prefix)) == 1 .and. tag /= "rst") cycle
         end if

         ! Cas des moyennes: accumuler les données
         if (tag == "avg" .and. registered_vars(i)%to_avg) then
            ! Allouer le buffer si nécessaire
            if (.not. allocated(registered_vars(i)%avg_buffer)) then
               allocate (registered_vars(i)%avg_buffer(size(registered_vars(i)%data, 1), &
                                                       size(registered_vars(i)%data, 2)))
               registered_vars(i)%avg_buffer = 0.0
               print *, "Initialized avg_buffer for ", trim(registered_vars(i)%name)
            end if

            ! Accumuler
            registered_vars(i)%avg_buffer = registered_vars(i)%avg_buffer + registered_vars(i)%data
            print *, "Accumulated ", trim(registered_vars(i)%name), &
               " buffer sum=", sum(registered_vars(i)%avg_buffer)
         end if

         ! Déterminer si on doit écrire à ce pas de temps
         if (abs(time_value) < TOL) then
            ! Premier pas de temps - toujours écrire
            should_write = .true.
         else
            ! Vérifier la fréquence
            should_write = abs(mod(time_value, freq)) < TOL
         end if

         if (should_write) any_written = .true.
      end do

      ! Si rien à écrire, sortir
      if (.not. any_written) return

      ! Écrire la valeur du temps
      if (open_files(file_idx)%time_varid /= -1) then
         time_slice(1) = time_value
         call check(nf90_put_var(ncid, open_files(file_idx)%time_varid, time_slice, &
                                 start=[open_files(file_idx)%time_index], count=[1]))
      end if

      ! Parcourir à nouveau les variables pour les écrire
      do i = 1, size(registered_vars)
         should_write = .false.

         ! Vérifier si cette variable doit être écrite dans ce type de fichier
         select case (tag)
         case ("his")
            if (.not. registered_vars(i)%to_his) cycle
            if (registered_vars(i)%varid_his <= 0) cycle  ! La variable n'est pas définie dans ce fichier
            freq = registered_vars(i)%freq_his
         case ("avg")
            if (.not. registered_vars(i)%to_avg) cycle
            if (registered_vars(i)%varid_avg <= 0) cycle  ! La variable n'est pas définie dans ce fichier
            freq = registered_vars(i)%freq_avg
         case ("rst")
            if (.not. registered_vars(i)%to_rst) cycle
            if (registered_vars(i)%varid_rst <= 0) cycle  ! La variable n'est pas définie dans ce fichier
            freq = registered_vars(i)%freq_rst
         end select

         ! Si ce fichier est pour une variable spécifique, vérifier que c'est la bonne variable
         if (index(current_filename, trim(registered_vars(i)%file_prefix)) /= 1 .and. &
             trim(registered_vars(i)%file_prefix) /= "") then
            cycle  ! Ce n'est pas le fichier pour cette variable
         end if

         ! Si ce fichier est un fichier global, vérifier que la variable n'a pas de préfixe spécifique
         if (trim(registered_vars(i)%file_prefix) == "") then
            ! Vérifier que le nom du fichier correspond au type global
            if (index(current_filename, trim(his_prefix)) == 1 .and. tag /= "his") cycle
            if (index(current_filename, trim(avg_prefix)) == 1 .and. tag /= "avg") cycle
            if (index(current_filename, trim(rst_prefix)) == 1 .and. tag /= "rst") cycle
         end if

         ! Déterminer si on doit écrire à ce pas de temps
         if (abs(time_value) < TOL) then
            ! Premier pas de temps - toujours écrire
            should_write = .true.
         else
            ! Vérifier la fréquence
            should_write = abs(mod(time_value, freq)) < TOL
         end if

         ! Écrire si nécessaire
         if (should_write) then
            print *, "Writing ", trim(registered_vars(i)%name), " to ", trim(tag), &
               " file at time=", time_value, " index=", open_files(file_idx)%time_index, &
               " file=", trim(current_filename)

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
               ! Déterminer le nombre de pas de temps depuis la dernière écriture pour la moyenne
               if (abs(time_value) < TOL) then
                  steps_since_last_write = 1
               else
                  steps_since_last_write = nint(freq/dt)  ! AVG
               end if

               ! Vérifier s'il y a quelque chose à écrire
               if (maxval(abs(registered_vars(i)%avg_buffer)) > TOL) then
                  if (registered_vars(i)%varid_avg > 0) then
                     start = [1, 1, open_files(file_idx)%time_index]
                     count = [size(registered_vars(i)%data, 1), size(registered_vars(i)%data, 2), 1]

                     print *, "Writing AVG for ", trim(registered_vars(i)%name), &
                        " steps=", steps_since_last_write, &
                        " sum=", sum(registered_vars(i)%avg_buffer), &
                        " avg=", sum(registered_vars(i)%avg_buffer)/real(steps_since_last_write)

                     ! Calculer la moyenne lors de l'écriture
                     call check(nf90_put_var(ncid, registered_vars(i)%varid_avg, &
                                             registered_vars(i)%avg_buffer/real(steps_since_last_write), &
                                             start=start, count=count))
                  end if
               else
                  print *, "Warning: Empty avg_buffer for ", trim(registered_vars(i)%name)
               end if

               ! Réinitialiser le buffer pour la prochaine période de moyenne
               registered_vars(i)%avg_buffer = 0.0

            case ("rst")
               if (registered_vars(i)%varid_rst > 0) then
                  start = [1, 1, open_files(file_idx)%time_index]
                  count = [size(registered_vars(i)%data, 1), size(registered_vars(i)%data, 2), 1]

                  ! Conversion explicite en double précision pour le redémarrage
                  call check(nf90_put_var(ncid, registered_vars(i)%varid_rst, &
                                          dble(registered_vars(i)%data), &
                                          start=start, count=count))
               end if
            end select
         end if
      end do

      ! Incrémenter l'index de temps pour tous les types de fichiers si quelque chose a été écrit
      if (any_written) then
         open_files(file_idx)%time_index = open_files(file_idx)%time_index + 1
         print *, "Incremented time index to ", open_files(file_idx)%time_index, " for file ", &
            trim(current_filename)
      end if
   end subroutine write_output
!>
!>
!> @param time_units, calendar
!>
!> @param time_units, calendar

   subroutine initialize_output_files(time_units, calendar)
      character(len=*), intent(in), optional :: time_units, calendar
      integer :: i, j, ncid
      real :: freq
      character(len=16) :: file_types(3) = ["his", "avg", "rst"]
      character(len=256) :: filename
      logical :: file_exists
      integer :: file_idx

      ! Check if variables are registered
      if (.not. allocated(registered_vars)) then
         print *, "Warning: No variables registered before initializing output files"
         return
      end if

      ! Parcourir chaque variable
      do i = 1, size(registered_vars)
         ! Parcourir chaque type de fichier
         do j = 1, size(file_types)
            ! Déterminer si cette variable doit être écrite dans ce type de fichier
            select case (trim(file_types(j)))
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

            ! Si la fréquence n'est pas définie, passer au suivant
            if (freq <= 0) cycle

            ! Générer le nom du fichier avec le préfixe approprié
            filename = generate_filename(registered_vars(i)%file_prefix, file_types(j), freq)

            ! Vérifier si le fichier existe déjà dans la liste
            file_exists = .false.
            if (allocated(open_files)) then
               do file_idx = 1, size(open_files)
                  if (trim(open_files(file_idx)%filename) == trim(filename)) then
                     ! Le fichier existe déjà, récupérer son ID
                     file_exists = .true.
                     ncid = open_files(file_idx)%ncid
                     exit
                  end if
               end do
            end if

            ! Si le fichier n'existe pas, le créer
            if (.not. file_exists) then
               print *, "Creating file: ", trim(filename), " for variable: ", trim(registered_vars(i)%name)

               ! Créer le fichier NetCDF
               call check(nf90_create(filename, nf90_clobber, ncid))

               ! Ajouter le fichier à la liste des fichiers ouverts
               if (.not. allocated(open_files)) then
                  allocate (open_files(1))
                  file_idx = 1
               else
                  call add_to_open_files(filename, file_types(j), freq, ncid)
                  file_idx = size(open_files)
               end if

               ! Initialiser les caractéristiques du fichier
               open_files(file_idx)%filename = filename
               open_files(file_idx)%type = file_types(j)
               open_files(file_idx)%freq = freq
               open_files(file_idx)%ncid = ncid
               open_files(file_idx)%time_index = 1

               ! Définir la dimension de temps
               call check(nf90_def_dim(ncid, "time", nf90_unlimited, open_files(file_idx)%time_dimid))

               ! Définir la variable de temps si les unités sont fournies
               if (present(time_units)) then
                  call check(nf90_def_var(ncid, "time", nf90_real, [open_files(file_idx)%time_dimid], &
                                          open_files(file_idx)%time_varid))
                  call check(nf90_put_att(ncid, open_files(file_idx)%time_varid, "units", trim(time_units)))
                  if (present(calendar)) then
                     call check(nf90_put_att(ncid, open_files(file_idx)%time_varid, "calendar", trim(calendar)))
                  end if
               end if
            end if

            ! Définir la variable dans ce fichier
            call define_single_variable(ncid, file_types(j), i)

            ! Terminer la définition des variables dans ce fichier
            call check(nf90_enddef(ncid))
         end do
      end do
   end subroutine initialize_output_files

! Nouvelle subroutine pour définir une seule variable dans un fichier
   subroutine define_single_variable(ncid, tag, var_idx)
      integer, intent(in) :: ncid
      character(len=*), intent(in) :: tag
      integer, intent(in) :: var_idx
      integer :: j, ncerr, dim_id, time_dimid
      integer :: axis_size
      integer, allocatable :: dim_ids(:)

      ! Récupérer l'ID de la dimension temps pour ce fichier
      do j = 1, size(open_files)
         if (open_files(j)%ncid == ncid) then
            time_dimid = open_files(j)%time_dimid
            exit
         end if
      end do

      ! Définir les dimensions de la grille
      do j = 1, registered_vars(var_idx)%var_grid%ndims
         axis_size = registered_vars(var_idx)%var_grid%axes(j)%size

         ! Vérifier si la dimension existe déjà
         ncerr = nf90_inq_dimid(ncid, trim(registered_vars(var_idx)%var_grid%axes(j)%name), dim_id)

         if (ncerr /= nf90_noerr) then
            ! Si non, la créer
            if (registered_vars(var_idx)%var_grid%axes(j)%is_unlimited) then
               ncerr = nf90_def_dim(ncid, trim(registered_vars(var_idx)%var_grid%axes(j)%name), &
                                    nf90_unlimited, dim_id)
            else
               ncerr = nf90_def_dim(ncid, trim(registered_vars(var_idx)%var_grid%axes(j)%name), &
                                    axis_size, dim_id)
            end if
            call check(ncerr)
         end if

         ! Stocker l'ID de la dimension
         registered_vars(var_idx)%var_grid%axes(j)%id = dim_id
      end do

      ! Créer un tableau d'IDs de dimensions pour la définition de variable
      allocate (dim_ids(registered_vars(var_idx)%var_grid%ndims + 1))  ! +1 pour le temps

      ! Copier les IDs des axes existants
      do j = 1, registered_vars(var_idx)%var_grid%ndims
         dim_ids(j) = registered_vars(var_idx)%var_grid%axes(j)%id
      end do

      ! Ajouter l'ID de la dimension temps
      dim_ids(registered_vars(var_idx)%var_grid%ndims + 1) = time_dimid

      ! Définir la variable selon le type de fichier
      select case (tag)
      case ("his")
         ncerr = nf90_def_var(ncid, registered_vars(var_idx)%name, nf90_real, &
                              dim_ids, registered_vars(var_idx)%varid_his)
         call check(ncerr)
         ! Ajouter les attributs
         ncerr = nf90_put_att(ncid, registered_vars(var_idx)%varid_his, "long_name", &
                              trim(registered_vars(var_idx)%long_name))
         call check(ncerr)
         ncerr = nf90_put_att(ncid, registered_vars(var_idx)%varid_his, "units", &
                              trim(registered_vars(var_idx)%units))
         call check(ncerr)
         print *, "Defined HIS variable ", trim(registered_vars(var_idx)%name), " in file with ID ", ncid

      case ("avg")
         ncerr = nf90_def_var(ncid, registered_vars(var_idx)%name, nf90_real, &
                              dim_ids, registered_vars(var_idx)%varid_avg)
         call check(ncerr)
         ! Ajouter les attributs
         ncerr = nf90_put_att(ncid, registered_vars(var_idx)%varid_avg, "long_name", &
                              trim(registered_vars(var_idx)%long_name))
         call check(ncerr)
         ncerr = nf90_put_att(ncid, registered_vars(var_idx)%varid_avg, "units", &
                              trim(registered_vars(var_idx)%units))
         call check(ncerr)
         if (.not. allocated(registered_vars(var_idx)%avg_buffer)) then
            allocate (registered_vars(var_idx)%avg_buffer(size(registered_vars(var_idx)%data, 1),&
            &size(registered_vars(var_idx)%data, 2)))
         end if
         print *, "Defined AVG variable ", trim(registered_vars(var_idx)%name), " in file with ID ", ncid

      case ("rst")
         ncerr = nf90_def_var(ncid, registered_vars(var_idx)%name, nf90_double, &
                              dim_ids, registered_vars(var_idx)%varid_rst)
         call check(ncerr)
         ! Ajouter les attributs
         ncerr = nf90_put_att(ncid, registered_vars(var_idx)%varid_rst, "long_name", &
                              trim(registered_vars(var_idx)%long_name))
         call check(ncerr)
         ncerr = nf90_put_att(ncid, registered_vars(var_idx)%varid_rst, "units", &
                              trim(registered_vars(var_idx)%units))
         call check(ncerr)
         print *, "Defined RST variable ", trim(registered_vars(var_idx)%name), " in file with ID ", ncid
      end select

      ! Libérer la mémoire
      deallocate (dim_ids)
   end subroutine define_single_variable
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

      !
      deallocate (open_files)
   end subroutine close_all_output_files

   ! Fonction pour ajouter une dimension de temps à une grille
   function add_time_dimension_to_grid(grid_in) result(grid_out)
      type(grid), intent(in) :: grid_in
      type(grid) :: grid_out
      type(axis) :: time_axis

      ! Créer l'axe temporel
      time_axis = create_time_axis(.true.)

      ! Cloner la grille existante
      grid_out = grid_in%clone()

      ! Ajouter l'axe temporel
      call grid_out%add_axis(time_axis)
   end function add_time_dimension_to_grid
!>
!>
!> @param current_time
!>
!> @param current_time

   subroutine write_all_outputs(current_time, is_final_step)
      real, intent(in) :: current_time
      logical, intent(in), optional :: is_final_step
      integer :: i
      logical :: final_step

      ! Déterminer si c'est le dernier pas de temps
      final_step = .false.
      if (present(is_final_step)) then
         final_step = is_final_step
      end if

      if (.not. allocated(open_files)) then
         print *, "Warning: No files open for writing"
         return
      end if

      ! Parcourir tous les fichiers ouverts
      do i = 1, size(open_files)
         ! Cas des fichiers de redémarrage
         if (trim(open_files(i)%type) == "rst") then
            ! Écrire à la fréquence définie OU si c'est le dernier pas de temps
            if (mod(current_time, open_files(i)%freq) < 1e-5 .or. final_step) then
               print *, "Writing to restart file: ", trim(open_files(i)%filename), " at time: ", current_time
               call write_output(open_files(i)%ncid, "rst", current_time)
            end if
         else
            ! Fichiers d'historique et de moyenne
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
