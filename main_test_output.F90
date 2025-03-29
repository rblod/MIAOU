!===============================================================================
! main_test_output.F90 : Minimal test program for file_manager and variables_registry
!===============================================================================
program main_test_output
   use file_manager
   use variables_registry
   use netcdf
   use ocean_var
   implicit none

   integer, parameter :: nx = 10, ny = 8, nz = 5, nt = 10
   integer :: ncid_his, ncid_avg, ncid_rst, t
   real :: current_time
   character(len=128) :: time_units

   ! Allocation des champs de test
   allocate (zeta(nx, ny))
   allocate (temp(nx, ny, nz))
   allocate (u(nx, ny))
   allocate (v(nx, ny))
   zeta = 0.; temp = 0.
   u = 0.; v = 0.

   call init_variables(nx, ny, nz)

   ! Définition des unités de temps
   time_units = "seconds since 2023-01-01 00:00:00"

   ! Création des fichiers
   call check(nf90_create("history.nc", nf90_clobber, ncid_his))
   call define_output_file(ncid_his, "his", time_units, "gregorian")
   call check(nf90_enddef(ncid_his))

   call check(nf90_create("average.nc", nf90_clobber, ncid_avg))
   call define_output_file(ncid_avg, "avg", time_units, "gregorian")
   call check(nf90_enddef(ncid_avg))

   call check(nf90_create("restart.nc", nf90_clobber, ncid_rst))
   call define_output_file(ncid_rst, "rst")
   call check(nf90_enddef(ncid_rst))

   ! Boucle de temps fictive
   do t = 1, nt
      ! Calcul du temps (par exemple, un pas de temps de 1800 secondes)
      current_time = (t - 1)*1800.0

      ! Mise à jour des données
      zeta = zeta + 0.1*t
      temp(:, :, 1) = temp(:, :, 1) + 0.2*t
      u = t

      call write_output(ncid_his, "his", current_time)
      call write_output(ncid_avg, "avg", current_time)
   end do

   ! Écriture du fichier de restart (instantané final
   call write_output(ncid_rst, "rst", current_time)

   ! Fermeture
   call check(nf90_close(ncid_his))
   call check(nf90_close(ncid_avg))
   call check(nf90_close(ncid_rst))

   ! Libération de la mémoire
   deallocate (zeta, temp, u, v)

end program main_test_output
