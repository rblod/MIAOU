!===============================================================================
! main_test_output.F90 : Minimal test program for file_manager and variables_registry
!===============================================================================
program main_test_output
   use file_manager
   use variables_registry
   use ocean_var
   implicit none

   integer, parameter :: nx = 10, ny = 8, nz = 5, nt = 10
   integer :: t
   real :: current_time
   character(len=128) :: time_units, calendar

   ! Allocation des champs de test
   allocate(zeta(nx, ny))
   allocate(temp(nx, ny, nz))
   allocate(u(nx, ny))
   allocate(v(nx, ny))
   
   ! Initialisation des données
   zeta = 0.; temp = 0.
   u = 0.; v = 0.

   ! Initialisation des variables du modèle
   call init_variables(nx, ny, nz)

   ! Définition des unités de temps
   time_units = "seconds since 2023-01-01 00:00:00"
   calendar = "gregorian"

   ! Initialisation automatique de tous les fichiers de sortie
   call initialize_output_files(time_units, calendar)

   ! Boucle de temps fictive
   do t = 1, nt
      ! Calcul du temps (par exemple, un pas de temps de 1800 secondes)
      current_time = (t - 1)*1800.0

      ! Mise à jour des données (simulation simplifiée)
      zeta = zeta + 0.1*t
      temp(:, :, 1) = temp(:, :, 1) + 0.2*t
      u = t
      v = t * 0.5

      ! Écriture de toutes les sorties selon leurs fréquences configurées
      call write_all_outputs(current_time)
   end do

   ! Fermeture de tous les fichiers
   call close_all_output_files()

   ! Libération de la mémoire
   deallocate(zeta, temp, u, v)

   print *, "Simulation terminée avec succès!"

end program main_test_output