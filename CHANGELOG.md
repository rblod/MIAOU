# MIAOU - Changelog

## Version 3.2.0 (Refactored) - 2025-04

### Résumé

Refactorisation majeure pour corriger trois problèmes d'architecture identifiés avant l'ajout de nouveaux backends.

---

### 1. Suppression de la duplication des registres de fichiers

**Problème :**  
Deux listes `open_files` existaient en parallèle :
- `io_manager.F90` : `type(file_descriptor), allocatable :: open_files(:)`
- `io_netcdf.F90` : `type(netcdf_file), allocatable :: open_files(:)`

Les métadonnées NetCDF (`time_dimid`, `time_varid`, `time_index`) étaient stockées uniquement dans `io_netcdf`, nécessitant des recherches linéaires via `get_time_index_from_ncid()` pour y accéder.

**Solution :**  
- `file_descriptor` enrichi avec tous les champs nécessaires
- Une seule liste dans `io_manager`
- `io_netcdf` travaille avec les `file_descriptor` passés en paramètre

**Fichiers modifiés :**

| Fichier | Modifications |
|---------|---------------|
| `io_definitions.F90` | Ajout de `time_dimid`, `time_varid` à `file_descriptor`. Nouvelles méthodes `increment_time()`, `is_open()` |
| `io_netcdf.F90` | Suppression de `type netcdf_file` et de la liste `open_files`. Toutes les fonctions utilisent `file_descriptor` directement |
| `io_manager.F90` | Gestion centralisée avec `add_to_open_files()`, `get_file_ptr()`, `num_files` |

---

### 2. Utilisation de pointeurs au lieu de copies

**Problème :**  
`var_registry%get(idx)` retournait une copie :
```fortran
var = var_registry%get(var_idx)           ! Copie !
if (var%to_avg) call accumulate_avg(var)  ! Modifie la copie
! Modifications de avg_count perdues !
```

Conséquence : `avg_count` restait à 0, les moyennes étaient incorrectes.

**Solution :**  
Utilisation de pointeurs pour modifier directement les variables dans le registry :
```fortran
var_ptr => var_registry%get_ptr(var_idx)  ! Pointeur !
if (var_ptr%to_avg) call accumulate_avg(var_ptr)  ! Modifie l'original
```

**Fichiers modifiés :**

| Fichier | Modifications |
|---------|---------------|
| `io_definitions.F90` | Ajout de `get_ptr()` et `update()` à `io_var_registry` |
| `io_manager.F90` | Remplacement de `var = get()` par `var_ptr => get_ptr()` dans `write_all_data()`, `create_output_files()`, etc. |
| `io_netcdf_avg.F90` | Signatures changées : `type(io_variable), pointer, intent(inout) :: var_ptr` pour `accumulate_avg()`, `write_variable_avg()`, `reset_avg()` |

---

### 3. Gestion des erreurs centralisée

**Problème :**  
`nc_check` faisait un `print` et continuait silencieusement :
```fortran
if (status /= nf90_noerr) then
   print *, "NetCDF Error: ", trim(nf90_strerror(status))
   ! Continue sans propager l'erreur...
end if
```

Les erreurs n'étaient pas propagées, menant à des états incohérents.

**Solution :**  
Nouveau module `io_error` avec :
- Codes d'erreur standardisés (`IO_SUCCESS`, `IO_ERR_FILE`, `IO_ERR_VAR`, etc.)
- Mode "strict" optionnel (arrêt en cas d'erreur)
- Mode "verbose" contrôlable
- Historique de la dernière erreur

**Nouveaux fichiers :**

| Fichier | Description |
|---------|-------------|
| `io_error.F90` | Module de gestion d'erreurs centralisé |

**Fichiers modifiés :**

| Fichier | Modifications |
|---------|---------------|
| `netcdf_utils.F90` | Intégration avec `io_error`. Nouvelle fonction `nc_check_status()` retournant un booléen. Mapping des erreurs NetCDF vers codes I/O |
| `io_manager.F90` | Ajout de `get_io_status()`, `set_error()`, `clear_error()` |

---

### Autres modifications

**Makefile :**
- Ajout de `io_error.F90` au niveau 0 (sans dépendances)
- Mise à jour des dépendances de `netcdf_utils.F90`

**Corrections mineures :**
- `io_netcdf.F90` : `nf90_close()` et `nf90_put_att()` utilisés comme fonctions (pas `call`)

---

### Migration

Le code utilisateur (`main_test_output.F90`, `var_registry.F90`, `var_definitions.F90`) n'est pas impacté. L'API publique reste identique :

```fortran
status = initialize_io("config.nml", time_units, calendar)
call init_variables(nx, ny, nz)
status = write_all_data(current_time)
status = finalize_io()
```

---

### Prochaines étapes

Cette refactorisation prépare l'ajout de nouveaux backends (HDF5, Zarr, etc.) en :
- Centralisant la gestion des fichiers dans `io_manager`
- Définissant une interface claire via `file_descriptor`
- Standardisant la gestion des erreurs

Une interface abstraite `io_backend` pourra être ajoutée dans une version future.
