# MIAOU - Changelog

## Version 0.5.0 - 2025-04

### Summary

Introduction of the `output_stream` type to unify and simplify output configuration.

---

### 7. Introduced output_stream type

**Problem:**  
`io_variable` had repetitive fields for each output type:
```fortran
logical :: to_his, to_avg, to_rst
real :: freq_his, freq_avg, freq_rst
character(len=128) :: file_prefix  ! Shared across all types
```

This led to repetitive `select case` blocks throughout the codebase and made
it difficult to add new output types or per-stream prefixes.

**Solution:**  
New `output_stream` type encapsulating stream configuration:

```fortran
type :: output_stream
   character(len=16) :: stream_type   ! "his", "avg", "rst"
   logical :: enabled                 ! Write to this stream?
   real :: frequency                  ! Output frequency (seconds)
   character(len=128) :: prefix       ! File prefix (empty = global)
contains
   procedure :: should_write          ! Check if write at given time
   procedure :: is_averaging          ! True if stream_type == "avg"
   procedure :: is_active             ! True if enabled with valid frequency
end type
```

`io_variable` now uses an array of streams:
```fortran
type :: io_variable
   ! ... metadata and data pointers ...
   type(output_stream) :: streams(3)  ! Indexed by STREAM_HIS, STREAM_AVG, STREAM_RST
   ! ... averaging buffers ...
contains
   procedure :: has_output            ! Any stream active?
   procedure :: needs_averaging       ! Is STREAM_AVG active?
   procedure :: get_prefix            ! Get prefix for a stream
end type
```

**Benefits:**

| Aspect | Before | After |
|--------|--------|-------|
| Fields per variable | 7 (to_his, freq_his, to_avg, freq_avg, to_rst, freq_rst, file_prefix) | 1 array of 3 streams |
| Iteration over types | `select case` with string matching | Simple `do` loop over streams array |
| Per-stream prefix | Not possible | Each stream has its own prefix |
| Adding new type | Modify io_variable + all select cases | Add constant + enlarge array |
| Time check logic | Duplicated in multiple modules | Encapsulated in `stream%should_write()` |

**New constants:**
```fortran
integer, parameter :: STREAM_HIS = 1
integer, parameter :: STREAM_AVG = 2
integer, parameter :: STREAM_RST = 3
```

**Files modified:**
- `io_definitions.F90` — New `output_stream` type, updated `io_variable`
- `io_config.F90` — Updated to populate `streams` array
- `io_manager.F90` — Refactored to iterate over streams
- `io_averaging.F90` — Uses `needs_averaging()` method

---

## Version 0.4.0 - 2025-04

### Summary

Decoupling of the averaging module to separate generic logic from backend-specific implementation.

---

### 6. Decoupled averaging module

**Problem:**  
`io_netcdf_avg.F90` mixed two responsibilities:
- Generic averaging logic (buffer management, accumulation, average computation)
- NetCDF-specific writing

This made it impossible to reuse the averaging logic with other backends.

**Solution:**  
Split into two modules:

| Module | Responsibility |
|--------|----------------|
| `io_averaging.F90` | Generic averaging: buffer init, accumulation, reset, computation |
| `io_netcdf_avg.F90` | NetCDF writing only, delegates averaging to `io_averaging` |

**New file: `io_averaging.F90`**

Public interface:
- `avg_init_buffers(var_ptr)` — Initialize averaging buffers
- `avg_accumulate(var_ptr)` — Add current values to accumulator
- `avg_reset(var_ptr)` — Reset buffers after writing
- `avg_get_count(var_ptr)` — Get number of accumulated values
- `avg_is_ready(var_ptr)` — Check if average is ready to write
- `avg_compute_scalar/1d/2d/3d(var_ptr, avg_data)` — Compute and return average

**Modified file: `io_netcdf_avg.F90`**

Now only contains:
- `accumulate_avg()` — Wrapper delegating to `avg_accumulate()`
- `reset_avg()` — Wrapper delegating to `avg_reset()`
- `write_variable_avg()` — Computes average via `io_averaging`, writes to NetCDF

**Benefits:**
- Averaging logic can be reused by HDF5, Zarr, or other backends
- Easier unit testing of averaging logic
- Clear separation of concerns

---

## Version 0.3.0 - 2025-04

### Summary

Improved modularity with centralized constants and file naming extraction.

---

### 4. Constantes centralisées

**Problème :**  
Les constantes (`TOLERANCE = 1.0e-5`, tailles de chaînes, etc.) étaient définies dans plusieurs fichiers, créant des risques d'incohérence.

**Solution :**  
Nouveau module `io_constants.F90` regroupant toutes les constantes :

| Catégorie | Constantes |
|-----------|------------|
| Tolérances | `IO_TIME_TOLERANCE` |
| Longueurs de chaînes | `IO_VARNAME_LEN`, `IO_LONGNAME_LEN`, `IO_UNITS_LEN`, `IO_PREFIX_LEN`, `IO_PATH_LEN`, `IO_FILETYPE_LEN` |
| Limites de tableaux | `IO_MAX_VARS`, `IO_MAX_FILES`, `IO_MAX_DIMS`, `IO_INITIAL_ALLOC`, `IO_GROWTH_FACTOR` |
| Types de fichiers | `IO_TYPE_HIS`, `IO_TYPE_AVG`, `IO_TYPE_RST`, `IO_FILE_TYPES`, `IO_NUM_FILE_TYPES` |
| Valeurs par défaut | `IO_FREQ_DISABLED`, `IO_DEFAULT_PREFIX` |

**Fichiers modifiés :**
- `io_definitions.F90` : Utilise `IO_*_LEN`, `IO_FREQ_DISABLED`, `IO_INITIAL_ALLOC`, `IO_GROWTH_FACTOR`
- `io_config.F90` : Utilise `IO_VARNAME_LEN`, `IO_PREFIX_LEN`, `IO_FREQ_DISABLED`, `IO_DEFAULT_PREFIX`
- `io_manager.F90` : Utilise `IO_TIME_TOLERANCE`, `IO_FILE_TYPES`, `IO_NUM_FILE_TYPES`, `IO_INITIAL_ALLOC`

---

### 5. Extraction du nommage des fichiers

**Problème :**  
`generate_filename()` était dans `io_netcdf.F90` mais la convention de nommage est indépendante de NetCDF.

**Solution :**  
Nouveau module `io_naming.F90` contenant :

| Fonction | Description |
|----------|-------------|
| `generate_filename(prefix, type, freq, ext)` | Génère un nom de fichier selon la convention `{prefix}_{type}_{freq}s.{ext}` |
| `parse_filename(...)` | Parse un nom de fichier pour extraire ses composants |
| `set_default_extension(ext)` | Définit l'extension par défaut (appelé par le backend) |
| `get_default_extension()` | Récupère l'extension par défaut |

**Avantages :**
- Le backend ne fournit que l'extension (`.nc`, `.h5`, `.zarr`)
- La convention de nommage est testable indépendamment
- Facilite l'ajout de conventions alternatives

**Fichiers modifiés :**
- `io_netcdf.F90` : Suppression de `generate_filename`, suppression de l'export public
- `io_manager.F90` : Import de `generate_filename` depuis `io_naming`

---

## Version 0.2.0 (Refactored) - 2025-04

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
