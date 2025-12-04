# MIAOU v5.0.0 - Architecture

## Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              APPLICATION (CROCO)                            │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   ocean_var.F90              output_vars.inc              output_config.nml │
│   ┌──────────────┐           ┌──────────────┐             ┌──────────────┐  │
│   │ real :: zeta │           │ OUTVAR(name, │             │ &io_files_nml│  │
│   │ real :: u    │           │  long, units,│             │ file_name()  │  │
│   │ real :: v    │           │  grid, ndims,│             │ file_freq()  │  │
│   │ real :: temp │           │  var,        │             │ file_vars()  │  │
│   │ (no target)  │           │  is_restart) │             │ groups()     │  │
│   └──────────────┘           └──────────────┘             └──────────────┘  │
│                                     │                            │          │
└─────────────────────────────────────┼────────────────────────────┼──────────┘
                                      │                            │
                                      ▼                            ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            MIAOU I/O SYSTEM                                 │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  var_definitions.F90                           io_config.F90                │
│  ┌────────────────────────────────┐           ┌────────────────────────┐    │
│  │ init_var_definitions()        │           │ load_io_config()       │    │
│  │   - Parse output_vars.inc     │           │   - Parse namelist     │    │
│  │   - Register restart flags    │           │   - Expand @groups     │    │
│  │                               │           │   - Configure files    │    │
│  │ send_all_outputs(time)        │           └────────────────────────┘    │
│  │   - Expand OUTVAR macros      │                      │                  │
│  │   - Call send_var() for each  │                      │                  │
│  └────────────────────────────────┘                      │                  │
│                 │                                        │                  │
│                 ▼                                        ▼                  │
│  ┌──────────────────────────────────────────────────────────────────────┐   │
│  │                         io_manager.F90                               │   │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │   │
│  │  │                      send_var(name, data, time)                 │ │   │
│  │  │                               │                                 │ │   │
│  │  │     ┌─────────────────────────┼─────────────────────────┐       │ │   │
│  │  │     │                         │                         │       │ │   │
│  │  │     ▼                         ▼                         ▼       │ │   │
│  │  │ ┌────────────┐         ┌────────────┐         ┌────────────┐    │ │   │
│  │  │ │  INSTANT   │         │  AVERAGE   │         │  RESTART   │    │ │   │
│  │  │ │            │         │            │         │            │    │ │   │
│  │  │ │ Direct     │         │ Accumulate │         │ Store in   │    │ │   │
│  │  │ │ write to   │         │ in         │         │ var_data   │    │ │   │
│  │  │ │ NetCDF     │         │ avg_state  │         │ buffer     │    │ │   │
│  │  │ │            │         │            │         │            │    │ │   │
│  │  │ │ ZERO COPY  │         │ 1 buffer   │         │ if flagged │    │ │   │
│  │  │ └────────────┘         └────────────┘         └────────────┘    │ │   │
│  │  └─────────────────────────────────────────────────────────────────┘ │   │
│  │                                                                      │   │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │   │
│  │  │                    write_output(time)                           │ │   │
│  │  │                            │                                    │ │   │
│  │  │     ┌──────────────────────┼──────────────────────┐             │ │   │
│  │  │     ▼                      ▼                      ▼             │ │   │
│  │  │ ┌────────────┐      ┌────────────┐      ┌────────────────┐      │ │   │
│  │  │ │  INSTANT   │      │  AVERAGE   │      │    RESTART     │      │ │   │
│  │  │ │            │      │            │      │                │      │ │   │
│  │  │ │ Write TIME │      │ Compute    │      │ Incremental    │      │ │   │
│  │  │ │ only       │      │ average    │      │ write with     │      │ │   │
│  │  │ │ (data      │      │ Write      │      │ rotation       │      │ │   │
│  │  │ │ already    │      │ Reset      │      │ after N levels │      │ │   │
│  │  │ │ written)   │      │            │      │                │      │ │   │
│  │  │ └────────────┘      └────────────┘      └────────────────┘      │ │   │
│  │  └─────────────────────────────────────────────────────────────────┘ │   │
│  └──────────────────────────────────────────────────────────────────────┘   │
│                                      │                                      │
└──────────────────────────────────────┼──────────────────────────────────────┘
                                       │
                                       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              BACKEND LAYER                                  │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  io_netcdf.F90                                     (Future: io_xios.F90)    │
│  ┌─────────────────────────────────────┐           ┌─────────────────────┐  │
│  │ nc_write_direct_2d(ncid, data)      │           │ xios_send_field()   │  │
│  │ nc_write_direct_3d(ncid, data)      │           │ xios_context_init() │  │
│  │ nc_write_avg_data(ncid, state)      │           │ ...                 │  │
│  │ nc_create_file()                    │           └─────────────────────┘  │
│  │ nc_define_variable()                │                                    │
│  └─────────────────────────────────────┘                                    │
│                    │                                                        │
│                    ▼                                                        │
│  ┌─────────────────────────────────────┐                                    │
│  │           NetCDF Library            │                                    │
│  │      (netcdf-fortran / HDF5)        │                                    │
│  └─────────────────────────────────────┘                                    │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Data Flow by Output Type

### INSTANT Files (Zero Copy)

```
send_var("zeta", zeta, time)
         │
         ▼
    is_output_time?
         │
    ┌────┴────┐
    │ YES     │ NO
    ▼         ▼
nc_write_   (skip)
direct_2d()
    │
    ▼
  NetCDF
   File
```

**Memory cost: ZERO** - Data flows directly from model array to NetCDF.

### AVERAGE Files (One Buffer)

```
send_var("zeta", zeta, time)
         │
         ▼
  avg_state%accumulate()
         │
         ▼
    ┌─────────┐
    │ Buffer  │  ← One copy per averaged variable
    │ (sum)   │
    └─────────┘
         │
         ▼ (at output time)
  avg_state%compute()
         │
         ▼
  nc_write_avg_data()
         │
         ▼
     NetCDF File
```

**Memory cost: 1× per averaged variable**

### RESTART Files (Selective Buffer + Rotation)

```
send_var("zeta", zeta, time)
         │
         ▼
   is_restart_var?  ←── Defined in output_vars.inc
         │
    ┌────┴────┐
    │ YES     │ NO
    ▼         ▼
var_data%   (skip)
  set()
    │
    ▼
┌─────────┐
│ Buffer  │  ← Only for restart variables
└─────────┘
    │
    ▼ (at checkpoint time)
write_restart_record()
    │
    ▼
┌─────────────────────────┐
│   Restart File          │
│  ┌─────┐ ┌─────┐        │
│  │ t-1 │ │ t   │        │  ← N levels with rotation
│  └─────┘ └─────┘        │
└─────────────────────────┘
```

**Memory cost: 1× per restart variable only**

## Memory Comparison

### Typical Configuration (1000×800×50 grid, 10 3D variables)

| Scenario | v4.0.0 (all buffered) | v5.0.0 (zero copy) | Savings |
|----------|----------------------|-------------------|---------|
| 10 instant vars | 1.6 GB | **0 GB** | 100% |
| 5 instant + 5 avg | 1.6 GB | 0.8 GB | 50% |
| 5 instant + 5 avg + 4 restart | 1.6 GB | 1.4 GB | 12% |
| All averaged | 1.6 GB | 1.6 GB | 0% |

## File Structure

```
MIAOU-5.0.0/
├── docs/
│   ├── ARCHITECTURE.md      # This file
│   ├── CHANGELOG.md         # Version history
│   └── RELEASE_NOTES.md     # v5.0.0 release notes
│
├── src/
│   ├── io_constants.F90     # Constants and parameters
│   ├── io_error.F90         # Error handling
│   ├── io_definitions.F90   # Type definitions
│   ├── io_file_registry.F90 # File and avg_state management
│   ├── io_config.F90        # Namelist parsing
│   ├── io_manager.F90       # Main API (send_var, write_output)
│   ├── io_netcdf.F90        # NetCDF backend
│   ├── io_naming.F90        # Filename generation
│   ├── netcdf_utils.F90     # NetCDF utilities
│   ├── netcdf_backend.F90   # Low-level NetCDF operations
│   ├── grid_module.F90      # Grid definitions
│   ├── var_definitions.F90  # Variable registration
│   └── var_registry.F90     # Variable lookup
│
├── config/
│   └── output_config.nml    # Example configuration
│
└── include/
    └── output_vars.inc      # Variable definitions (user-edited)
```

## Integration with Model

### Minimal Integration (3 calls)

```fortran
program ocean_model
   use var_definitions, only: init_var_definitions, send_all_outputs
   use io_manager, only: initialize_io, finalize_io, write_output, &
                         ensure_files_created
   
   ! Initialize
   call initialize_io("output_config.nml")
   call init_var_definitions()
   call ensure_files_created()
   
   ! Time loop
   do while (running)
      call compute_physics()
      
      call send_all_outputs(current_time)  ! Send data
      call write_output(current_time)       ! Write files
   end do
   
   ! Finalize
   call finalize_io()
end program
```

### Adding a New Variable

Edit **only** `output_vars.inc`:

```fortran
! Syntax: OUTVAR(name, long_name, units, grid, ndims, variable, is_restart)

OUTVAR("salinity", "sea water salinity", "PSU", grd_rho3d, 3, salt, .true.)
```

Then add the variable to the appropriate file in the namelist.

## Future: XIOS Backend

The architecture is designed for easy XIOS integration:

```fortran
subroutine send_var_2d(name, val, current_time)
   ...
#ifdef USE_XIOS
   call xios_send_field(name, val)
#else
   call process_var_instant(name, current_time, d2=val)
   call process_var_average(name, d2=val)
#endif
end subroutine
```

XIOS would handle:
- Timing decisions
- Averaging operations  
- Parallel I/O
- Server-side processing

The `output_vars.inc` file could generate `iodef.xml` automatically.
