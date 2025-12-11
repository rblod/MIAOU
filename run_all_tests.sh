#!/bin/bash
#===============================================================================
# MIAOU I/O Test Suite
# Tests all I/O modes: serial, MPI sequential, MPI parallel files, NC4PAR
#
# Usage:
#   ./run_all_tests.sh          # Run all tests
#   ./run_all_tests.sh serial   # Run only serial test
#   ./run_all_tests.sh mpi      # Run only MPI sequential test
#   ./run_all_tests.sh mpi_pf   # Run only MPI parallel files test
#   ./run_all_tests.sh nc4par   # Run only NC4PAR test
#
# Exit codes:
#   0 = All tests passed
#   1 = Some tests failed
#===============================================================================

set -e

# Colors for output (disabled if not a terminal)
if [ -t 1 ]; then
    RED=$(printf '\033[0;31m')
    GREEN=$(printf '\033[0;32m')
    YELLOW=$(printf '\033[1;33m')
    BLUE=$(printf '\033[0;34m')
    NC=$(printf '\033[0m')
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    NC=''
fi

# Configuration
NP=4  # Number of MPI processes
VERIFY_SCRIPT="verify_output.py"

# Results tracking (Bash 3 compatible - use simple variables)
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
RESULTS_SERIAL=""
RESULTS_MPI=""
RESULTS_MPI_PF=""
RESULTS_NC4PAR=""

#===============================================================================
# Helper functions
#===============================================================================

print_header() {
    printf "\n"
    printf "%s============================================================%s\n" "${BLUE}" "${NC}"
    printf "%s%s%s\n" "${BLUE}" "$1" "${NC}"
    printf "%s============================================================%s\n" "${BLUE}" "${NC}"
}

print_subheader() {
    printf "\n"
    printf "%s--- %s ---%s\n" "${YELLOW}" "$1" "${NC}"
}

print_success() {
    printf "%s✓ %s%s\n" "${GREEN}" "$1" "${NC}"
}

print_failure() {
    printf "%s✗ %s%s\n" "${RED}" "$1" "${NC}"
}

print_info() {
    printf "  %s\n" "$1"
}

cleanup() {
    print_subheader "Cleaning up"
    rm -f *.nc *.o *.mod test_output.exe 2>/dev/null || true
    print_info "Removed temporary files"
}

check_netcdf_parallel() {
    # Check if NetCDF has parallel support
    if /usr/local/bin/nc-config --has-parallel4 2>/dev/null | grep -q "yes"; then
        return 0
    else
        return 1
    fi
}

# Detect mpirun options for root/container environments
get_mpirun_opts() {
    local opts="--oversubscribe"
    # Check if we need --allow-run-as-root (Linux containers)
    if [ "$(id -u)" = "0" ]; then
        opts="$opts --allow-run-as-root"
    fi
    echo "$opts"
}

verify_outputs() {
    local mode=$1
    local expected_files=$2
    
    print_subheader "Verifying outputs for $mode"
    
    # Check expected files exist
    local all_exist=true
    for f in $expected_files; do
        if [ -f "$f" ]; then
            local size=$(ls -lh "$f" 2>/dev/null | awk '{print $5}')
            print_info "Found: $f ($size)"
        else
            print_failure "Missing: $f"
            all_exist=false
        fi
    done
    
    if [ "$all_exist" = "false" ]; then
        return 1
    fi
    
    # Run verification script if available
    if [ -f "$VERIFY_SCRIPT" ]; then
        print_info "Running verification script..."
        python3 "$VERIFY_SCRIPT" 2>&1 | tail -20
        # Check if verification passed
        if python3 "$VERIFY_SCRIPT" 2>&1 | grep -qE "Verification completed successfully|All files.*correct"; then
            return 0
        else
            return 1
        fi
    else
        print_info "Verification script not found, skipping value checks"
        return 0
    fi
}

run_test() {
    local mode=$1
    local build_target=$2
    local run_cmd=$3
    local expected_files=$4
    local description=$5
    local build_status run_status
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    print_header "TEST: $description"
    
    # Clean before test
    cleanup
    
    # Build - capture exit code properly
    print_subheader "Building ($build_target)"
    make $build_target 2>&1 | tail -10
    build_status=${PIPESTATUS[0]}
    if [ $build_status -eq 0 ]; then
        print_success "Build successful"
    else
        print_failure "Build failed (exit code: $build_status)"
        eval "RESULTS_${mode}=\"FAILED (build)\""
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    
    # Run - capture exit code properly
    print_subheader "Running ($run_cmd)"
    eval $run_cmd 2>&1 | tail -20
    run_status=${PIPESTATUS[0]}
    if [ $run_status -eq 0 ]; then
        print_success "Execution completed"
    else
        print_failure "Execution failed (exit code: $run_status)"
        eval "RESULTS_${mode}=\"FAILED (run)\""
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    
    # List output files
    print_subheader "Output files"
    ls -la *.nc 2>/dev/null || echo "No .nc files found"
    
    # Verify
    if verify_outputs "$mode" "$expected_files"; then
        print_success "Verification passed"
        eval "RESULTS_${mode}=\"PASSED\""
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        print_failure "Verification failed"
        eval "RESULTS_${mode}=\"FAILED (verify)\""
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

#===============================================================================
# Test functions
#===============================================================================

test_serial() {
    run_test "SERIAL" "serial" "./test_output.exe" \
        "ocean_hourly_3600s.nc ocean_6hourly_21600s.nc ocean_daily_avg_86400s.nc" \
        "Serial Mode"
}

test_mpi_sequential() {
    local mpi_opts=$(get_mpirun_opts)
    run_test "MPI" "mpi" "mpirun $mpi_opts -np $NP ./test_output.exe" \
        "ocean_hourly_3600s.nc ocean_6hourly_21600s.nc ocean_daily_avg_86400s.nc" \
        "MPI Sequential I/O (token passing, single file)"
}

test_mpi_parallel_files() {
    local expected=""
    local i=0
    while [ $i -lt $NP ]; do
        suffix=$(printf "%04d" $i)
        expected="$expected ocean_hourly_3600s_${suffix}.nc"
        i=$((i + 1))
    done
    
    local mpi_opts=$(get_mpirun_opts)
    run_test "MPI_PF" "mpi_pf" "mpirun $mpi_opts -np $NP ./test_output.exe" \
        "$expected" \
        "MPI Parallel Files (one file per process)"
}

test_nc4par() {
    # Check if NetCDF has parallel support
    if ! check_netcdf_parallel; then
        print_header "TEST: NC4PAR (Parallel NetCDF-4)"
        print_info "NetCDF does not have parallel HDF5 support"
        print_info "Skipping NC4PAR test"
        RESULTS_NC4PAR="SKIPPED (no parallel support)"
        return 0
    fi
    
    local mpi_opts=$(get_mpirun_opts)
    run_test "NC4PAR" "nc4par" "mpirun $mpi_opts -np $NP ./test_output.exe" \
        "ocean_hourly_3600s.nc ocean_6hourly_21600s.nc ocean_daily_avg_86400s.nc" \
        "NC4PAR (Parallel NetCDF-4, single shared file)"
}

print_result() {
    local mode=$1
    local result=$2
    
    if [ -z "$result" ]; then
        return
    fi
    
    case "$result" in
        PASSED)
            print_success "$mode: $result"
            ;;
        SKIPPED*)
            printf "%s○ %s: %s%s\n" "${YELLOW}" "$mode" "$result" "${NC}"
            ;;
        *)
            print_failure "$mode: $result"
            ;;
    esac
}

#===============================================================================
# Main
#===============================================================================

print_header "MIAOU I/O Test Suite"
echo ""
echo "Testing configuration:"
echo "  - MPI processes: $NP"
echo "  - Verify script: $VERIFY_SCRIPT"
echo "  - NetCDF parallel: $(check_netcdf_parallel && echo 'yes' || echo 'no')"

# Determine which tests to run
if [ $# -eq 0 ]; then
    # Run all tests
    TESTS="serial mpi mpi_pf nc4par"
else
    TESTS="$@"
fi

# Run selected tests
for test in $TESTS; do
    case $test in
        serial)
            test_serial || true
            ;;
        mpi)
            test_mpi_sequential || true
            ;;
        mpi_pf)
            test_mpi_parallel_files || true
            ;;
        nc4par)
            test_nc4par || true
            ;;
        *)
            echo "Unknown test: $test"
            echo "Valid tests: serial, mpi, mpi_pf, nc4par"
            ;;
    esac
done

# Summary
print_header "TEST SUMMARY"
printf "\n"
print_result "serial" "$RESULTS_SERIAL"
print_result "mpi" "$RESULTS_MPI"
print_result "mpi_pf" "$RESULTS_MPI_PF"
print_result "nc4par" "$RESULTS_NC4PAR"

printf "\n"
printf "Total: %d tests\n" "$TOTAL_TESTS"
printf "  %sPassed: %d%s\n" "${GREEN}" "$PASSED_TESTS" "${NC}"
printf "  %sFailed: %d%s\n" "${RED}" "$FAILED_TESTS" "${NC}"

# Cleanup
cleanup

# Exit with appropriate code
if [ $FAILED_TESTS -gt 0 ]; then
    exit 1
else
    exit 0
fi
