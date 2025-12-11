#!/bin/bash
#===============================================================================
# MIAOU I/O Test Suite
# Tests all I/O modes: serial, MPI sequential, MPI parallel files, NC4PAR
# Also includes configuration validation tests
#
# Usage:
#   ./run_all_tests.sh                    # Run all tests
#   ./run_all_tests.sh serial             # Run only serial test
#   ./run_all_tests.sh mpi                # Run only MPI sequential test
#   ./run_all_tests.sh mpi_pf             # Run only MPI parallel files test
#   ./run_all_tests.sh nc4par             # Run only NC4PAR test
#   ./run_all_tests.sh validation         # Run config validation test
#   ./run_all_tests.sh validation_strict  # Run strict mode validation test
#   ./run_all_tests.sh all_io             # Run all I/O tests (skip validation)
#   ./run_all_tests.sh serial mpi         # Run multiple specific tests
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
JUNIT_OUTPUT=""  # Set to filename to generate JUnit XML

# Results tracking (Bash 3 compatible - use simple variables)
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
RESULTS_SERIAL=""
RESULTS_MPI=""
RESULTS_MPI_PF=""
RESULTS_NC4PAR=""
RESULTS_VALIDATION=""
RESULTS_VALIDATION_STRICT=""

# JUnit XML storage
JUNIT_TESTCASES=""
JUNIT_START_TIME=$(date +%s)

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
    rm -f *.nc test_output.exe 2>/dev/null || true
    # Don't remove .o and .mod - let individual tests handle their own clean
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

#===============================================================================
# Validation tests
#===============================================================================

test_validation() {
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    print_header "TEST: Configuration Validation (detect errors)"
    
    # Build first
    print_subheader "Building serial"
    make clean >/dev/null 2>&1
    make serial 2>&1 | tail -5
    if [ ${PIPESTATUS[0]} -ne 0 ]; then
        print_failure "Build failed"
        RESULTS_VALIDATION="FAILED (build)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    
    # Save original config
    cp output_config.nml output_config.nml.backup
    
    # Use test config with errors
    cp test_validation.nml output_config.nml
    
    print_subheader "Running with invalid config (should show errors but continue)"
    rm -f *.nc
    output=$(./test_output.exe 2>&1)
    run_status=$?
    
    # Restore config
    mv output_config.nml.backup output_config.nml
    
    # Check that errors were detected
    if echo "$output" | grep -q "ERROR.*VAR_NOTFOUND.*temperatue"; then
        print_success "Typo 'temperatue' correctly detected"
    else
        print_failure "Typo detection failed"
        RESULTS_VALIDATION="FAILED (typo detection)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    
    if echo "$output" | grep -q "WARNING.*empty_file.*no variables"; then
        print_success "Empty file warning correctly generated"
    else
        print_failure "Empty file warning missing"
        RESULTS_VALIDATION="FAILED (empty file warning)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    
    # In non-strict mode, should still complete
    if [ $run_status -eq 0 ]; then
        print_success "Non-strict mode: continued despite errors"
    else
        print_failure "Non-strict mode: should not have failed"
        RESULTS_VALIDATION="FAILED (non-strict exit)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    
    print_success "Validation test passed"
    RESULTS_VALIDATION="PASSED"
    PASSED_TESTS=$((PASSED_TESTS + 1))
    return 0
}

test_validation_strict() {
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    print_header "TEST: Strict Mode Validation (fail on errors)"
    
    # Build first
    print_subheader "Building serial"
    make clean >/dev/null 2>&1
    make serial 2>&1 | tail -3
    if [ ${PIPESTATUS[0]} -ne 0 ]; then
        print_failure "Build failed"
        RESULTS_VALIDATION_STRICT="FAILED (build)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
    
    # Save original config
    cp output_config.nml output_config.nml.backup
    
    # Use strict test config
    cp test_strict.nml output_config.nml
    
    print_subheader "Running with strict mode (should fail on error)"
    rm -f *.nc
    ./test_output.exe 2>&1 | tail -10
    run_status=${PIPESTATUS[0]}
    
    # Restore config
    mv output_config.nml.backup output_config.nml
    
    # In strict mode with missing var, should fail
    if [ $run_status -ne 0 ]; then
        print_success "Strict mode: correctly failed on invalid config"
        RESULTS_VALIDATION_STRICT="PASSED"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        print_failure "Strict mode: should have failed but didn't"
        RESULTS_VALIDATION_STRICT="FAILED (should fail)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
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

# Parse options and tests
CI_MODE=false
TESTS=""

while [ $# -gt 0 ]; do
    case "$1" in
        --junit=*)
            JUNIT_OUTPUT="${1#--junit=}"
            ;;
        --junit)
            if [ -n "$2" ] && [ "${2:0:1}" != "-" ]; then
                JUNIT_OUTPUT="$2"
                shift
            else
                echo "Error: --junit requires a filename"
                exit 1
            fi
            ;;
        --ci)
            CI_MODE=true
            # Disable colors in CI mode
            RED=''
            GREEN=''
            YELLOW=''
            BLUE=''
            NC=''
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS] [TESTS...]"
            echo ""
            echo "Options:"
            echo "  --junit=FILE  Generate JUnit XML report"
            echo "  --ci          CI mode (no colors, minimal output)"
            echo "  --help        Show this help"
            echo ""
            echo "Tests: serial, mpi, mpi_pf, nc4par, validation, validation_strict, all_io"
            echo ""
            echo "If no tests specified, all tests are run."
            exit 0
            ;;
        -*)
            echo "Unknown option: $1"
            exit 1
            ;;
        *)
            TESTS="$TESTS $1"
            ;;
    esac
    shift
done

# Default to all tests if none specified
if [ -z "$TESTS" ]; then
    TESTS="serial mpi mpi_pf nc4par validation validation_strict"
fi

print_header "MIAOU I/O Test Suite v5.5.0"
echo ""
echo "Testing configuration:"
echo "  - MPI processes: $NP"
echo "  - Verify script: $VERIFY_SCRIPT"
echo "  - NetCDF parallel: $(check_netcdf_parallel && echo 'yes' || echo 'no')"
if [ -n "$JUNIT_OUTPUT" ]; then
    echo "  - JUnit output: $JUNIT_OUTPUT"
fi
if [ "$CI_MODE" = true ]; then
    echo "  - CI mode: enabled"
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
        validation)
            test_validation || true
            ;;
        validation_strict)
            test_validation_strict || true
            ;;
        all_io)
            # Just I/O mode tests without validation
            test_serial || true
            test_mpi_sequential || true
            test_mpi_parallel_files || true
            test_nc4par || true
            ;;
        *)
            echo "Unknown test: $test"
            echo "Valid tests: serial, mpi, mpi_pf, nc4par, validation, validation_strict, all_io"
            ;;
    esac
done

# Summary
print_header "TEST SUMMARY"
printf "\n"
printf "I/O Mode Tests:\n"
print_result "  serial" "$RESULTS_SERIAL"
print_result "  mpi" "$RESULTS_MPI"
print_result "  mpi_pf" "$RESULTS_MPI_PF"
print_result "  nc4par" "$RESULTS_NC4PAR"
printf "\nValidation Tests:\n"
print_result "  validation" "$RESULTS_VALIDATION"
print_result "  validation_strict" "$RESULTS_VALIDATION_STRICT"

printf "\n"
printf "Total: %d tests\n" "$TOTAL_TESTS"
printf "  %sPassed: %d%s\n" "${GREEN}" "$PASSED_TESTS" "${NC}"
printf "  %sFailed: %d%s\n" "${RED}" "$FAILED_TESTS" "${NC}"

# Cleanup
cleanup

# Generate JUnit XML if requested
if [ -n "$JUNIT_OUTPUT" ]; then
    JUNIT_END_TIME=$(date +%s)
    JUNIT_DURATION=$((JUNIT_END_TIME - JUNIT_START_TIME))
    
    cat > "$JUNIT_OUTPUT" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<testsuite name="MIAOU" tests="$TOTAL_TESTS" failures="$FAILED_TESTS" time="$JUNIT_DURATION">
EOF
    
    # Add test cases
    add_junit_testcase() {
        local name="$1"
        local result="$2"
        if [[ "$result" == "PASSED" ]]; then
            echo "  <testcase name=\"$name\" classname=\"MIAOU.IO\"/>" >> "$JUNIT_OUTPUT"
        elif [[ "$result" == SKIPPED* ]]; then
            echo "  <testcase name=\"$name\" classname=\"MIAOU.IO\"><skipped message=\"${result#SKIPPED: }\"/></testcase>" >> "$JUNIT_OUTPUT"
        else
            echo "  <testcase name=\"$name\" classname=\"MIAOU.IO\"><failure message=\"$result\"/></testcase>" >> "$JUNIT_OUTPUT"
        fi
    }
    
    [ -n "$RESULTS_SERIAL" ] && add_junit_testcase "serial" "$RESULTS_SERIAL"
    [ -n "$RESULTS_MPI" ] && add_junit_testcase "mpi_sequential" "$RESULTS_MPI"
    [ -n "$RESULTS_MPI_PF" ] && add_junit_testcase "mpi_parallel_files" "$RESULTS_MPI_PF"
    [ -n "$RESULTS_NC4PAR" ] && add_junit_testcase "nc4par" "$RESULTS_NC4PAR"
    [ -n "$RESULTS_VALIDATION" ] && add_junit_testcase "validation" "$RESULTS_VALIDATION"
    [ -n "$RESULTS_VALIDATION_STRICT" ] && add_junit_testcase "validation_strict" "$RESULTS_VALIDATION_STRICT"
    
    echo "</testsuite>" >> "$JUNIT_OUTPUT"
    
    echo ""
    echo "JUnit XML report written to: $JUNIT_OUTPUT"
fi

# Exit with appropriate code
if [ $FAILED_TESTS -gt 0 ]; then
    exit 1
else
    exit 0
fi
