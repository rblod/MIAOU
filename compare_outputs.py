#!/usr/bin/env python3
"""
MIAOU Inter-Mode Output Comparison

This script compares NetCDF outputs from different I/O modes to ensure
they produce equivalent results (within numerical tolerance).

Usage:
    python3 compare_outputs.py dir1 dir2 [--tolerance TOL] [--verbose]

Example:
    python3 compare_outputs.py output_serial/ output_mpi/
"""

import os
import sys
import argparse
import numpy as np

try:
    import netCDF4 as nc
except ImportError:
    print("ERROR: netCDF4 required. Install with: pip install netCDF4")
    sys.exit(1)


class Colors:
    if sys.stdout.isatty():
        GREEN = "\033[92m"
        RED = "\033[91m"
        YELLOW = "\033[93m"
        BLUE = "\033[94m"
        ENDC = "\033[0m"
    else:
        GREEN = RED = YELLOW = BLUE = ENDC = ""


def compare_files(file1, file2, tolerance=1e-6, verbose=False):
    """
    Compare two NetCDF files.
    
    Returns:
        (bool, list): (all_match, list of difference descriptions)
    """
    differences = []
    
    if not os.path.exists(file1):
        return False, [f"File not found: {file1}"]
    if not os.path.exists(file2):
        return False, [f"File not found: {file2}"]
    
    try:
        nc1 = nc.Dataset(file1, 'r')
        nc2 = nc.Dataset(file2, 'r')
    except Exception as e:
        return False, [f"Error opening files: {e}"]
    
    try:
        # Compare dimensions
        dims1 = set(nc1.dimensions.keys())
        dims2 = set(nc2.dimensions.keys())
        
        if dims1 != dims2:
            differences.append(f"Dimension mismatch: {dims1} vs {dims2}")
        else:
            for dim in dims1:
                len1 = len(nc1.dimensions[dim])
                len2 = len(nc2.dimensions[dim])
                if len1 != len2:
                    differences.append(f"Dimension '{dim}' size: {len1} vs {len2}")
        
        # Compare variables
        vars1 = set(nc1.variables.keys())
        vars2 = set(nc2.variables.keys())
        
        # Exclude dimension variables from comparison (may differ in chunking)
        data_vars1 = vars1 - dims1 - {'time'}
        data_vars2 = vars2 - dims2 - {'time'}
        
        if data_vars1 != data_vars2:
            only1 = data_vars1 - data_vars2
            only2 = data_vars2 - data_vars1
            if only1:
                differences.append(f"Variables only in file1: {only1}")
            if only2:
                differences.append(f"Variables only in file2: {only2}")
        
        # Compare common variables
        common_vars = data_vars1 & data_vars2
        
        for var in sorted(common_vars):
            var1 = nc1.variables[var]
            var2 = nc2.variables[var]
            
            # Check shapes
            if var1.shape != var2.shape:
                differences.append(f"Variable '{var}' shape: {var1.shape} vs {var2.shape}")
                continue
            
            # Compare data
            data1 = var1[:]
            data2 = var2[:]
            
            # Handle masked arrays
            if hasattr(data1, 'mask'):
                data1 = np.ma.filled(data1, np.nan)
            if hasattr(data2, 'mask'):
                data2 = np.ma.filled(data2, np.nan)
            
            # Skip if all NaN
            valid1 = ~np.isnan(data1)
            valid2 = ~np.isnan(data2)
            
            if not np.array_equal(valid1, valid2):
                differences.append(f"Variable '{var}': different NaN patterns")
                continue
            
            if not np.any(valid1):
                if verbose:
                    print(f"  {var}: all NaN, skipping comparison")
                continue
            
            # Calculate differences where both are valid
            diff = np.abs(data1[valid1] - data2[valid2])
            max_diff = np.max(diff)
            mean_diff = np.mean(diff)
            
            # Relative error (avoid division by zero)
            scale = np.maximum(np.abs(data1[valid1]), np.abs(data2[valid2]))
            scale = np.where(scale > 1e-10, scale, 1.0)
            rel_diff = diff / scale
            max_rel_diff = np.max(rel_diff)
            
            if max_diff > tolerance:
                differences.append(
                    f"Variable '{var}': max_diff={max_diff:.2e}, "
                    f"mean_diff={mean_diff:.2e}, max_rel={max_rel_diff:.2e}"
                )
            elif verbose:
                print(f"  {var}: OK (max_diff={max_diff:.2e})")
        
        # Compare time variable specifically
        if 'time' in vars1 and 'time' in vars2:
            time1 = nc1.variables['time'][:]
            time2 = nc2.variables['time'][:]
            
            if len(time1) != len(time2):
                differences.append(f"Time records: {len(time1)} vs {len(time2)}")
            else:
                time_diff = np.max(np.abs(time1 - time2))
                if time_diff > tolerance:
                    differences.append(f"Time values differ: max_diff={time_diff:.2e}")
                elif verbose:
                    print(f"  time: OK ({len(time1)} records)")
    
    finally:
        nc1.close()
        nc2.close()
    
    return len(differences) == 0, differences


def find_matching_files(dir1, dir2):
    """
    Find NetCDF files that exist in both directories.
    
    Returns:
        list of (file1, file2, basename) tuples
    """
    matches = []
    
    files1 = {f for f in os.listdir(dir1) if f.endswith('.nc')}
    files2 = {f for f in os.listdir(dir2) if f.endswith('.nc')}
    
    # Handle MPI parallel files (remove rank suffixes)
    def base_name(f):
        # ocean_hourly_3600s_0000.nc -> ocean_hourly_3600s.nc
        import re
        return re.sub(r'_\d{4}\.nc$', '.nc', f)
    
    # For serial/sequential: direct match
    common = files1 & files2
    for f in common:
        matches.append((os.path.join(dir1, f), os.path.join(dir2, f), f))
    
    return matches


def main():
    parser = argparse.ArgumentParser(
        description="Compare MIAOU NetCDF outputs from different I/O modes"
    )
    parser.add_argument("dir1", help="First output directory")
    parser.add_argument("dir2", help="Second output directory")
    parser.add_argument("--tolerance", "-t", type=float, default=1e-6,
                        help="Numerical tolerance (default: 1e-6)")
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Verbose output")
    parser.add_argument("--junit", "-j", metavar="FILE",
                        help="Output JUnit XML report")
    
    args = parser.parse_args()
    
    if not os.path.isdir(args.dir1):
        print(f"{Colors.RED}Error: {args.dir1} is not a directory{Colors.ENDC}")
        sys.exit(1)
    if not os.path.isdir(args.dir2):
        print(f"{Colors.RED}Error: {args.dir2} is not a directory{Colors.ENDC}")
        sys.exit(1)
    
    print(f"{Colors.BLUE}Comparing outputs:{Colors.ENDC}")
    print(f"  Directory 1: {args.dir1}")
    print(f"  Directory 2: {args.dir2}")
    print(f"  Tolerance: {args.tolerance}")
    print()
    
    matches = find_matching_files(args.dir1, args.dir2)
    
    if not matches:
        print(f"{Colors.YELLOW}No matching NetCDF files found{Colors.ENDC}")
        sys.exit(1)
    
    total = 0
    passed = 0
    failed = 0
    results = []
    
    for file1, file2, basename in sorted(matches):
        total += 1
        print(f"Comparing: {basename}")
        
        match, diffs = compare_files(file1, file2, args.tolerance, args.verbose)
        
        if match:
            print(f"  {Colors.GREEN}✓ Files match{Colors.ENDC}")
            passed += 1
            results.append((basename, "PASSED", []))
        else:
            print(f"  {Colors.RED}✗ Files differ:{Colors.ENDC}")
            for d in diffs:
                print(f"    - {d}")
            failed += 1
            results.append((basename, "FAILED", diffs))
    
    print()
    print(f"{Colors.BLUE}{'='*60}{Colors.ENDC}")
    print(f"Summary: {total} files compared")
    print(f"  {Colors.GREEN}Passed: {passed}{Colors.ENDC}")
    print(f"  {Colors.RED}Failed: {failed}{Colors.ENDC}")
    
    # Generate JUnit XML if requested
    if args.junit:
        with open(args.junit, 'w') as f:
            f.write('<?xml version="1.0" encoding="UTF-8"?>\n')
            f.write(f'<testsuite name="MIAOU-Compare" tests="{total}" failures="{failed}">\n')
            for basename, status, diffs in results:
                if status == "PASSED":
                    f.write(f'  <testcase name="{basename}" classname="MIAOU.Compare"/>\n')
                else:
                    f.write(f'  <testcase name="{basename}" classname="MIAOU.Compare">\n')
                    f.write(f'    <failure message="Files differ">\n')
                    for d in diffs:
                        f.write(f'      {d}\n')
                    f.write(f'    </failure>\n')
                    f.write(f'  </testcase>\n')
            f.write('</testsuite>\n')
        print(f"\nJUnit report: {args.junit}")
    
    if failed > 0:
        print(f"\n{Colors.RED}COMPARISON FAILED{Colors.ENDC}")
        sys.exit(1)
    else:
        print(f"\n{Colors.GREEN}ALL FILES MATCH{Colors.ENDC}")
        sys.exit(0)


if __name__ == "__main__":
    main()
