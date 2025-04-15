#!/usr/bin/env python3
"""
Enhanced verification script for MIAOU
This script:
1. Reproduces the calculations from the main_test_output program
2. Reads the namelist configuration with support for global defaults
3. Compares calculated values with the content of generated NetCDF files
4. Supports 0D (scalar), 1D, 2D, and 3D variables
5. Displays errors in red and success in green
6. Provides a summary of errors at the end
"""

import os
import re
import numpy as np
import xarray as xr
from datetime import datetime


# ANSI color codes for terminal output
class Colors:
    HEADER = "\033[95m"
    BLUE = "\033[94m"
    GREEN = "\033[92m"
    YELLOW = "\033[93m"
    RED = "\033[91m"
    ENDC = "\033[0m"
    BOLD = "\033[1m"
    UNDERLINE = "\033[4m"


def color_print(message, color):
    """Print a message with color"""
    print(f"{color}{message}{Colors.ENDC}")


# Simulation parameters (identical to those in main_test_output program)
NX = 10
NY = 8
NZ = 5
NT = 10
DT = 1800.0  # seconds - time step

# Global error tracking
error_summary = []


def parse_namelist(filename="output_config.nml"):
    """Parse the namelist file to extract output configuration with global defaults support"""

    config = {
        "global": {
            "output_prefix": "ocean",  # Default prefix
            "global_freq_his": 3600.0,  # Default frequencies
            "global_freq_avg": 7200.0,
            "global_freq_rst": 9000.0,
            "global_to_his": True,  # Default output flags
            "global_to_avg": False,
            "global_to_rst": True,
        },
        "variables": [],
    }

    if not os.path.exists(filename):
        color_print(f"ERROR: File {filename} not found!", Colors.RED)
        return config

    # Read the file
    with open(filename, "r") as f:
        content = f.read()

    # Extract global parameters
    global_match = re.search(r"&output_global(.*?)/", content, re.DOTALL)
    if global_match:
        global_section = global_match.group(1)

        # Extract output_prefix
        prefix_match = re.search(r'output_prefix\s*=\s*"([^"]*)"', global_section)
        if prefix_match:
            config["global"]["output_prefix"] = prefix_match.group(1)

        # Extract global frequencies
        freq_his_match = re.search(r"global_freq_his\s*=\s*([\d\.]+)", global_section)
        if freq_his_match:
            config["global"]["global_freq_his"] = float(freq_his_match.group(1))

        freq_avg_match = re.search(r"global_freq_avg\s*=\s*([\d\.]+)", global_section)
        if freq_avg_match:
            config["global"]["global_freq_avg"] = float(freq_avg_match.group(1))

        freq_rst_match = re.search(r"global_freq_rst\s*=\s*([\d\.]+)", global_section)
        if freq_rst_match:
            config["global"]["global_freq_rst"] = float(freq_rst_match.group(1))

        # Extract global output flags
        to_his_match = re.search(
            r"global_to_his\s*=\s*(\.true\.|\.false\.)", global_section
        )
        if to_his_match:
            config["global"]["global_to_his"] = to_his_match.group(1) == ".true."

        to_avg_match = re.search(
            r"global_to_avg\s*=\s*(\.true\.|\.false\.)", global_section
        )
        if to_avg_match:
            config["global"]["global_to_avg"] = to_avg_match.group(1) == ".true."

        to_rst_match = re.search(
            r"global_to_rst\s*=\s*(\.true\.|\.false\.)", global_section
        )
        if to_rst_match:
            config["global"]["global_to_rst"] = to_rst_match.group(1) == ".true."

    # Extract variables
    dyn_match = re.search(r"&output_vars(.*?)/", content, re.DOTALL)
    if dyn_match:
        dyn_section = dyn_match.group(1)

        # Find all var_configs lines
        var_lines = re.findall(
            r'var_configs\(\d+\)\s*=\s*"([^"]*)"\s*,\s*(\.true\.|\.false\.)\s*,\s*(\.true\.|\.false\.)\s*,\s*(\.true\.|\.false\.)\s*,\s*"([^"]*)"\s*,\s*([^,]*),\s*([^,]*),\s*([^.,]*)',
            dyn_section,
        )
        for var_line in var_lines:
            name, wrt, avg, rst, prefix, freq_his, freq_avg, freq_rst = var_line
            var_config = {
                "name": name.strip(),
                "wrt": wrt == ".true.",
                "avg": avg == ".true.",
                "rst": rst == ".true.",
                "prefix": prefix.strip(),
                "freq_his": float(freq_his),
                "freq_avg": float(freq_avg),
                "freq_rst": float(freq_rst),
            }

            # Apply global defaults for missing values
            if not (var_config["wrt"] or var_config["avg"] or var_config["rst"]):
                var_config["wrt"] = config["global"]["global_to_his"]
                var_config["avg"] = config["global"]["global_to_avg"]
                var_config["rst"] = config["global"]["global_to_rst"]

            if var_config["wrt"] and var_config["freq_his"] < 0:
                var_config["freq_his"] = config["global"]["global_freq_his"]

            if var_config["avg"] and var_config["freq_avg"] < 0:
                var_config["freq_avg"] = config["global"]["global_freq_avg"]

            if var_config["rst"] and var_config["freq_rst"] < 0:
                var_config["freq_rst"] = config["global"]["global_freq_rst"]

            config["variables"].append(var_config)

    # Debug info
    color_print(
        f"Global output_prefix: '{config['global']['output_prefix']}'", Colors.BLUE
    )
    color_print(f"Found {len(config['variables'])} variables in namelist:", Colors.BLUE)
    for var in config["variables"]:
        print(
            f"  - {var['name']}: his={var['wrt']}, avg={var['avg']}, rst={var['rst']}, prefix='{var['prefix']}'"
        )

    return config


def simulate_calculations():
    """Reproduces the analytical calculations performed in the model"""

    # Initialize arrays for different dimensions
    zeta = np.zeros((NX, NY))  # 2D
    temp = np.zeros((NX, NY, NZ))  # 3D
    u = np.zeros((NX, NY))  # 2D
    v = np.zeros((NX, NY))  # 2D
    wind_speed = 0.0  # 0D (scalar)
    temp_profile = np.zeros(NZ)  # 1D

    # Array to store results at each time step
    results = {
        "time": [],
        "zeta": [],
        "temp": [],
        "u": [],
        "v": [],
        "wind_speed": [],
        "temp_profile": [],
    }

    # Time loop
    for t in range(1, NT + 1):
        # Time in seconds
        current_time = t * DT

        # Update values (as in the model)
        zeta = zeta + 0.1 * t  # 2D field
        temp = temp + 0.2 * t  # 3D field
        u[:] = t  # 2D field
        v[:] = t * 0.5  # 2D field
        wind_speed = 5.0 + 0.1 * t  # 0D scalar
        temp_profile = temp_profile + 0.5 * t  # 1D profile

        # Store results
        results["time"].append(current_time)
        results["zeta"].append(zeta.copy())
        results["temp"].append(temp.copy())
        results["u"].append(u.copy())
        results["v"].append(v.copy())
        results["wind_speed"].append(wind_speed)
        results["temp_profile"].append(temp_profile.copy())

    return results


def find_netcdf_files():
    """Finds all NetCDF files generated by the model"""

    nc_files = []
    for filename in os.listdir("."):
        if filename.endswith(".nc"):
            nc_files.append(filename)

    return nc_files


def extract_file_info(filename):
    """Extracts information from the filename"""
    info = {"prefix": None, "type": None, "freq": None}

    # Match the expected pattern: prefix_type_freqs.nc or prefix_type.nc
    pattern1 = r"(.+)_(his|avg|rst)_(\d+)s\.nc$"
    pattern2 = r"(.+)_(his|avg|rst)\.nc$"

    match1 = re.match(pattern1, filename)
    match2 = re.match(pattern2, filename)

    if match1:
        info["prefix"] = match1.group(1)
        info["type"] = match1.group(2)
        info["freq"] = int(match1.group(3))
    elif match2:
        info["prefix"] = match2.group(1)
        info["type"] = match2.group(2)
        info["freq"] = -1
    else:
        # Fallback partial detection
        if "_his_" in filename:
            info["type"] = "his"
        elif "_avg_" in filename:
            info["type"] = "avg"
        elif "_rst_" in filename:
            info["type"] = "rst"

        freq_match = re.search(r"_(\d+)s\.nc$", filename)
        if freq_match:
            info["freq"] = int(freq_match.group(1))

        # Try to extract prefix
        parts = filename.split("_")
        if len(parts) > 0:
            info["prefix"] = parts[0]

    return info


def should_variable_be_in_file(var_config, file_info, config):
    """Determines if a variable should be in a file based on configuration"""
    # Debug info
    print(f"    Debug info for variable '{var_config['name']}':")
    print(
        f"      - File prefix: '{file_info['prefix']}', type: {file_info['type']}, freq: {file_info['freq']}"
    )
    print(
        f"      - Var config: prefix='{var_config['prefix']}', his={var_config['wrt']}, avg={var_config['avg']}, rst={var_config['rst']}"
    )
    print(f"      - Global prefix: '{config['global']['output_prefix']}'")

    # 1. Check file type compatibility
    if file_info["type"] == "his" and not var_config["wrt"]:
        print(f"      - EXCLUDE: Variable not set for history output")
        return False
    elif file_info["type"] == "avg" and not var_config["avg"]:
        print(f"      - EXCLUDE: Variable not set for average output")
        return False
    elif file_info["type"] == "rst" and not var_config["rst"]:
        print(f"      - EXCLUDE: Variable not set for restart output")
        return False

    # 2. Check frequency compatibility
    if file_info["type"] == "his" and var_config["freq_his"] > 0:
        if abs(var_config["freq_his"] - file_info["freq"]) > 1e-5:
            print(
                f"      - EXCLUDE: Frequency mismatch for history - expected {var_config['freq_his']}, got {file_info['freq']}"
            )
            return False
    elif file_info["type"] == "avg" and var_config["freq_avg"] > 0:
        if abs(var_config["freq_avg"] - file_info["freq"]) > 1e-5:
            print(
                f"      - EXCLUDE: Frequency mismatch for average - expected {var_config['freq_avg']}, got {file_info['freq']}"
            )
            return False
    elif file_info["type"] == "rst" and var_config["freq_rst"] > 0:
        if abs(var_config["freq_rst"] - file_info["freq"]) > 1e-5:
            print(
                f"      - EXCLUDE: Frequency mismatch for restart - expected {var_config['freq_rst']}, got {file_info['freq']}"
            )
            return False

    # 3. Check prefix compatibility - FIXED
    file_prefix = file_info["prefix"]

    # Variable with empty prefix should use global prefix
    if var_config["prefix"] == "":
        if file_prefix == config["global"]["output_prefix"]:
            print(
                f"      - INCLUDE: File uses global prefix '{config['global']['output_prefix']}'"
            )
            return True
        else:
            print(
                f"      - EXCLUDE: Prefix mismatch - expected global '{config['global']['output_prefix']}', got '{file_prefix}'"
            )
            return False
    else:
        # Variable has custom prefix
        if file_prefix == var_config["prefix"]:
            print(
                f"      - INCLUDE: File uses matching custom prefix '{var_config['prefix']}'"
            )
            return True
        else:
            print(
                f"      - EXCLUDE: Prefix mismatch - expected custom '{var_config['prefix']}', got '{file_prefix}'"
            )
            return False


def check_file_content(filename, config, calc_results):
    """Checks the content of a NetCDF file against analytical calculations"""

    color_print(f"\nVerifying file: {filename}", Colors.BLUE)
    file_has_errors = False

    # Extract file information
    file_info = extract_file_info(filename)

    # Display file identification info
    print(f"  File identification:")
    print(f"  - Type: {file_info['type']}")
    print(f"  - Prefix: {file_info['prefix']}")
    print(f"  - Frequency: {file_info['freq']}s")

    # Open NetCDF file
    try:
        nc = xr.open_dataset(filename, decode_times=False)
    except Exception as e:
        error_msg = f"ERROR: Cannot open {filename}: {e}"
        color_print(error_msg, Colors.RED)
        error_summary.append(f"File error: {filename} - {e}")
        return

    # Display general information
    print(f"  Dimensions: {list(nc.dims)}")
    print(f"  Variables: {list(nc.data_vars.keys())}")

    # Get time values
    if "time" in nc.coords:
        time_points = nc.time.values
        time_points = time_points.astype(float)
    else:
        error_msg = "  WARNING: No time dimension found in file"
        color_print(error_msg, Colors.YELLOW)
        nc.close()
        return

    # Map variable names
    var_name_map = {
        "zeta": "zeta",
        "temp": "temp",
        "u": "u",
        "v": "v",
        "wind": "wind_speed",
        "profile": "temp_profile",
    }

    # Check each variable in the file
    for var_name in nc.data_vars:
        if var_name == "time":
            continue  # Skip time variable

        var_has_errors = False
        print(f"\n  Checking variable: {var_name}")

        # Find configuration for this variable
        var_config = None
        for var in config["variables"]:
            if var["name"] == var_name:
                var_config = var
                break

        if var_config is None:
            # Try reverse mapping
            for config_name, nc_name in var_name_map.items():
                if nc_name == var_name:
                    for var in config["variables"]:
                        if var["name"] == config_name:
                            var_config = var
                            break
                    if var_config:
                        break

        # If still not found, create default config
        if var_config is None:
            color_print(
                f"    WARNING: Creating default config for variable {var_name}",
                Colors.YELLOW,
            )
            var_config = {
                "name": var_name,
                "wrt": file_info["type"] == "his",
                "avg": file_info["type"] == "avg",
                "rst": file_info["type"] == "rst",
                "prefix": "",  # Use empty prefix to default to global prefix
                "freq_his": config["global"]["global_freq_his"],
                "freq_avg": config["global"]["global_freq_avg"],
                "freq_rst": config["global"]["global_freq_rst"],
            }

        # Check if variable should be in this file
        if not should_variable_be_in_file(var_config, file_info, config):
            error_msg = f"    ERROR: Variable {var_name} should NOT be in this file!"
            color_print(error_msg, Colors.RED)
            var_has_errors = True
            file_has_errors = True
            error_summary.append(
                f"File placement error: {var_name} in {filename} shouldn't be there"
            )
            continue

        # Read data
        var_data = nc[var_name].values

        # Map variable name to results key
        result_key = var_name
        for k, v in var_name_map.items():
            if v == var_name:
                result_key = v
                break

        # Handle comparison by dimension
        value_errors = 0
        for i, time_value in enumerate(time_points):
            # Skip t=0 for avg and rst files
            if abs(time_value) < 1e-5 and file_info["type"] in ["avg", "rst"]:
                continue

            time_idx = np.argmin(np.abs(np.array(calc_results["time"]) - time_value))

            # Determine dimensionality and prepare expected values
            ndim = len(var_data.shape) - 1  # Subtract 1 for time dimension

            if ndim == 0:  # 0D (scalar)
                expected = calc_results[result_key][time_idx]
                actual = var_data[i]

            elif ndim == 1:  # 1D
                expected = calc_results[result_key][time_idx]
                actual = var_data[i]

            elif ndim == 2:  # 2D
                expected = calc_results[result_key][time_idx]
                actual = var_data[i]

            elif ndim == 3:  # 3D
                expected = calc_results[result_key][time_idx]
                actual = var_data[i]
            else:
                warning_msg = (
                    f"    WARNING: Unexpected dimensionality {ndim} for {var_name}"
                )
                color_print(warning_msg, Colors.YELLOW)
                continue

            # For averages, calculate expected average
            if file_info["type"] == "avg":
                if file_info["freq"] > 0:
                    # Number of steps per average
                    steps_per_avg = int(file_info["freq"] / DT)

                    # Determine current interval
                    current_interval = int(time_value / file_info["freq"])

                    # Start and end times for averaging
                    avg_start_time = (current_interval - 1) * file_info["freq"]
                    avg_end_time = current_interval * file_info["freq"]

                    # Find indices for averaging
                    avg_indices = []
                    for j in range(len(calc_results["time"])):
                        t = calc_results["time"][j]
                        if t > avg_start_time and t <= avg_end_time:
                            avg_indices.append(j)

                    # If no points found, use alternative
                    if not avg_indices:
                        time_idx = np.argmin(
                            np.abs(np.array(calc_results["time"]) - time_value)
                        )
                        avg_indices = list(
                            range(max(0, time_idx - steps_per_avg + 1), time_idx + 1)
                        )

                    # Calculate average based on dimension
                    if ndim == 0:  # Scalar
                        avg_sum = 0.0
                        for idx in avg_indices:
                            avg_sum += calc_results[result_key][idx]
                        expected = avg_sum / len(avg_indices)

                    else:  # Arrays (1D, 2D, 3D)
                        avg_sum = np.zeros_like(expected)
                        for idx in avg_indices:
                            avg_sum += calc_results[result_key][idx]
                        expected = avg_sum / len(avg_indices)

            # Calculate error
            if ndim == 0:  # 0D (scalar)
                if abs(expected) < 1e-10:
                    rel_error = abs(actual - expected)
                else:
                    rel_error = abs((actual - expected) / expected)

                max_error = rel_error

                if max_error > 1e-5:
                    error_msg = f"    ERROR at t={time_value}: Error = {max_error:.6f}"
                    color_print(error_msg, Colors.RED)
                    print(f"      Expected: {expected}, Actual: {actual}")
                    value_errors += 1
                    var_has_errors = True
                    file_has_errors = True
                else:
                    success_msg = f"    OK at t={time_value}: Error = {max_error:.6f}"
                    color_print(success_msg, Colors.GREEN)

            else:  # 1D, 2D, 3D
                # Flatten arrays for comparison
                expected_flat = expected.flatten()
                actual_flat = actual.flatten()

                # Calculate relative error
                with np.errstate(divide="ignore", invalid="ignore"):
                    rel_error = np.abs(
                        (actual_flat - expected_flat)
                        / np.maximum(1e-10, np.abs(expected_flat))
                    )
                    # Replace NaN or inf values with absolute error
                    rel_error = np.where(
                        np.isnan(rel_error) | np.isinf(rel_error),
                        np.abs(actual_flat - expected_flat),
                        rel_error,
                    )

                max_error = np.max(rel_error)

                if max_error > 1e-5:
                    error_msg = (
                        f"    ERROR at t={time_value}: Max error = {max_error:.6f}"
                    )
                    color_print(error_msg, Colors.RED)
                    # Find location of maximum error
                    max_error_idx = np.argmax(rel_error)
                    print(
                        f"      Expected[{max_error_idx}]: {expected_flat[max_error_idx]}, "
                        f"Actual[{max_error_idx}]: {actual_flat[max_error_idx]}"
                    )
                    value_errors += 1
                    var_has_errors = True
                    file_has_errors = True
                else:
                    success_msg = (
                        f"    OK at t={time_value}: Max error = {max_error:.6f}"
                    )
                    color_print(success_msg, Colors.GREEN)

        # Add summary for this variable if it had errors
        if var_has_errors and value_errors > 0:
            error_summary.append(
                f"Value error: {var_name} in {filename} has {value_errors} incorrect values"
            )

    nc.close()

    # Final status for this file
    if file_has_errors:
        color_print(f"\n  ❌ File {filename} has errors", Colors.RED)
    else:
        color_print(f"\n  ✅ File {filename} is correct", Colors.GREEN)


def main():
    # Reset error summary
    global error_summary
    error_summary = []

    color_print("Enhanced verification script for MIAOU", Colors.HEADER + Colors.BOLD)
    color_print("=====================================", Colors.HEADER)

    # Parse command line arguments
    import argparse

    parser = argparse.ArgumentParser(description="Verify output of MIAOU.")
    parser.add_argument(
        "--namelist",
        default="output_config.nml",
        help="Path to the namelist file (default: output_config.nml)",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Enable verbose debug output (default: false)",
    )
    args = parser.parse_args()

    # Step 1: Analyze configuration
    print("\nAnalyzing configuration...")
    config = parse_namelist(args.namelist)

    if not config["variables"]:
        color_print("ERROR: No variables found in configuration!", Colors.RED)
        return

    print(f"Global settings:")
    print(f"  Output prefix: '{config['global']['output_prefix']}'")
    print(
        f"  Default frequencies: his={config['global']['global_freq_his']}s, avg={config['global']['global_freq_avg']}s, rst={config['global']['global_freq_rst']}s"
    )
    print(
        f"  Default output flags: his={config['global']['global_to_his']}, avg={config['global']['global_to_avg']}, rst={config['global']['global_to_rst']}"
    )

    # Step 2: Reproduce calculations
    print("\nReproducing analytical calculations...")
    calc_results = simulate_calculations()
    print(f"Calculations performed for {len(calc_results['time'])} time steps")

    # Step 3: Find NetCDF files
    print("\nSearching for NetCDF files...")
    nc_files = find_netcdf_files()

    if not nc_files:
        color_print("ERROR: No NetCDF files found!", Colors.RED)
        return

    print(f"Files found: {nc_files}")

    # Step 4: Check each file
    for filename in nc_files:
        check_file_content(filename, config, calc_results)

    # Final summary
    if error_summary:
        color_print("\n===== ERROR SUMMARY =====", Colors.RED + Colors.BOLD)
        for i, error in enumerate(error_summary, 1):
            color_print(f"{i}. {error}", Colors.RED)
        color_print(
            f"\n❌ Verification completed with {len(error_summary)} errors",
            Colors.RED + Colors.BOLD,
        )
    else:
        color_print(
            "\n✅ Verification completed successfully! All files and values are correct.",
            Colors.GREEN + Colors.BOLD,
        )


if __name__ == "__main__":
    main()
