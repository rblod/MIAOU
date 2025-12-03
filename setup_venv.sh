#!/bin/bash
# Installation script for MIAOU verification using Python's built-in venv module
# Uses xarray instead of netCDF4 for better compatibility on macOS

echo "Setting up virtual environment for MIAOU verification script..."

# Check if Python is installed
if ! command -v python3 &> /dev/null; then
    echo "ERROR: Python 3 is not installed. Please install Python 3 first."
    exit 1
fi

# Create virtual environment using the built-in venv module
VENV_DIR="miaou_venv"
if [ -d "$VENV_DIR" ]; then
    echo "Virtual environment already exists. Do you want to recreate it? (y/n)"
    read answer
    if [ "$answer" = "y" ]; then
        echo "Removing existing environment..."
        rm -rf $VENV_DIR
        echo "Creating new virtual environment in $VENV_DIR..."
        python3 -m venv $VENV_DIR
    fi
else
    echo "Creating virtual environment in $VENV_DIR..."
    python3 -m venv $VENV_DIR
fi

if [ ! -d "$VENV_DIR" ]; then
    echo "ERROR: Failed to create virtual environment."
    exit 1
fi

echo "Activating virtual environment..."
source $VENV_DIR/bin/activate

if [ $? -ne 0 ]; then
    echo "ERROR: Failed to activate virtual environment."
    exit 1
fi

# Upgrade pip inside the virtual environment
echo "Upgrading pip in virtual environment..."
pip install --upgrade pip

# Install required Python packages in the virtual environment
echo "Installing Python packages in virtual environment..."
pip install numpy matplotlib xarray dask[array] scipy netCDF4

# Check if installations were successful
echo "Verifying installations..."
python -c "import numpy as np; print(f'NumPy version: {np.__version__}')"
python -c "import matplotlib as mpl; print(f'Matplotlib version: {mpl.__version__}')"
python -c "import xarray as xr; print(f'xarray version: {xr.__version__}')"
python -c "import dask; print(f'dask version: {dask.__version__}')"
python -c "import scipy; print(f'scipy version: {scipy.__version__}')"

# Create an activation script for convenience
cat > activate_miaou_env.sh << EOF
#!/bin/bash
source $VENV_DIR/bin/activate
echo "MIAOU verification environment activated. Run 'deactivate' to exit."
EOF

chmod +x activate_miaou_env.sh

# Display final instructions
echo ""
echo "Setup complete! To use the verification script:"
echo ""
echo "1. Activate the virtual environment with:"
echo "   source activate_miaou_env.sh"
echo ""
echo "2. Run the verification script with:"
echo "   python verify__output.py"
echo ""
echo "3. When finished, deactivate the virtual environment with:"
echo "   deactivate"
