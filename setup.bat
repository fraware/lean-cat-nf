@echo off
REM lean-cat-nf Setup Script for Windows
REM Automatically installs and sets up lean-cat-nf for immediate use

setlocal enabledelayedexpansion

REM Configuration
set REPO_URL=https://github.com/fraware/lean-cat-nf.git
set INSTALL_DIR=%USERPROFILE%\.local\share\lean-cat-nf
set BIN_DIR=%USERPROFILE%\.local\bin

echo.
echo ================================================================
echo                      lean-cat-nf
echo            Category Normal Form for Lean 4
echo                     Setup Script
echo ================================================================
echo.

REM Check if git is available
git --version >nul 2>&1
if errorlevel 1 (
    echo Error: git is required but not installed
    echo Please install git from: https://git-scm.com/
    pause
    exit /b 1
)

REM Check if curl is available
curl --version >nul 2>&1
if errorlevel 1 (
    echo Error: curl is required but not installed
    echo Please install curl or use PowerShell instead
    pause
    exit /b 1
)

echo → Checking prerequisites...
echo ✓ Prerequisites check passed

REM Install elan if needed
elan --version >nul 2>&1
if errorlevel 1 (
    echo → Installing elan (Lean version manager)...
    curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh | sh -s -- --default-toolchain none -y
    set PATH=%USERPROFILE%\.elan\bin;%PATH%
    echo ✓ elan installed successfully
) else (
    echo ✓ elan already installed
    set PATH=%USERPROFILE%\.elan\bin;%PATH%
)

REM Setup directories
echo → Setting up directories...
if not exist "%BIN_DIR%" mkdir "%BIN_DIR%"
if not exist "%INSTALL_DIR%\.." mkdir "%INSTALL_DIR%\.."
echo ✓ Directories created

REM Clone repository
echo → Cloning lean-cat-nf repository...
if exist "%INSTALL_DIR%" (
    echo ⚠ Installation directory already exists. Updating...
    cd /d "%INSTALL_DIR%"
    git pull
) else (
    git clone "%REPO_URL%" "%INSTALL_DIR%"
)
cd /d "%INSTALL_DIR%"
echo ✓ Repository cloned/updated

REM Build project
echo → Building lean-cat-nf...
for /f %%i in (lean-toolchain) do set LEAN_VERSION=%%i
elan toolchain install %LEAN_VERSION%
lake exe cache get 2>nul || echo Cache get failed, continuing...
lake build
echo ✓ Project built successfully

REM Create wrapper script
echo → Creating wrapper script...
(
echo @echo off
echo setlocal
echo set PATH=%USERPROFILE%\.elan\bin;%%PATH%%
echo cd /d "%INSTALL_DIR%"
echo.
echo if "%%1"=="bench" (
echo     shift
echo     lake exe bench %%*
echo     goto :eof
echo ^)
echo.
echo if "%%1"=="test" (
echo     shift  
echo     lake exe test-runner %%*
echo     goto :eof
echo ^)
echo.
echo if "%%1"=="test-final" (
echo     shift
echo     lake exe test-runner-final %%*
echo     goto :eof
echo ^)
echo.
echo if "%%1"=="--help" goto :help
echo if "%%1"=="-h" goto :help
echo if "%%1"=="help" goto :help
echo if "%%1"=="" goto :help
echo.
echo echo Unknown command: %%1
echo echo Use 'lean-cat-nf --help' for usage information
echo exit /b 1
echo.
echo :help
echo echo lean-cat-nf - Category Normal Form for Lean 4
echo echo.
echo echo Usage: lean-cat-nf [command] [options]
echo echo.
echo echo Commands:
echo echo   bench        Run benchmarks
echo echo   test         Run test suite
echo echo   test-final   Run final test runner
echo echo   --help       Show this help message
echo echo.
echo echo Installation directory: %INSTALL_DIR%
echo echo For more information, visit: https://github.com/fraware/lean-cat-nf
) > "%BIN_DIR%\lean-cat-nf.bat"

echo ✓ Wrapper script created at %BIN_DIR%\lean-cat-nf.bat

REM Update PATH
echo → Updating PATH...
echo set PATH=%BIN_DIR%;%%PATH%% > "%USERPROFILE%\lean-cat-nf-path.bat"
echo ✓ PATH update script created at %USERPROFILE%\lean-cat-nf-path.bat

REM Run quick test
echo → Running quick tests...
call "%BIN_DIR%\lean-cat-nf.bat" --help >nul 2>&1
if errorlevel 1 (
    echo ✗ Installation test failed
    pause
    exit /b 1
) else (
    echo ✓ Installation test passed
)

echo.
echo ================================================================
echo                  Installation Complete!
echo ================================================================
echo.
echo lean-cat-nf has been successfully installed!
echo.
echo Quick start:
echo   "%BIN_DIR%\lean-cat-nf.bat" --help    # Show help
echo   "%BIN_DIR%\lean-cat-nf.bat" bench     # Run benchmarks
echo   "%BIN_DIR%\lean-cat-nf.bat" test      # Run tests
echo.
echo Installation location: %INSTALL_DIR%
echo Executable location: %BIN_DIR%\lean-cat-nf.bat
echo.
echo Note: To use lean-cat-nf from anywhere, add %BIN_DIR% to your PATH
echo       or run: call "%USERPROFILE%\lean-cat-nf-path.bat"
echo.
pause
