# Compile et lance le systeme bancaire sous Windows (GnuCOBOL via MSYS2)
$msys = "C:\msys64\ucrt64"
$env:PATH = "$msys\bin;$env:PATH"
$env:COB_CONFIG_DIR = "$msys\share\gnucobol\config"
$env:COB_COPY_DIR = "$msys\share\gnucobol\copy"
# Sinon REWRITE casse les fichiers LINE SEQUENTIAL sous Windows
$env:COB_UNIX_LF = "Y"

Set-Location $PSScriptRoot
cobc -x -o Bank Bank.cbl
if ($LASTEXITCODE -eq 0) { .\Bank.exe }
