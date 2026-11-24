# build-identity.ps1 -- vessel's package identity on Windows (a sparse package; see AppxManifest.xml).
#
# What this script does, all inside the workspace (nothing here changes Windows):
#   1. copies modus-win64-hosted.exe to vessel.exe in the workspace and embeds a manifest naming the package
#      (mt.exe): Windows gives the package's identity only to an executable that names it
#   2. stages the manifest (publisher and launch parameters filled in) and draws its logos
#   3. packs out\vessel-identity.msix (makeappx), and signs it if a certificate is given (signtool)
#
#   .\build-identity.ps1                                       # pack, unsigned (checks the manifest)
#   .\build-identity.ps1 -Pfx C:\path\vessel-dev.pfx -Password (Read-Host -AsSecureString)
#
# WHAT YOU DO ONCE, YOURSELF (these change Windows' trust settings, so the script never does them):
#   a. a signing certificate whose subject is the publisher, in your user store, exported:
#        $c = New-SelfSignedCertificate -Type Custom -Subject "CN=Vessel Dev" -KeyUsage DigitalSignature `
#               -CertStoreLocation Cert:\CurrentUser\My `
#               -TextExtension @("2.5.29.37={text}1.3.6.1.5.5.7.3.3", "2.5.29.19={text}")
#        Export-PfxCertificate -Cert $c -FilePath vessel-dev.pfx -Password (Read-Host -AsSecureString)
#   b. trust it for packages (an ELEVATED PowerShell):
#        Import-PfxCertificate -FilePath vessel-dev.pfx -CertStoreLocation Cert:\LocalMachine\TrustedPeople `
#               -Password (Read-Host -AsSecureString)
#   c. after a signed build, install it, the workspace as its external location:
#        Add-AppxPackage -Path out\vessel-identity.msix -ExternalLocation C:\Users\you\Documents\modus-lisp
#   "Vessel" is then in Start; it runs vessel.exe --script <Script>.  Remove it with
#        Get-AppxPackage ModusLisp.Vessel | Remove-AppxPackage

param(
  [string]$Workspace = (Resolve-Path "$PSScriptRoot\..\.."),
  [string]$Publisher = "CN=Vessel Dev",
  [string]$Script = "vessel\src\hello3d-windows.lisp",
  [string]$Pfx = "",
  [SecureString]$Password = $null,
  [string]$Sdk = "C:\Program Files (x86)\Windows Kits\10\bin\10.0.22621.0\x64"
)
$ErrorActionPreference = "Stop"
$out = Join-Path $PSScriptRoot "out"
$stage = Join-Path $out "stage"
Remove-Item -Recurse -Force $stage -ErrorAction SilentlyContinue
New-Item -ItemType Directory -Force "$stage\Assets" | Out-Null

# 1. vessel.exe: modus, with the manifest that names the package
$modus = Join-Path $Workspace "modus-win64-hosted.exe"
$exe = Join-Path $Workspace "vessel.exe"
if (-not (Test-Path $modus)) { throw "no $modus" }
Copy-Item -Force $modus $exe
$side = Join-Path $out "vessel.exe.manifest"
@"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly manifestVersion="1.0" xmlns="urn:schemas-microsoft-com:asm.v1">
  <assemblyIdentity version="1.0.0.0" name="ModusLisp.Vessel" type="win32"/>
  <msix xmlns="urn:schemas-microsoft-com:msix.v1"
        publisher="$Publisher" packageName="ModusLisp.Vessel" applicationId="Vessel" />
</assembly>
"@ | Set-Content -Encoding utf8 $side
& "$Sdk\mt.exe" -nologo -manifest $side "-outputresource:$exe;#1"
if ($LASTEXITCODE -ne 0) { throw "mt.exe failed ($LASTEXITCODE)" }

# 2. the manifest and its logos
(Get-Content -Raw "$PSScriptRoot\AppxManifest.xml") `
  -replace 'Publisher="PUBLISHER"', "Publisher=`"$Publisher`"" `
  -replace 'uap10:Parameters="PARAMETERS"', "uap10:Parameters=`"--script $Script`"" |
  Set-Content -Encoding utf8 "$stage\AppxManifest.xml"
Add-Type -AssemblyName System.Drawing
function Logo([int]$size, [string]$name) {
  $bmp = New-Object System.Drawing.Bitmap $size, $size
  $g = [System.Drawing.Graphics]::FromImage($bmp)
  $g.SmoothingMode = "AntiAlias"
  $g.Clear([System.Drawing.Color]::FromArgb(255, 31, 42, 56))
  $brush = New-Object System.Drawing.SolidBrush ([System.Drawing.Color]::FromArgb(255, 120, 190, 255))
  $m = [int]($size * 0.2)
  $g.FillEllipse($brush, $m, $m, $size - 2 * $m, $size - 2 * $m)
  $g.Dispose()
  $bmp.Save("$stage\Assets\$name", [System.Drawing.Imaging.ImageFormat]::Png)
  $bmp.Dispose()
}
Logo 150 "Square150x150Logo.png"
Logo 44 "Square44x44Logo.png"
Logo 50 "StoreLogo.png"

# 3. pack, and sign if asked
$msix = Join-Path $out "vessel-identity.msix"
& "$Sdk\makeappx.exe" pack /o /nv /d $stage /p $msix
if ($LASTEXITCODE -ne 0) { throw "makeappx failed ($LASTEXITCODE)" }
if ($Pfx) {
  $plain = [Runtime.InteropServices.Marshal]::PtrToStringAuto([Runtime.InteropServices.Marshal]::SecureStringToBSTR($Password))
  & "$Sdk\signtool.exe" sign /fd SHA256 /f $Pfx /p $plain $msix
  if ($LASTEXITCODE -ne 0) { throw "signtool failed ($LASTEXITCODE)" }
  "signed: $msix"
} else {
  "packed (unsigned): $msix -- sign it with -Pfx to install it"
}
