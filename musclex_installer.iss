; MuscleX Installer Script for Inno Setup
; Generated based on Advanced Installer configuration

#define MyAppName "MuscleX"
#define MyAppVersion "1.26.1"
#define MyAppPublisher "BioCAT"
#define MyAppURL "https://github.com/biocatiit/musclex"
#define MyAppExeName "musclex-launcher.exe"
#define MyAppContact "biocat@lethocerus.biol.iit.edu"
#define MyAppHelpURL "https://musclex.readthedocs.io/"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
AppId={{D8F23B9E-AF5B-4A27-8D6D-C56172626614}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppHelpURL}
AppUpdatesURL={#MyAppURL}
AppContact={#MyAppContact}
DefaultDirName={autopf}\{#MyAppPublisher}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
LicenseFile=LICENSE.txt
OutputDir=dist
OutputBaseFilename=musclex-{#MyAppVersion}-Windows-Setup
SetupIconFile=dev_docs\win\AppIcon.ico
Compression=lzma
SolidCompression=yes
WizardStyle=modern
; Require 64-bit Windows
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
; Minimum Windows version
MinVersion=6.1sp1
UninstallDisplayIcon={app}\{#MyAppExeName}
UninstallDisplayName={#MyAppName} {#MyAppVersion}

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
; Include all files from PyInstaller dist/musclex folder
Source: "dist\musclex\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
; Start Menu shortcut
Name: "{group}\{#MyAppName}@{#MyAppVersion}"; Filename: "{app}\{#MyAppExeName}"; WorkingDir: "{app}"
Name: "{group}\{cm:ProgramOnTheWeb,{#MyAppName}}"; Filename: "{#MyAppURL}"
Name: "{group}\Documentation"; Filename: "{#MyAppHelpURL}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
; Desktop shortcut (optional)
Name: "{autodesktop}\{#MyAppName}@{#MyAppVersion}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon; WorkingDir: "{app}"

[Registry]
; Store installation path in registry
Root: HKLM; Subkey: "Software\{#MyAppPublisher}\{#MyAppName}"; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: "Software\{#MyAppPublisher}\{#MyAppName}"; ValueType: string; ValueName: "Path"; ValueData: "{app}"
Root: HKLM; Subkey: "Software\{#MyAppPublisher}\{#MyAppName}"; ValueType: string; ValueName: "Version"; ValueData: "{#MyAppVersion}"

[Run]
; Option to launch the application after installation
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

[Code]
// Custom code to get version dynamically (if needed)
function GetAppVersion: String;
begin
  Result := '{#MyAppVersion}';
end;

// Check if .NET or other dependencies are needed
function InitializeSetup(): Boolean;
begin
  Result := True;
  // Add any pre-installation checks here if needed
end;

