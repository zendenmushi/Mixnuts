unit NetCoreClr;

interface
uses
  System.SysUtils, System.Variants, System.Classes
  {$ifdef MSWINDOWS}
  ,Winapi.Windows
  {$endif}
  {$ifdef MACOS}

  {$endif}
  ;
type
  THostHandle = Pointer;
  PHostHandle = ^THostHandle;
  TDelegate = Pointer;
  PDelegate = ^TDelegate;
  {$ifdef MSWINDOWS}
  TCoreclr_initialize_ptr = function(exePath : PAnsiChar ; appDomainFriendlyName : PAnsiChar; propertyCount : Integer; propertyKeys : PPAnsiChar; propertyValues : PPAnsiChar ; hostHandle : PHostHandle; domainId : PUint32 ) : integer; stdcall;
  TCoreclr_shutdown_ptr = function(hostHandle : THostHandle; domainId : UInt32 ) : integer; stdcall;
  TCoreclr_shutdown_2_ptr = function(hostHandle : THostHandle; domainId : UInt32; latchedExitCode : PInteger ) : integer; stdcall;
  TCoreclr_create_delegate_ptr = function(hostHandle : THostHandle; domainId : UInt32;  entryPointAssemblyName : PAnsiChar; entryPointTypeName : PAnsiChar; entryPointMethodName : PAnsiChar; delegate : PDelegate ) : integer; stdcall;
  TCoreclr_execute_assembly_ptr = function(hostHandle : THostHandle; domainId : UInt32;  argc : Integer; argv : PPAnsiChar; managedAssemblyPath : PAnsiChar; exitCode : PInteger ) : integer; stdcall;
  {$endif}
  {$ifdef MACOS}
  TCoreclr_initialize_ptr = function(exePath : PAnsiChar ; appDomainFriendlyName : PAnsiChar; propertyCount : Integer; propertyKeys : PPAnsiChar; propertyValues : PPAnsiChar ; hostHandle : PHostHandle; domainId : PUint32 ) : integer; cdecl;
  TCoreclr_shutdown_ptr = function(hostHandle : THostHandle; domainId : UInt32 ) : integer; cdecl;
  TCoreclr_shutdown_2_ptr = function(hostHandle : THostHandle; domainId : UInt32; latchedExitCode : PInteger ) : integer; cdecl;
  TCoreclr_create_delegate_ptr = function(hostHandle : THostHandle; domainId : UInt32;  entryPointAssemblyName : PAnsiChar; entryPointTypeName : PAnsiChar; entryPointMethodName : PAnsiChar; delegate : PDelegate ) : integer; cdecl;
  TCoreclr_execute_assembly_ptr = function(hostHandle : THostHandle; domainId : UInt32;  argc : Integer; argv : PPAnsiChar; managedAssemblyPath : PAnsiChar; exitCode : PInteger ) : integer; cdecl;
  {$endif}

  TNetCoreClr = class
  private
    FClrPath : string;
    FClrHandle : THandle;
    FHostHandle : THostHandle;
    FDomainID : UInt32;
    FAppDomainFriendlyName : string;
    FTpaList : string;
    FAppPaths : string;
    FClrStarted : Boolean;
    FInitializeCoreClr : TCoreclr_initialize_ptr;
    FCreateManagedDelegate : TCoreclr_create_delegate_ptr;
    FShutdownCoreClr : TCoreclr_shutdown_ptr;

    procedure BuildTpaList(directory, extension: string);
  protected

  public

    constructor Create(coreclrPath : string; appDomainFriendlyName : string);
    destructor  Destroy; override;

    class function IsValidRuntimePath(Path : string) : Boolean;

    procedure Start();
    procedure ShutDown();

    procedure AddAppPath(path : string);
    function CreateDelegate(AssemblyName : string; TypeName : string; MethodName : string) : TDelegate;

    property Started : Boolean read FClrStarted;

  end;

implementation
uses
  System.IOUtils;

{ TNetCoreClr }

constructor TNetCoreClr.Create(coreclrPath, appDomainFriendlyName: string);
begin
  FAppPaths := '';
  FClrStarted := false;
  FClrPath := coreclrPath;
  {$ifdef MSWINDOWS}
  FClrHandle := LoadLibraryExW(PChar(coreclrPath+'\coreclr.dll'), 0, 0);
  {$endif}
  {$ifdef MACOS}
  FClrHandle := LoadLibrary(PChar(coreclrPath+'/libcoreclr.dylib'));
  {$endif}
  FAppDomainFriendlyName := appDomainFriendlyName;
  if (FClrHandle>0) then
  begin
    @FInitializeCoreClr := GetProcAddress(FClrHandle, 'coreclr_initialize');
    @FCreateManagedDelegate := GetProcAddress(FClrHandle, 'coreclr_create_delegate');
    @FShutdownCoreClr := GetProcAddress(FClrHandle, 'coreclr_shutdown');

    BuildTpaList(coreclrPath, '.dll');
  end;

end;

procedure TNetCoreClr.AddAppPath(path: string);
begin
  FAppPaths := FAppPaths + path+PathSep;
end;

procedure TNetCoreClr.BuildTpaList(directory : string; extension : string);
var
  rec  : TSearchRec;
  search_path : string;
begin
  search_path := IncludeTrailingPathDelimiter(directory) + '*'+extension;
  if FindFirst(search_path, faAnyFile, rec) = 0 then
  begin
    repeat
      if not ((rec.Name = '..') or (rec.Name = '.')) then
      begin
        if (rec.Attr and faDirectory) = 0 then
        begin
          FTpaList := FTpaList + directory+PathDelim+rec.Name+PathSep;
        end;
      end;
    until FindNext(rec) <> 0;
    System.SysUtils.FindClose(rec);
  end;
end;


procedure TNetCoreClr.Start;
   var propertyKeys : array[0..1] of PAnsiChar;
   var propertyValues : array [0..1]of PAnsiChar;
begin
  if (FClrHandle>0) then
  begin
    propertyKeys[0] := PAnsiChar('TRUSTED_PLATFORM_ASSEMBLIES');      // Trusted assemblies
    propertyKeys[1] :=  PAnsiChar('APP_PATHS');

    propertyValues[0] := PAnsiChar(AnsiString(FTpaList));
    propertyValues[1] := PAnsiChar(AnsiString(FAppPaths));

    var hr := FInitializeCoreClr(
                    PAnsiChar(AnsiString(FClrPath)),        // App base path
                    PAnsiChar(AnsiString(FAppDomainFriendlyName)),       // AppDomain friendly name
                    2,   // Property count
                    @(propertyKeys[0]),       // Property names
                    @(propertyValues[0]),     // Property values
                    @FHostHandle,        // Host handle
                    @FDomainId);         // AppDomain ID


    FClrStarted :=  hr >= 0;

  end;

end;


function TNetCoreClr.CreateDelegate(AssemblyName, TypeName, MethodName: string): TDelegate;
begin
  result := nil;
  if (FClrStarted) then
  begin
     var funcptr : Pointer;

     var hr := FCreateManagedDelegate(
              FHostHandle,
              FDomainId,
              PAnsiChar(AnsiString(AssemblyName)),
              PAnsiChar(AnsiString(TypeName)),
              PAnsiChar(AnsiString(MethodName)),
              @funcptr);

    if (hr >= 0) then
    begin
      result := funcptr;
    end else begin
      result := nil;
    end;

  end;
end;

destructor TNetCoreClr.Destroy;
begin
  inherited;
  if (FClrStarted) then ShutDown;
//  if (FClrHandle > 0) then FreeLibrary(FClrHandle); //CoreCLR ライブラリをアンロードしてはいけないらしい
end;

class function TNetCoreClr.IsValidRuntimePath(Path: string): Boolean;
begin
  {$ifdef MSWINDOWS}
  result := FileExists(Path+'\coreclr.dll');
  {$endif}
  {$ifdef MACOS}
  result := FileExists(Path+'/libcoreclr.dylib');
  {$endif}

end;

procedure TNetCoreClr.ShutDown;
begin
  if (FClrStarted) then
  begin
     {var hr := }FShutdownCoreClr(FHostHandle, FDomainId);
     FClrStarted := false;
  end;
end;

end.
