unit NetCoreClr;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Winapi.ActiveX;

type
  THostHandle = Pointer;
  PHostHandle = ^THostHandle;
  TDelegate = Pointer;
  PDelegate = ^TDelegate;
  TCoreclr_initialize_ptr = function(exePath : PAnsiChar ; appDomainFriendlyName : PAnsiChar; propertyCount : Integer; propertyKeys : PPAnsiChar; propertyValues : PPAnsiChar ; hostHandle : PHostHandle; domainId : PDWORD ) : integer; stdcall;
  TCoreclr_shutdown_ptr = function(hostHandle : THostHandle; domainId : DWORD ) : integer; stdcall;
  TCoreclr_shutdown_2_ptr = function(hostHandle : THostHandle; domainId : DWORD; latchedExitCode : PInteger ) : integer; stdcall;
  TCoreclr_create_delegate_ptr = function(hostHandle : THostHandle; domainId : DWORD;  entryPointAssemblyName : PAnsiChar; entryPointTypeName : PAnsiChar; entryPointMethodName : PAnsiChar; delegate : PDelegate ) : integer; stdcall;
  TCoreclr_execute_assembly_ptr = function(hostHandle : THostHandle; domainId : DWORD;  argc : Integer; argv : PPAnsiChar; managedAssemblyPath : PAnsiChar; exitCode : PInteger ) : integer; stdcall;

  TNetCoreClr = class
  private
    FClrPath : string;
    FClrHandle : THandle;
    FHostHandle : THostHandle;
    FDomainID : DWORD;
    FAppDomainFriendlyName : string;
    FTpaList : string;
    FAppPaths : string;
    FClrStarted : Boolean;
    procedure BuildTpaList(directory, extension: string);
  protected

  public
    FInitializeCoreClr : TCoreclr_initialize_ptr;
    FCreateManagedDelegate : TCoreclr_create_delegate_ptr;
    FShutdownCoreClr : TCoreclr_shutdown_ptr;

    constructor Create(coreclrPath : string; appDomainFriendlyName : string);
    destructor  Destroy; override;

    procedure Start();
    procedure ShutDown();

    procedure AddAppPath(path : string);
    function CreateDelegate(AssemblyName : string; TypeName : string; MethodName : string) : TDelegate;

  end;

procedure ListUpDotnetRuntimes(var str : TStringList);

implementation
uses
  ShellApi, Forms, System.IOUtils;

{ Helper }
procedure ListUpDotnetRuntimes(var str : TStringList);
begin
{
  var temppath := TPath.GetTempPath;

  var cmd := 'dotnet.exe';
  var param := '--list-runtimes > '+temppath+'coreruntimes.txt';
  ShellExecute( 0, 'open', PChar(cmd), PChar(param), nil, SW_SHOWNORMAL);
  SysUtils.ExecuteProcess(
  }
end;

{ TNetCoreClr }

constructor TNetCoreClr.Create(coreclrPath, appDomainFriendlyName: string);
begin
  FAppPaths := '';
  FClrStarted := false;
  FClrPath := coreclrPath;
  FClrHandle := LoadLibraryExW(PChar(coreclrPath+'\coreclr.dll'), 0, 0);
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
  FAppPaths := FAppPaths + path+';';
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
          FTpaList := FTpaList + directory+'\'+rec.Name+';';
        end;
      end;
    until FindNext(rec) <> 0;
    FindClose(rec);
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

procedure TNetCoreClr.ShutDown;
begin
  if (FClrStarted) then
  begin
     var hr := FShutdownCoreClr(FHostHandle, FDomainId);
     FClrStarted := false;
  end;
end;

end.
