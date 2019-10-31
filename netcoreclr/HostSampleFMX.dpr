program HostSampleFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  HostMainFMX in 'HostMainFMX.pas' {FormHostMain},
  NetCoreClr in 'NetCoreClr.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormHostMain, FormHostMain);
  Application.Run;
end.
