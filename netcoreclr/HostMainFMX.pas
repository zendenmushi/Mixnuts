unit HostMainFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, NetCoreClr,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.ListBox, FMX.Layouts;

type
  TFormHostMain = class(TForm)
    CallButton: TButton;
    StartButton: TButton;
    Label1: TLabel;
    RuntimeComboBox: TComboBox;
    ListBox1: TListBox;
    procedure StartButtonClick(Sender: TObject);
    procedure CallButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private 宣言 }
    FNetCoreClr : TNetCoreClr;
    procedure ListUpDotnetRuntimes(runtimes: TStringList);
  public
    { public 宣言 }
  end;

var
  FormHostMain: TFormHostMain;

implementation
uses
  System.IOUtils
  {$ifdef MSWINDOWS}
  ,ShellApi, WinApi.Windows
  {$endif}
  {$ifdef MACOS}
  ,Posix.stdlib
  {$endif}
  ;

{$R *.fmx}

type
  TCallBack = procedure(msg : PChar); stdcall;
  TClientEntry = procedure (AppPath : PChar; funcptr : TCallBack);

procedure HostShowMessage(msg : PChar); stdcall;
begin
  FormHostMain.ListBox1.Items.Add(string(msg));
end;

procedure TFormHostMain.FormCreate(Sender: TObject);
begin
  var runtimes := TStringList.Create;
  try
    ListUpDotnetRuntimes(runtimes);
    RuntimeComboBox.Items.Clear;
    var cnt := runtimes.Count;
    if cnt > 0 then
    begin
      for var i := 0 to cnt-1 do
      begin
        var s := runtimes.ValueFromIndex[i];
        RuntimeComboBox.Items.Add(s);
      end;
      RuntimeComboBox.ItemIndex := cnt-1;
    end;
  finally
    runtimes.Free;
  end;
end;

procedure TFormHostMain.ListUpDotnetRuntimes(runtimes : TStringList);
begin
  var appdatapath := TPath.GetHomePath() + PathDelim +'HostSample'+PathDelim;
  if not DirectoryExists(appdatapath) then
  begin
    ForceDirectories(appdatapath);
  end;

  // インストールされているNET Coreランタイムのリストを取得
  // パイプやstdoutから出力を得ても良いと思うが、お手軽にホームにフォルダを作ってリダイレクト出力している
  {$ifdef MSWINDOWS}
  var cmd := 'cmd.exe';
  var param := '/S /C "dotnet --list-runtimes > "'+appdatapath+'coreruntimes.txt""';
  ShellExecute( 0, 'open', PChar(cmd), PChar(param), nil, SW_HIDE);
  {$endif}
  {$ifdef MACOS}
  var cmd := 'dotnet --list-runtimes > "'+appdatapath+'coreruntimes.txt"';
  var m : TMarshaller;
  _system(m.AsAnsi(cmd).ToPointer);
  {$endif}

  var str := TStringList.Create;
  try
    str.LoadFromFile(appdatapath+'coreruntimes.txt');
    for var s in str do
    begin
      var key := 'Microsoft.NETCore.App';
      if Pos(key, s) > 0 then
      begin
        var keylen := Length(key);
        var slen := Length(s);
        var bpos := Pos('[',s);
        var ver := Trim( Copy(s, keylen+1, bpos-keylen-1) );
        var path := Copy(s, bpos+1, slen-bpos-1);
        runtimes.Add(ver+'='+path+PathDelim+ver);

      end;
    end;

    runtimes.Sort();
  finally
    str.Free;
  end;

end;

procedure TFormHostMain.CallButtonClick(Sender: TObject);
begin
  if (FNetCoreClr <> nil) and FNetCoreClr.Started then
  begin
    // 対象DLLのアセンブリ名＋バージョン、呼び出す関数の属するクラス名、呼び出す関数名を渡して、関数ポインタを得る
    var entry := TClientEntry(FNetCoreClr.CreateDelegate('netcore_client, Version=1.0.0', 'netcore_client.Class1', 'Entry'));
    // 呼出し
    entry(PChar(ParamStr(0)), HostShowMessage);
  end;

end;

procedure TFormHostMain.StartButtonClick(Sender: TObject);
begin
  if RuntimeComboBox.ItemIndex > -1 then
  begin
    RuntimeComboBox.Enabled := false;
    //edtRuntimePath.Text := ExtractFilePath(OpenDialog1.FileName);
    var runtimepath := RuntimeComboBox.Items[ RuntimeComboBox.ItemIndex ];

    if TNetCoreClr.IsValidRuntimePath(runtimepath) then
    begin
      // clr作成
      FNetCoreClr := TNetCoreClr.Create(runtimepath,'HostDemo');
      // 対象DLLへのパスを追加する
  {$ifdef MSWINDOWS}
      // Windowsの時はプロジェクトフォルダと同じ位置にexeを出力。そこからの相対パスでクライアント側dllの出力位置までパスを通す
      var libpath := ExtractFilePath(ParamStr(0))+'netcore_client\bin\Debug\netstandard2.0';
  {$endif}
  {$ifdef MACOS}
      // Macosの時はアプリケーションバンドルのContents/lib内にクライアント側dllを配置し、そこまでのパスを通す
      var libpath := ExtractFilePath(ParamStr(0))+'../lib/netstandard2.0';
  {$endif}
      if DirectoryExists(libpath) then
      begin
        // dllの検索パスをフルパスで与える。複数のdllがある場合は複数回AddAppPathすることが可能
        FNetCoreClr.AddAppPath(ExpandFileName( libpath ));
        //clr起動
        FNetCoreClr.Start;
      end;

      StartButton.Enabled := false;
      CallButton.Enabled := true;
    end;
  end;
end;

end.
