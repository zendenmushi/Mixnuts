DelphiからNET Core Clrを起動してC#で作成したDLLを呼び出すサンプル

C#で作成したDLLをDelphiから呼び出したいケースは以下の2通りがあると思います

(1） .NET Framework向けDLL を呼び出したい
  
  DllExportを使う方法が一番楽。ここでは紹介しない。
  
  ※はまりポイント：ホストEXEとクライアントDLLが同じフォルダに無い場合、ホストEXE.configファイルを使ってパス指定が必要。
  
(2) .NET Core向けDLL を呼び出したい

 参照：  https://docs.microsoft.com/ja-jp/dotnet/core/tutorials/netcore-hosting

 上記ページで3通りの方法が紹介されている。
 .NET Core 3.X以降はNetHost.h と HostFxr.h を使用してホストを作成する方法が一番よさそうだけど、DLL内関数を呼び出す際に引数を構造体で渡す必要があるようなので少々気持ち悪い。
 
  二番目の方法は、coreclr.dllの位置を自力で解決するか、ランタイムを自アプリ下のフォルダに取り込む等する必要があるけど、DLL内関数のポインタが得られた後は通常DLL内関数と同じ感覚で使えるメリットあり。
  
  三番目の方法は古い方法らしいので無視。
  
  このサンプルでは二番目のcorelr.dllを起動する方法をDelphiで行う。
  
 以下略。あとはソースで
 
 ホスト側はHostSample.dproj。サンプルはDelphi 10.3 Rioで作成。Win64のみテストしています。その他プラットフォーム対応の場合はLoadLibrary部分の変更が必要
 クライアント側はnetcore_client/netcore_client.csproj。サンプルはVisualStudioCodeとNET Core3 SDKで作成。
