CustomMarshaler for Delphi (Unicode)String
==========================================


##  Delphi側では公開したいメソッドの他に以下の二つの手続きも公開してください  
  
// Delphiメソッドの引数へstringをC#から参照渡しするために、C#文字列からDelphi管理の文字列を作成する  
procedure DelphiStringClone(source : string; var dest : Pointer); stdcall;  
begin  
  var clone := Copy(source,1,Length(source));  
  dest := Pointer(PChar(clone));  
  var destaddress := PByte(dest);  
  
// 参照カウントを+1しておくことで、この関数から抜けた時に解放されないようにする  
  Dec(destaddress,8);  
  var refcount := PUInt32(destaddress)^;  
  if refcount <> $ffffffff then // 空文字列だとリテラル扱い(参照カウント-1)になるので操作しない  
  begin  
    PUInt32(destaddress)^ := refcount + 1; // 参照カウントを+1しておく。  
  end;  
end;  
  
// DelphiStringCloneで生成した変数の参照カウントを-1して解放する  
// DelphiStringCloneで生成した変数が呼び出したDelphiメソッドの中で書き換わった場合、その時点で参照カウントが-1されるのでこの関数を呼び出す必要はないが  
// その時、C#側のTDelphiStringMarshaler のCleanUpNativeData()には DelphiStringCloneで渡したポインタとは異なる値(書き換え後のstring変数のアドレス=s2)が入っているので  
// s2 に対して、やはりDelphiStringReleaseを呼び出す必要がある(そのままではs2が解放されないため)  
// 結論としてはのCleanUpNativeDataで受け取ったポインタに対して無条件でDelphiStringReleaseを呼び出せばよい。  
procedure DelphiStringRelease(ptr : Pointer); stdcall;  
var  
  originstring : string;  
  strptr : Pointer absolute originstring;  
begin  
  strptr := ptr;  // C#から受け取ったポインタをabsolute指定でstringとみなす。stringはこの関数を抜ける時点で参照カウントが-1される  
end;  
  
## 使い方  
  
下記のようなメソッドをC#側から呼び出したい場合  
  
function sample_method(digit : integer; var msg : string) : string;  
begin  
  result := IntToStr(digit*4)+msg;  
end;  
  
以下のようにMarshalingを記述する  
  
  public delegate void TSampleMethod([MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(TDelphiStringMarshaler))]ref string result, int digit, [MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(TDelphiStringMarshaler))]ref string msg);  
もしくは  
  [DllImport("XXXX.dll")]  
  public static extern void SampleMethod([MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(TDelphiStringMarshaler))]ref string result, int digit, [MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(TDelphiStringMarshaler))]ref string msg);  
  
※string等のDelphiが管理する型を戻り値とする関数は、内部的にはvarな隠し第一パラメータとなる。(オブジェクトのメソッドの時は第二パラメータ)  
