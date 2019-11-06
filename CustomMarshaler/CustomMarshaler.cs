/*
           DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
                   Version 2, December 2004
 
Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>

Everyone is permitted to copy and distribute verbatim or modified
copies of this license document, and changing it is allowed as long
as the name is changed.
 
           DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
  TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
 
 0. You just DO WHAT THE FUCK YOU WANT TO.  */

// Author 2019 TMaeda  on WTFPL（Do What The Fuck You Want To Public License）

using System;
using System.Runtime.InteropServices;


namespace Sample
{
    
    // refer:https://docs.microsoft.com/ja-jp/dotnet/api/system.runtime.interopservices.icustommarshaler?view=netframework-4.8
    // delphiのstring（UnicodeString)のvar引数や戻り値（隠し第一パラメータ）をref/outで受け取るためのMarshaler。
    public class TDelphiStringMarshaler : ICustomMarshaler
    {
        public class UnsafeNativeMethods
        {
        #if USE_HOSTING_CORECLR

            public delegate void TDelphiStringClone([MarshalAs(UnmanagedType.LPWStr)] string source, out IntPtr dest);
            public delegate void TDelphiStringRelease(IntPtr ptr);
            
            internal static TDelphiStringClone DelphiStringClone = null;
            internal static TDelphiStringRelease DelphiStringRelease = null;

            public static void Initialize(IntPtr func_table, int func_count)
            {
                var funcptrs = new IntPtr[func_count];
                Marshal.Copy(func_table, funcptrs, 0, funcptrs.Length);

                if (func_count > 0) DelphiStringClone = Marshal.GetDelegateForFunctionPointer<TDelphiStringClone>(funcptrs[0]);
                if (func_count > 1) DelphiStringRelease = Marshal.GetDelegateForFunctionPointer<TDelphiStringRelease>(funcptrs[1]);
                
            }
        #else
            [DllImport("XXXX.dll")]
            internal static extern void DelphiStringClone([MarshalAs(UnmanagedType.LPWStr)] string source, out IntPtr dest);
            [DllImport("XXXX.dll")]
            internal static extern void DelphiStringRelease(IntPtr ptr);
        #endif
        }
        
        unsafe public Object MarshalNativeToManaged( IntPtr pNativeData )
        {
            return new string((char*)pNativeData);
        }

        public IntPtr MarshalManagedToNative( Object ManagedObj )
        {
            IntPtr NativeData = IntPtr.Zero;
            if (ManagedObj is string) {
                var s = (string)ManagedObj;
                UnsafeNativeMethods.DelphiStringClone(s, out NativeData);
            }
            return NativeData;
        }
        public void CleanUpNativeData( IntPtr pNativeData )
        {
            UnsafeNativeMethods.DelphiStringRelease(pNativeData);    
        }
        public void CleanUpManagedData( Object ManagedObj )
        {

        }
        public int GetNativeDataSize()
        {
            return -1;
        }

        private static TDelphiStringMarshaler instance = null;
        public static ICustomMarshaler GetInstance(string Cookie)
        {
            if (instance == null) {
                instance = new TDelphiStringMarshaler();
            }
            return instance;
        }

    }
    
/*
● Delphi側では公開したいメソッドの他に以下の二つの手続きも公開してください
      
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



●使い方

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

*/
}
