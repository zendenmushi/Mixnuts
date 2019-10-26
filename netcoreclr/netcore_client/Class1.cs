using System;
using System.Runtime.InteropServices;

namespace netcore_client
{
    [UnmanagedFunctionPointer(CallingConvention.StdCall)]
    public delegate void THostShowMessage([MarshalAs(UnmanagedType.LPWStr)]string msg);  
    public class Class1
    {
        // delphi側から呼び出される関数。例として呼び出し元のパスと、コールバック関数ポインタが渡される
        public static void Entry([MarshalAs(UnmanagedType.LPWStr)] string AppPath, IntPtr funcptr)
        {
            string msg = "call from "+AppPath;
            var callback = (THostShowMessage)Marshal.GetDelegateForFunctionPointer(funcptr, typeof(THostShowMessage));

            // 文字列を加工して、コールバック
            callback(msg);

        }

    }
}
