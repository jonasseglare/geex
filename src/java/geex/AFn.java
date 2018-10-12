package geex;

import java.lang.RuntimeException;
import clojure.lang.IFn;
import clojure.lang.ISeq;

public class AFn implements IFn {


    public Object throwArity() {
        throw new RuntimeException("Wrong number of arguments to " 
            + toString());
    }

    public Object call() {
        return throwArity();
    }

    public void run(){
        throw new UnsupportedOperationException();
    }

    public Object invoke() {return throwArity();}

    public Object invoke(Object arg1) {return throwArity();}

    public Object invoke(Object arg1, Object arg2) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
        {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
        {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15, Object arg16) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15, Object arg16, Object arg17) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15, Object arg16, Object arg17, Object arg18) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
        {return throwArity();}

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20,
        Object... args)
        {return throwArity();}

    public Object applyTo(ISeq arglist) {return throwArity();}
}
