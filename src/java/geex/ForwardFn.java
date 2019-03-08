package geex;

import clojure.lang.IFn;
import clojure.lang.ISeq;

public class ForwardFn implements IFn {
    private String _error_message;
    private IFn _f;

    public void setForwardedFunction(IFn f) {
        _f = f;
    }

    public ForwardFn(String em) {
        _error_message = em;
    }

    public ForwardFn(IFn f) {
        _f = f;
    }

    private IFn f() {
        if (_f == null) {
            throw new RuntimeException(_error_message);
        }
        return _f;
    }

    public Object call() throws Exception {
        return f().invoke(this);
    }

    public void run() {
        f().invoke(this);
    }

    public Object invoke() {
        return f().invoke(this);
    }

    public Object invoke(Object arg1) {
        return f().invoke(this, arg1);
    }

    public Object invoke(Object arg1, Object arg2) {
        return f().invoke(this, arg1, arg2);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3) {
        return f().invoke(this, arg1, arg2, arg3);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {
        return f().invoke(this, arg1, arg2, arg3, arg4);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) {
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
		{
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8) {
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9) {
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10) {
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11) {
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) {
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
		{
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
		{
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15) {
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15, Object arg16) {
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15, Object arg16, Object arg17) {
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15, Object arg16, Object arg17, Object arg18) {
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) {
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20) {
        return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
    }


    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
        Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
        Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20,
        Object... args) {
        throw new RuntimeException("This arity is not implemented");
        //return f().invoke(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, args);
    }

    public Object applyTo(ISeq arglist) {
        return f().applyTo(arglist.cons(this));
    }
}
