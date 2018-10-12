package geex;

import geex.LocalVar;

public class LocalStruct {
    private Object _typeSignature = null;
    private LocalVar[] _flatVars = null;

    public LocalStruct(Object tpSig, LocalVar[] fvi) {
        _typeSignature = tpSig;
        _flatVars = fvi;
    }

    public Object getTypeSignature() {
        return _typeSignature;
    }

    public LocalVar[] getFlatVars() {
        return _flatVars;
    }
}
