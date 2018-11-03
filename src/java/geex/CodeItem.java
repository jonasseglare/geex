package geex;

import clojure.lang.AFn;

public class CodeItem {
    private Object _key = null;
    private AFn _build = null;
    private Object _code = null;
    private Object[] _deps = null;
    private boolean _built = false;

    public CodeItem(
        Object key, 
        AFn build,
        Object[] deps) {
        _key = key;
        _build = build;
        _deps = deps;
    }

    public Object getCode() {
        if (!_built) {
            _code = _build.invoke();
            _built = true;
        }
        return _code;
    }

    public Object getKey() {
        return _key;
    }
    
};
