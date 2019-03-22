package geex;

import geex.Optional;

public class SeedState {

    public void setCompilationResult(Object o) {
        _key = null;
        _value = Optional.of(o);
        _listed = false;
    }

    public void listCompilationResult(Object k, Object v) {
        _key = Optional.of(k);
        _value = Optional.of(v);
        _listed = true;
    }

    public Object getCompilationResult() {
        return _listed? _key.get() : _value.get();
    }

    public Object getKey() {
        return _key.get();
    }

    public Object getValue() {
        return _value.get();
    }

    public boolean isListed() {
        return _listed;
    }

    private boolean _listed = false;
    private Optional<Object> _key = Optional.empty();
    private Optional<Object> _value = Optional.empty();
};
