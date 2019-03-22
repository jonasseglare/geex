package geex;

import geex.Optional;

public class SeedState {

    public void setCompilationResult(Object o) {
        _key = null;
        _value = Optional.of(o);
        _bound = false;
    }

    public void listCompilationResult(Object k, Object v) {
        _key = Optional.of(k);
        _value = Optional.of(v);
        _bound = true;
    }

    public Object getCompilationResult() {
        return _bound? _key.get() : _value.get();
    }

    public Object getKey() {
        return _key.get();
    }

    public Object getValue() {
        return _value.get();
    }

    public boolean isBound() {
        return _bound;
    }

    private boolean _bound = false;
    private Optional<Object> _key = Optional.empty();
    private Optional<Object> _value = Optional.empty();
};
