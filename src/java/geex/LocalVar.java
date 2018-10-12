package geex;

import geex.Optional;

public class LocalVar {

    private int _index = -1;
    private Optional<Object> _type = Optional.empty();
    
    public LocalVar(int i) {
        _index = i;
    }

    public int getIndex() {
        return _index;
    }

    public Optional<Object> getType() {
        return _type;
    }

    public void setType(Object tp) {
        if (_type.isPresent()) {
            if (!(_type.get() == tp)) {
                throw new RuntimeException(
                    "Trying to set type of lvar " + _index 
                    + " that currently has type " + _type.get()
                    + " to have type " + tp);
            }
        } else {
            _type = Optional.of(tp);
        }
    }
}
