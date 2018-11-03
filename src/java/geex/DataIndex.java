package geex;

import java.util.HashMap;

public class DataIndex {
    private HashMap<Object, Integer> _indices 
        = new HashMap<Object, Integer>();
    
    public int get(Object x) {
        if (_indices.containsKey(x)) {
            return _indices.get(x);
        } else {
            int i = _indices.size();
            _indices.put(x, i);
            return i;
        }
    }
}
