package geex;

import java.util.HashMap;

public class Referents {
    HashMap<Object, Integer> 
        _keyReferentMap = new HashMap<Object, Integer>();

    public void add(Object key, int refId) {
        _keyReferentMap.put(key, refId);
    }
}
