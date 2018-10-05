package geex;

import java.util.HashMap;
import geex.Seed;

public class Dependencies {
    private HashMap<Object, Seed> _deps 
        = new java.util.HashMap<Object, Seed>();

    public void addDep(Object key, Seed val) {
        _deps.put(key, val);
    }
}
