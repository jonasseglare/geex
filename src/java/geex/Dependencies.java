package geex;

import java.util.HashMap;
import geex.Seed;
import java.lang.RuntimeException;

public class Dependencies {
    private HashMap<Object, Seed> _deps 
        = new java.util.HashMap<Object, Seed>();

    public void addDep(Object key, Seed val) {
        if (!SeedUtils.isRegistered(val)) {
            throw new RuntimeException("Seed must be registered before it can be added as a dependency.");
        }
        _deps.put(key, val);
    }
}
