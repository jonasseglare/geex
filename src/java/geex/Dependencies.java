package geex;

import java.util.HashMap;
import geex.Seed;
import java.lang.RuntimeException;
import clojure.lang.PersistentVector;
import clojure.lang.Keyword;

public class Dependencies {
    private static Keyword depScopeKey 
        = Keyword.intern("depending-scope");

    private HashMap<Object, Seed> _deps 
        = new java.util.HashMap<Object, Seed>();

    public void addDep(Object key, Seed val) {
        if (!SeedUtils.isRegistered(val)) {
            throw new RuntimeException("Seed must be registered before it can be added as a dependency.");
        }
        _deps.put(key, val);
    }

    public void addGenKey(Seed val) {
        addDep(
            PersistentVector.create(
                depScopeKey, _deps.size()),
            val);
    }
}
