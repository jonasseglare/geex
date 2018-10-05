package geex;

import java.util.HashMap;
import java.util.Set;
import java.util.Map;
import java.util.Iterator;
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

    public void addReferentsFromId(int id) {
        Set set = _deps.entrySet();
        Iterator iterator = set.iterator();
        while(iterator.hasNext()) {
            Map.Entry mentry = (Map.Entry)iterator.next();
            Object key = mentry.getKey();
            Seed value = (Seed)mentry.getValue();
            value.refs().add(key, id);
        }
    }
}
