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

    public Seed get(Object key) {
        return _deps.get(key);
    }

    public Seed[] toArray() {
        int n = _deps.size();
        Seed[] dst = new Seed[n];
        for (int i = 0; i < n; i++) {
            dst[i] = getOrError(new Long(i));
        }
        return dst;
    }

    public Object[] compilationResultsToArray() {
        int n = _deps.size();
        Object[] dst = new Object[n];
        for (int i = 0; i < n; i++) {
            dst[i] = getOrError(new Long(i)).getCompilationResult();
        }
        return dst;        
    }

    public Seed getOrError(Object key) {
        Seed result =  _deps.get(key);
        if (result == null) {
            throw new RuntimeException(
                "No dep at '" + key.toString() + "'");
        }
        return result;
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

    static String indent = "         ";

    public void disp() {
        if (!_deps.isEmpty()) {
            String dst = indent + "Deps on";
            Set set = _deps.entrySet();
            Iterator iterator = set.iterator();
            while(iterator.hasNext()) {
                Map.Entry mentry = (Map.Entry)iterator.next();
                Seed value = (Seed)mentry.getValue();
                dst += " " + value.getId();
            }
            System.out.println(dst);
        }
    }

    public HashMap<Object, Seed> getMap() {
        return _deps;
    }

    public HashMap<Object, Object> getCompilationResults() {
        HashMap<Object, Object> dst 
            = new HashMap<Object, Object>();
        Set set = _deps.entrySet();
        Iterator iterator = set.iterator();
        while(iterator.hasNext()) {
            Map.Entry mentry = (Map.Entry)iterator.next();
            Object key = mentry.getKey();
            Seed value = (Seed)mentry.getValue();
            dst.put(key, value.getCompilationResult());
        }
        return dst;
    }
}
