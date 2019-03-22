package geex;

import java.util.HashMap;
import java.util.Set;
import java.util.Map;
import java.util.Iterator;
import geex.ISeed;
import java.lang.RuntimeException;
import clojure.lang.PersistentVector;
import clojure.lang.Keyword;

public class Dependencies {
    private static Keyword depScopeKey 
        = Keyword.intern("depending-scope");

    private HashMap<Object, ISeed> _deps 
        = new java.util.HashMap<Object, ISeed>();

    public void addDep(Object key, ISeed val) {
        if (!SeedUtils.isRegistered(val)) {
            throw new RuntimeException("Seed must be registered before it can be added as a dependency.");
        }
        _deps.put(key, val);
    }

    public void addGenKey(ISeed val) {
        addDep(
            PersistentVector.create(
                depScopeKey, _deps.size()),
            val);
    }

    public void addCounted(ISeed val) {
        addDep(_deps.size(), val);
    }

    public ISeed get(Object key) {
        return _deps.get(key);
    }

    public ISeed[] toArray() {
        int n = _deps.size();
        ISeed[] dst = new ISeed[n];
        for (int i = 0; i < n; i++) {
            dst[i] = getOrError(Long.valueOf(i));
        }
        return dst;
    }

    public int countIndexedArgs() {
        int counter = 0;
        while (_deps.get(Long.valueOf(counter)) != null) {
            counter++;
        }
        return counter;
    }

    public Object[] compilationResultsToArray() {
        int n = countIndexedArgs();
        Object[] dst = new Object[n];
        for (int i = 0; i < n; i++) {
            dst[i] = getOrError(Long.valueOf(i))
                .getState().getCompilationResult();
        }
        return dst;        
    }

    public ISeed getOrError(Object key) {
        ISeed result =  _deps.get(key);
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
            ISeed value = (ISeed)mentry.getValue();
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
                ISeed value = (ISeed)mentry.getValue();
                dst += " " + value.getId();
            }
            System.out.println(dst);
        }
    }

    public HashMap<Object, ISeed> getMap() {
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
            ISeed value = (ISeed)mentry.getValue();
            dst.put(key, value.getState().getCompilationResult());
        }
        return dst;
    }
}
