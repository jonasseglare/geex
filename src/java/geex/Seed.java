package geex;

import java.util.ArrayList;
import geex.Dependencies;
import clojure.lang.IPersistentMap;
import clojure.lang.APersistentMap;

public interface Seed {
    static int UNDEFINED_ID = Integer.MIN_VALUE;

    public Object getType();
    public void setId(int id);
    public int getId();

    public boolean equals(Object other);
    public int hashCode();

    public APersistentMap getRawDeps();

    // Dependencies
    public Dependencies deps();

    // Compilation result
    public void setCompilationResult(Object x);
    public Object getCompilationResult();

    // Extra data
    Object getData();
    void setData(Object o);
}
