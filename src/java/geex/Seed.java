package geex;

import java.util.ArrayList;
import geex.Dependencies;
import geex.Referents;
import geex.Mode;
import geex.SeedFunction;
import geex.State;
import geex.StateCallback;
import clojure.lang.IPersistentMap;
import clojure.lang.APersistentMap;
import clojure.lang.IFn;

public interface Seed {
    static int UNDEFINED_ID = Integer.MIN_VALUE;

    public Object getType();
    public void setId(int id);
    public int getId();
    public Mode getMode();

    public SeedFunction getSeedFunction();

    public boolean equals(Object other);
    public int hashCode();

    public APersistentMap getRawDeps();

    public Dependencies deps();
    public Referents refs();

    // Compilation result
    public Object compile(State state, IFn cb);
    public void setCompilationResult(Object x);
    public Object getCompilationResult();

    // Extra data
    Object getData();
    void setData(Object o);
}
