package geex;

import java.util.ArrayList;
import geex.Dependencies;
import geex.Referents;
import geex.Mode;
import geex.State;
import clojure.lang.IPersistentMap;
import clojure.lang.APersistentMap;
import clojure.lang.IFn;

public interface ISeed {
    static int UNDEFINED_ID = Integer.MIN_VALUE;

    public Object getType();
    public void setId(int id);
    public int getId();
    public Mode getMode();
    public boolean hasValue();
    public String getDescription();

    public boolean equals(Object other);
    public int hashCode();

    public APersistentMap getRawDeps();

    public Dependencies deps();
    public Referents refs();

    public SeedState getState();

    // Local vars
    public String generateVarName();

    public Object compile(State s);

    public Boolean shouldBind();
    public void setBind(Boolean value);

    // Used to make it callable.
    public void setForwardedFunction(IFn f);

    // Extra data
    Object getData();
    void setData(Object o);
}
