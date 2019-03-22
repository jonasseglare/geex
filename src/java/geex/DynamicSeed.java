package geex;

import geex.ISeed;
import geex.SeedUtils;
import java.util.ArrayList;
import geex.Dependencies;
import java.lang.RuntimeException;
import clojure.lang.APersistentMap;
import geex.Mode;
import clojure.lang.IFn;
import geex.ForwardFn;
import geex.SeedState;

public class DynamicSeed extends ForwardFn implements ISeed {
    private SeedParameters _params = null;
    private Object _compilationResult = null;
    private Dependencies _deps = new Dependencies();
    private Referents _refs = new Referents();
    private int _id = ISeed.UNDEFINED_ID;
    private int _varCounter = 0;
    private boolean _hasResult = false;
    private SeedState _state = new SeedState();

    public DynamicSeed(SeedParameters p) {
        super("This seed (" + (p.description == null? "no desc" : p.description) 
            + ") cannot be used as a function");
        SeedUtils.checkSeedType(p.type);

        if (p.description == null) {
            throw new RuntimeException("Missing description");
        }
        if (p.compiler == null) {
            throw new RuntimeException("Missing compiler");
        }
        if (p.mode == null) {
            throw new RuntimeException(
                "Seed mode has not been defined");
        }
        _params = p;

        if (p.callable != null) {
            this.setForwardedFunction(p.callable);
        }
    }

    public APersistentMap getRawDeps() {
        return _params.rawDeps;
    }

    public Mode getMode() {
        return _params.mode;
    }

    public boolean hasValue() {
        return _params.hasValue;
    }

    public String getDescription() {
        return _params.description;
    }

    public Object getType() {
        return _params.type;
    }

    public void setId(int id) {
        _id = id;
    }

    public int getId() {
        return _id;
    }

    public String toString() {
        return SeedUtils.toString(this);
    }

    public boolean equals(Object other) {
        return SeedUtils.equals(this, other);
    }

    public int hashCode() {
        return SeedUtils.hashCode(this);
    }

    public Dependencies deps() {
        return _deps;
    }

    public Referents refs() {
        return _refs;
    }

    public SeedState getState() {
        return _state;
    }

    public Object compile(State state) {
        try {
            return _params.compiler.invoke(state, this);
        } catch (Exception e) {
            System.out.println(
                "Failed to compile " + toString());
            throw e;
        }
    }

    public String generateVarName() {
        _varCounter++;
        return _varCounter == 1? 
            String.format("s%04d", _id)
            : String.format("s%04d_%02d", _id, _varCounter);
    }

    public Boolean shouldBind() {
        return _params.bind;
    }

    public void setBind(Boolean value) {
        _params.bind = value;
    }


    public Object getData() {
        return _params.data;
    }

    public void setData(Object o) {
        _params.data = o;
    }

    public SeedParameters getParams() {
        return _params;
    }
}
