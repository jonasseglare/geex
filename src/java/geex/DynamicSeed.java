package geex;


import geex.Seed;
import geex.SeedUtils;
import java.util.ArrayList;
import geex.Dependencies;
import java.lang.RuntimeException;

public class DynamicSeed implements Seed {
    private SeedParameters _params = null;
    private Object _compilationResult = null;
    private Dependencies _deps = new Dependencies();
    private Object _data;
    private int _id = Seed.UNDEFINED_ID;

    public DynamicSeed(SeedParameters p) {
        if (p.mode == null) {
            throw new RuntimeException(
                "Seed mode has not been defined");
        }
        _params = p;
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

    public void setCompilationResult(Object x) {
        _compilationResult = x;
    }

    public Object getCompilationResult() {
        return _compilationResult;
    }

    public Object getData() {
        return _data;
    }

    public void setData(Object o) {
        _data = o;
    }
}
