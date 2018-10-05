package geex;


import geex.Seed;
import geex.SeedUtils;
import java.util.ArrayList;
import geex.Dependencies;

public class DynamicSeed implements Seed {
    private SeedParameters _params = null;
    private int _id = -1;
    private Object _compilationResult = null;
    private Dependencies _deps = new Dependencies();
    private Object _data;

    public DynamicSeed(SeedParameters p) {
        _params = p;
        _id = p.id;
    }

    public Object getType() {
        return _params.type;
    }

    public int getId() {
        return _id;
    }

    public String toString() {
        return SeedUtils.toString(this);
    }

    public boolean equals(Object other) {
        if (other == null) {
            return false;
        } else if (other == this) {
            return true;
        } else if (other instanceof Seed) {
            return SeedUtils.equals(this, (Seed)other);
        }
        return false;
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
