package geex;

import java.lang.RuntimeException;
import geex.Seed;
import geex.Dependencies;
import geex.SeedUtils;
import geex.Mode;
import clojure.lang.APersistentMap;
import geex.SeedFunction;

public class TypedSeed implements Seed {
    Object _type = null;

    public APersistentMap getRawDeps() {
        throw new RuntimeException("TypedSeed has no raw deps");
    }

    public TypedSeed(Object type) {
        _type = type;
    }

    public Object getType() {
        return _type;
    }

    public int getId() {
        return Seed.UNDEFINED_ID;
    }

    public void setId(int id) {
        throw new RuntimeException("Cannot set id of TypedSeed");
    }

    public Mode getMode() {
        return Mode.Pure;
    }

    public boolean equals(Object other) {
        return SeedUtils.equals(this, other);
    }

    public int hashCode() {
        return SeedUtils.hashCode(this);
    }

    public Dependencies deps() {
        throw new RuntimeException(
            "A typed seed cannot have dependencies");
    }

    public Referents refs() {
        throw new RuntimeException(
            "A typed seed cannot have referents");
    }

    public void setCompilationResult(Object x) {
        throw new RuntimeException(
            "Cannot set compilation result of typed seed");
    }

    public Object getCompilationResult() {
        throw new RuntimeException(
            "Cannot get compilation result of typed seed");
    }

    public Object getData() {
        throw new RuntimeException("Cannot get data of typed seed");
    }

    public void setData(Object o) {
        throw new RuntimeException(
            "Cannot set data of runtime exception");
    }

    public SeedFunction getSeedFunction() {
        return null;
    }
}
