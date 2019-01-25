package geex;

import java.lang.RuntimeException;
import geex.ISeed;
import geex.Dependencies;
import geex.SeedUtils;
import geex.Mode;
import clojure.lang.IFn;
import clojure.lang.APersistentMap;
import geex.SeedFunction;

public class TypedSeed implements ISeed {
    Object _type = null;

    public APersistentMap getRawDeps() {
        throw new RuntimeException("TypedSeed has no raw deps");
    }

    public TypedSeed(Object type) {
        SeedUtils.checkSeedType(type);
        _type = type;
    }

    public String getDescription() {
        return "TypedSeed";
    }

    public Object getType() {
        return _type;
    }

    public int getId() {
        return ISeed.UNDEFINED_ID;
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

    public boolean hasCompilationResult() {
        throw new RuntimeException(
            "TypedSeed not compilable");
    }

    public Object getCompilationResult() {
        throw new RuntimeException(
            "Cannot get compilation result of typed seed");
    }

    public Object compile(State state, IFn cb) {
        throw new RuntimeException("Cannot compile a TypedSeed");
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

    public Boolean shouldBind() {
        throw new RuntimeException("Not bindable");
    }

    public void setBind(Boolean v) {
        throw new RuntimeException("Not bindable");
    }

    public String generateVarName() {
        throw new RuntimeException(
            "Cannot generateVarName for typed seed");
    }

    public String toString() {
        return SeedUtils.toString(this);
    }
}
