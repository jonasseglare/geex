package geex;

import geex.Mode;
import clojure.lang.IFn;
import clojure.lang.APersistentMap;
import geex.SeedFunction;

public class SeedParameters {
    public Object type = null;
    public Mode mode = null;
    public String description = null;
    public IFn compiler = null;
    public Object data = null;
    public Boolean bind = null;
    public APersistentMap rawDeps = null;
    public SeedFunction seedFunction = null;
}
