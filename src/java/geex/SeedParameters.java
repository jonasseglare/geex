package geex;

import geex.Mode;
import clojure.lang.IFn;

public class SeedParameters {
    public Object type = null;
    public Mode mode = null;
    String description = "";
    IFn compiler = null;
    Object data = null;
    Boolean bind = null;
}
