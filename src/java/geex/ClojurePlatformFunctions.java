package geex;

import clojure.lang.Symbol;
import geex.PlatformFunctions;

public class ClojurePlatformFunctions 
    implements PlatformFunctions {
    public Object renderLocalVarName(String s) {
        return Symbol.create(s);
    }
}
