package geex;

import clojure.lang.IFn;

public class StateSettings {
    public Object platform = null;
    public IFn forwardedFunction = null;
    public IFn closeScope = null;

    void check() {
        if (platform == null) {
            throw new RuntimeException("No platform specified");
        }
        if (closeScope == null) {
            throw new RuntimeException(
                "No function to close the scope");
        }
    }
}
