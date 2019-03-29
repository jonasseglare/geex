package geex;

import clojure.lang.IFn;

public class StateSettings {
    public Object platform = null;
    public IFn forwardedFunction = null;
    public IFn closeScope = null;
    public IFn generateSeedSymbol = null;
    public IFn checkCompilationResult = null;

    void check() {
        if (platform == null) {
            throw new RuntimeException("No platform specified");
        }
        if (closeScope == null) {
            throw new RuntimeException(
                "No function to close the scope");
        }
        if (generateSeedSymbol == null) {
            throw new RuntimeException("No generate seed symbol");
        }
    }
}
