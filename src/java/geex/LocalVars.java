package geex;

import java.util.ArrayList;

public class LocalVars {
    private int _counter = 0;
    ArrayList<Binding> _bindings;

    Binding addBinding(Seed seed) {
        Object result = seed.getCompilationResult();
        if (result == null) {
            throw new RuntimeException(
                "Seed " + seed.toString() 
                + " does not have a compilation result");
        }
        Binding b = new Binding(
            seed.generateVarName(),
            seed.getType(),
            result);
        _bindings.add(b);
        return b;
    }
}
