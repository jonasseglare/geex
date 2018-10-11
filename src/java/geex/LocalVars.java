package geex;

import java.util.ArrayList;

public class LocalVars {
    private int _counter = 0;
    ArrayList<Binding> _bindings;

    void addBinding(Seed seed) {
        Object result = seed.getCompilationResult();
        if (result == null) {
            throw new RuntimeException(
                "Seed " + seed.toString() 
                + " does not have a compilation result");
        }
        _bindings.add(new Binding(
                seed.generateVarName(),
                seed.getType(),
                result));
    }
}
