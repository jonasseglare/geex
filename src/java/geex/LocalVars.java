package geex;

import java.util.ArrayList;

public class LocalVars {
    private int _counter = 0;
    ArrayList<Binding> _bindings = new ArrayList<Binding>();

    public void clear() {
        _bindings = new ArrayList<Binding>();
    }

    public ArrayList<Binding> bindings() {
        return _bindings;
    }

    public Binding addBinding(Seed seed) {
        System.out.println("A");
        Object result = seed.getCompilationResult();
        System.out.println("B");
        if (result == null) {
            throw new RuntimeException(
                "Seed " + seed.toString() 
                + " does not have a compilation result");
        }
        System.out.println("C");
        Binding b = new Binding(
            seed.generateVarName(),
            seed.getType(),
            result);
        _bindings.add(b);
        return b;
    }
}
