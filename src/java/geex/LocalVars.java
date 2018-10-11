package geex;

import geex.LocalVar;
import java.util.ArrayList;

public class LocalVars {
    private ArrayList<LocalVar> _vars = new ArrayList<LocalVar>();

    public LocalVar declare() {
        int index = _vars.size();
        LocalVar lvar = new LocalVar(index);
        _vars.add(lvar);
        return lvar;
    }

    public LocalVar get(int i) {
        if (0 <= i && i < _vars.size()) {
            return _vars.get(i);
        }
        throw new RuntimeException(
            "Local variable index out of bounds " + i);
    }
}
