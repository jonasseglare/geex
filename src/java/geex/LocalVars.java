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
}
