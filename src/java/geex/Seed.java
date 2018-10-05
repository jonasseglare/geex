package geex;

import java.util.ArrayList;
import geex.Dependencies;

public interface Seed {
    public Object getType();
    public int getId();

    public boolean equals(Object other);
    public int hashCode();

    // Dependencies
    public Dependencies deps();

    // Compilation result
    public void setCompilationResult(Object x);
    public Object getCompilationResult();
}
