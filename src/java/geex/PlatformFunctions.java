package geex;

import java.util.ArrayList;

public interface PlatformFunctions {
    Object renderLocalVarName(String s);
    ISeed closeScope(ArrayList<ISeed> seeds);
}
