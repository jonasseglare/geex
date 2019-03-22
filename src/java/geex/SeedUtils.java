package geex;

import geex.ISeed;
import java.util.Objects;
import geex.Mode;


public class SeedUtils {
    public static void checkSeedType(Object o) {
        if (o instanceof ISeed) {
            throw new RuntimeException(
                "A seed having a seed as type is likely an error");
        }
    }
    
    public static boolean equalTypes(Object a, Object b) {
        if (a == null) {
            return b == null;
        } else if (b == null) {
            return false;
        }
        return a.equals(b);
    }

    public static boolean equals(ISeed a, Object other) {
        if (other == null) {
            return a == null;
        } else if (a == null) {
            return false;
        } else if (a == other) {
            return true;
        } else if (other instanceof ISeed) {
            ISeed b = (ISeed)other;
            return a.getId() == b.getId() 
                && equalTypes(a.getType(), b.getType());            
        }
        return false;
    }

    public static String toString(ISeed x) {
        Object tp = x.getType();
        int id = x.getId();
        return "ISeed(type=" + ( tp == null? "nil" : tp.toString() )
            + (id == ISeed.UNDEFINED_ID? "" : (", id=" + id))
                + ", desc=" + x.getDescription() + ")";
    }

    public static int hashCode(ISeed x) {
        return Objects.hash(x.getType(), x.getId());
    }

    public static boolean isRegistered(ISeed x) {
        return x.getId() != ISeed.UNDEFINED_ID;
    }

    public static int intFromMode(Mode m) {
        if (m == null) {
            throw new RuntimeException(
                "The mode must not be null");
        } if (m == Mode.Pure) {
            return 0;
        } else if (m == Mode.Ordered) {
            return 1;
        } else {
            return 2;
        }
    }

    public static Mode modeFromInt(int m) {
        if (m == 0) {
            return Mode.Pure;
        } else if (m == 1) {
            return Mode.Ordered;
        } else if (m == 2) {
            return Mode.SideEffectful;
        } else {
            throw new RuntimeException(
                "Cannot map " + m + " to mode");
        }
    }

    public static Mode max(Mode a, Mode b) {
        return modeFromInt(
            Math.max(intFromMode(a), intFromMode(b)));
    }

    public static boolean hasCompilationResult(ISeed x) {
        return x.hasCompilationResult();
    }
}
