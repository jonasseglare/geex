package geex;

import geex.Seed;
import java.util.Objects;
import geex.Mode;


public class SeedUtils {
    public static boolean equals(Seed a, Object other) {
        if (other == null) {
            return false;
        } else if (a == other) {
            return true;
        } else if (other instanceof Seed) {
            Seed b = (Seed)other;
            return a.getId() == b.getId() 
                && a.getType().equals(b.getType());            
        }
        return false;
    }

    public static String toString(Seed x) {
        Object tp = x.getType();
        return "Seed(type=" + ( tp == null? "nil" : tp.toString() )
            + ", id=" + x.getId() + ")";
    }

    public static int hashCode(Seed x) {
        return Objects.hash(x.getType(), x.getId());
    }

    public static boolean isRegistered(Seed x) {
        return x.getId() != Seed.UNDEFINED_ID;
    }

    public static int intFromMode(Mode m) {
        if (m == null) {
            return -1;
        } else if (m == Mode.Pure) {
            return 0;
        } else if (m == Mode.Ordered) {
            return 1;
        }
        return 2;
    }

    public static Mode modeFromInt(int m) {
        if (m == -1) {
            return null;
        } else if (m == 0) {
            return Mode.Pure;
        } else if (m == 1) {
            return Mode.Ordered;
        }
        return Mode.SideEffectful;
    }

    public static Mode max(Mode a, Mode b) {
        return modeFromInt(
            Math.max(intFromMode(a), intFromMode(b)));
    }
}
