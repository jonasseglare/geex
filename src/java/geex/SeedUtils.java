package geex;

import geex.Seed;
import java.util.Objects;


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
        return x.getId() != -1;
    }
}
