package geex;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Objects;

public class Referents {
    class Item {
        Object key;
        Integer ref;

        public String toString() {
            return ref.toString();
        }

        public Item(Object k, Integer r) {
            key = k;
            ref = r;
        }

        public boolean equals(Object other) {
            if (other == null) {
                return false;
            }
            if (!(other instanceof Item)) {
                return false;
            }
            Item y = (Item)other;
            return key.equals(y.key) && ref.equals(y.ref);
        }

        public int hashCode() {
            return Objects.hash(key, ref);
        }
    };

    HashSet<Item> _set = new HashSet<Item>();

    public void add(Object key, int refId) {
        _set.add(new Item(key, refId));
    }

    static String indent = "         ";

    public void disp() {
        if (!_set.isEmpty()) {
            String dst = indent + "Refd by";
            Iterator iter = _set.iterator();
            while (iter.hasNext()) {
                dst += " " + iter.next();
            }
            System.out.println(dst);
        }
    }

    public int count() {
        return _set.size();
    }
}
