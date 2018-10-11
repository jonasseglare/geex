package geex;

import java.util.HashMap;
import java.util.Set;
import java.util.Iterator;
import java.util.Map;

public class Referents {
    HashMap<Object, Integer> 
        _keyReferentMap = new HashMap<Object, Integer>();

    public void add(Object key, int refId) {
        _keyReferentMap.put(key, refId);
    }

    static String indent = "         ";

    public void disp() {
        if (!_keyReferentMap.isEmpty()) {
            String dst = indent + "Refd by";
            Set set = _keyReferentMap.entrySet();
            Iterator iterator = set.iterator();
            while(iterator.hasNext()) {
                Map.Entry mentry = (Map.Entry)iterator.next();
                Integer value = (Integer)mentry.getValue();
                dst += " " + value;
            }
            System.out.println(dst);
        }
    }
}
