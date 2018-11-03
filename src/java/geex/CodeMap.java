package geex;

import java.util.HashMap;
import java.util.Map;
import java.util.Iterator;
import geex.CodeItem;

public class CodeMap {
    private HashMap<Object, CodeItem> _items 
        = new HashMap<Object, CodeItem>();
    
    public void add(CodeItem x) {
        _items.put(x.getKey(), x);
    }

    public Object[] getUnorderedCode() {
        int n = _items.size();
        Object[] dst = new Object[n];
        int i = 0;
        Iterator<Map.Entry<Object, CodeItem>> entries 
            = _items.entrySet().iterator();
        while (entries.hasNext()) {
            Map.Entry<Object, CodeItem> entry = entries.next();
            dst[i++] = entry.getValue().getCode();
        }
        return dst;
    }

    public void mergeInPlace(CodeMap other) {
        Iterator<Map.Entry<Object, CodeItem>> entries 
            = other._items.entrySet().iterator();
        while (entries.hasNext()) {
            Map.Entry<Object, CodeItem> e = entries.next();
            _items.put(e.getKey(), e.getValue());
        }
    }
}
