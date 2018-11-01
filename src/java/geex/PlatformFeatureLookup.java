package geex;

import java.util.ArrayList;

public class PlatformFeatureLookup {
    ArrayList<ArrayList<Object>> _data = new ArrayList<ArrayList<Object>>();

    public void setFeature(
        int platformIndex, int featureIndex, 
        Object value) {
        _data.ensureCapacity(platformIndex+1);
        if (_data.get(platformIndex) == null) {
            _data.set(platformIndex, new ArrayList<Object>());
        }
        ArrayList<Object> features = _data.get(platformIndex);
        features.ensureCapacity(featureIndex+1);
        features.set(featureIndex, value);
    }

    


}
