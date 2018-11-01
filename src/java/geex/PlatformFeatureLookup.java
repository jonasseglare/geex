package geex;

import java.util.ArrayList;

public class PlatformFeatureLookup {
    ArrayList<ArrayList<Object>> _data = new ArrayList<ArrayList<Object>>();

    public void setFeature(
        int platformIndex, int featureIndex, 
        Object value) {
        if (value == null) {
            throw new RuntimeException("Value must not be null");
        }
        _data.ensureCapacity(platformIndex+1);
        if (_data.get(platformIndex) == null) {
            _data.set(platformIndex, new ArrayList<Object>());
        }
        ArrayList<Object> features = _data.get(platformIndex);
        features.ensureCapacity(featureIndex+1);
        features.set(featureIndex, value);
    }

    public Object getFeature(int platformIndex, int featureIndex) {
        if (_data == null || !(platformIndex < _data.size())) {
            throw new RuntimeException("Platform index out of bounds");
        }
        ArrayList<Object> features = _data.get(platformIndex);
        if (features == null) {
            throw new RuntimeException("No features for this platform");
        }
        if (!(featureIndex < features.size())) {
            throw new RuntimeException("Feature index out of bounds");
        }
        Object feature = features.get(featureIndex);
        if (feature == null) {
            throw new RuntimeException("No such feature on this platform");
        }
        return feature;
    }
}
