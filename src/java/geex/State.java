package geex;

import java.util.ArrayList;
import geex.Seed;
import geex.SeedUtils;
import java.lang.RuntimeException;

public class State {

    private ArrayList<Seed> _lowerSeeds = new ArrayList<Seed>();
    private ArrayList<Seed> _upperSeeds = new ArrayList<Seed>();

    int nextUpperIndex() {
        return _upperSeeds.size();
    }
    
    void addSeed(Seed x) {
        if (SeedUtils.isRegistered(x)) {
            throw new RuntimeException(
                "Cannot add seed with id "
                + x.getId() + " because it is already registered");
        }
        x.setId(nextUpperIndex());
        _upperSeeds.add(x);
    }
}
