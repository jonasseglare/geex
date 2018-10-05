package geex;

import java.util.ArrayList;
import geex.Seed;
import geex.SeedUtils;
import java.lang.RuntimeException;

public class State {

    private ArrayList<Seed> _lowerSeeds = new ArrayList<Seed>();
    private ArrayList<Seed> _upperSeeds = new ArrayList<Seed>();
    private Mode _maxMode = Mode.Pure;
    private Object _output = null;
    private Object _platform = null;
    ArrayList<Seed> _dependingScopes = new ArrayList<Seed>();

    public void setPlatform(Object p) {
        _platform = p;
    }

    public Object getPlatform() {
        return _platform;
    }

    public int getLower() {
        return -_lowerSeeds.size();
    }

    public int getUpper() {
        return _upperSeeds.size();
    }
    
    int nextLowerIndex() {
        return _lowerSeeds.size()-1;
    }

    int nextUpperIndex() {
        return _upperSeeds.size();
    }


    public void addSeed(Seed x) {
        if (SeedUtils.isRegistered(x)) {
            throw new RuntimeException(
                "Cannot add seed with id "
                + x.getId() + " because it is already registered");
        }
        x.setId(nextUpperIndex());
        _maxMode = SeedUtils.max(_maxMode, x.getMode());
        _upperSeeds.add(x);
    }

    public Seed getSeed(int index) {
        if (0 <= index) {
            return _upperSeeds.get(index);
        }
        return _lowerSeeds.get(-index-1);
    }

    public void setOutput(Object o) {
        _output = o;
    }

    public Object getOutput() {
        return _output;
    }

    public void addDependenciesFromDependingScopes(Seed dst) {
        for (int i = 0; i < _dependingScopes.size(); i++) {
            Seed from = _dependingScopes.get(i);
            if (from.getId() > dst.getId()) {
                from.deps().addGenKey(dst);
            }
        }
    }

    public int getSeedCount() {
        return _upperSeeds.size() + _lowerSeeds.size();
    }

  
    /*build-referents
  build-ids-to-visit
  check-referent-visibility
  check-scope-stacks*/

    private void buildReferents() {
        int lower = getLower();
        int upper = getUpper();
        for (int i = lower; i < upper; i++) {
            Seed seed = getSeed(i);
            int id = seed.getId();
            seed.deps().addReferentsFromId(id);
        }
    }

    public void finalizeState() {
        buildReferents();
    }
}
