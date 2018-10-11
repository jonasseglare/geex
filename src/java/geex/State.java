package geex;

import java.util.ArrayList;
import geex.Seed;
import geex.SeedUtils;
import geex.Counter;
import java.lang.RuntimeException;
import geex.LocalVars;

public class State {

    private ArrayList<Seed> _lowerSeeds = new ArrayList<Seed>();
    private ArrayList<Seed> _upperSeeds = new ArrayList<Seed>();
    private Mode _maxMode = Mode.Pure;
    private Object _output = null;
    private Object _platform = null;
    ArrayList<Seed> _dependingScopes = new ArrayList<Seed>();
    LocalVars _lvars = new LocalVars();

    private class StateCallbackWrapper extends AFn {
        private StateCallback _cb;
        
        public StateCallbackWrapper(StateCallback cb) {
            if (cb == null) {
                throw new RuntimeException(
                    "StateCallback cannot be null");
            }
            _cb = cb;
        }
        
        public Object invoke(Object x) {
            if (x == null) {
                throw new RuntimeException("StateCallbackWrapper got a null pointer");
            }
            if (!(x instanceof State)) {
                throw new RuntimeException(
                    "StateCallbackWrapper did not receive a valid state" + x.toString());
            }
            return _cb.call((State)x);
        }
    }

    private StateCallbackWrapper wrapCallback(StateCallback cb) {
        return new StateCallbackWrapper(cb);
    }


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

    private Seed advanceToNextSeed(int index) {
        while (index < getUpper()) {
            Seed seed = getSeed(index);
            if (seed != null) {
                return seed;
            }
        }        
        return null;
    }

    private Object generateCodeFrom(
        Object lastResult, int index) {
        Seed seed = advanceToNextSeed(index);
        if (seed == null) {
            return lastResult;
        } else if (SeedUtils.hasCompilationResult(seed)) {
            return generateCodeFrom(
                seed.getCompilationResult(),
                index+1);
        }
        
        final Counter wasCalled = new Counter();
        StateCallback innerCallback = new StateCallback() {
                public Object call(State state) {
                    if (!SeedUtils.hasCompilationResult(seed)) {
                        throw new RuntimeException(
                            "No compilation result set for seed"
                            + seed.toString());
                    }
                    wasCalled.step();
                    Object result = seed.getCompilationResult();
                    if (result instanceof Seed) {
                        throw new RuntimeException(
                            "The result of '" + seed 
                            + "' is a seed'");
                    }
                    System.out.println("inner result is=" + result);

                    if (seed.getSeedFunction() == SeedFunction.End) {
                        return result;
                    }
                    return generateCodeFrom(
                        result,
                        index+1);
                }
            };

        Object result = seed.compile(this, wrapCallback(
                innerCallback));

        System.out.println("Result of seed " + seed + " is " + result);

        if (wasCalled.get() == 0) {
            throw new RuntimeException(
                "Callback never called when compiling seed "
                + seed.toString());
        }

        return result;
    }

    public Object generateCode() {
        return generateCodeFrom(null, getLower());
    }
}
