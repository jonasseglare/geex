package geex;

import java.util.ArrayList;
import java.util.Stack;
import java.util.HashMap;
import java.util.HashSet;
import geex.Seed;
import geex.SeedUtils;
import geex.Counter;
import java.lang.RuntimeException;
import geex.LocalBindings;
import geex.Binding;
import geex.LocalVars;
import geex.LocalStruct;
import clojure.lang.Keyword;
import geex.CodeMap;
import geex.DataIndex;

public class State {

    private ArrayList<Seed> _lowerSeeds = new ArrayList<Seed>();
    private ArrayList<Seed> _upperSeeds = new ArrayList<Seed>();
    private Mode _maxMode = Mode.Pure;
    private Object _output = null;
    private Stack<Seed> _dependingScopes = new Stack<Seed>();
    private LocalBindings _localBindings = new LocalBindings();
    private StateSettings _settings = null;
    private Stack<Seed> _scopeStack 
        = new Stack<Seed>();
    private HashMap<Object, Seed> _seedCache 
        = new HashMap<Object, Seed>();
    private Stack<HashMap<Object, Seed>> _seedCacheStack 
        = new Stack<HashMap<Object, Seed>>();
    private Stack<Mode> _modeStack = new Stack<Mode>();
    private Seed _currentSeed = null;
    private LocalVars _lvars = new LocalVars();
    private HashMap<Object, LocalStruct> _localStructs 
        = new HashMap<Object, LocalStruct>();
    private int _symbolCounter = 0;
    private CodeMap _topCode 
        = new CodeMap();
    private HashSet<Keyword> _flags = new HashSet<Keyword>();
    private DataIndex _typeIndexMap = new DataIndex();
    
    
    
    public State(StateSettings s) {
        if (s == null) {
            throw new RuntimeException("No settings provided");
        }
        s.check();
        _settings = s;
    }

    public LocalVars getLocalVars() {
        return _lvars;
    }

    public void beginScope(Seed s, boolean isDepending) {
        _scopeStack.add(s);
        if (isDepending) {
            _dependingScopes.add(s);
        }
        _modeStack.add(_maxMode);
        _seedCacheStack.add(_seedCache);
        _seedCache = new HashMap<Object, Seed>();
        _maxMode = Mode.Pure;
    }

    public Seed popScopeId() {
        Seed beginSeed = _scopeStack.pop();
        if (!_dependingScopes.empty() && 
            beginSeed == _dependingScopes.peek()) {
            _dependingScopes.pop();
        }
        return beginSeed;
    }

    public void popScope() {
        _modeStack.pop();
        _seedCache = _seedCacheStack.pop();
    }

    public LocalBindings localBindings() {
        return _localBindings;
    }

    public Mode maxMode() {
        return _maxMode;
    }

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

    public Object getPlatform() {
        return _settings.platform;
    }

    public int getLower() {
        return -_lowerSeeds.size();
    }

    public int getUpper() {
        return _upperSeeds.size();
    }
    
    int nextLowerIndex() {
        return -_lowerSeeds.size()-1;
    }

    int nextUpperIndex() {
        return _upperSeeds.size();
    }


    public void addSeed(Seed x, boolean reverse) {
        if (SeedUtils.isRegistered(x)) {
            throw new RuntimeException(
                "Cannot add seed with id "
                + x.getId() + " because it is already registered");
        }
        x.setId(reverse? nextLowerIndex() : nextUpperIndex());
        if (!reverse) {
            _maxMode = SeedUtils.max(_maxMode, x.getMode());
        }
        (reverse? _lowerSeeds : _upperSeeds).add(x);
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
        if (!_scopeStack.empty()) {
            throw new RuntimeException(
                "_scopeStack not empty");
        }
        if (!_seedCacheStack.empty()) {
            throw new RuntimeException(
                "_seedCacheStack not empty");
        }
        if (!_modeStack.empty()) {
            throw new RuntimeException(
                "_modeStack not empty");
        }
        if (!_dependingScopes.empty()) {
            throw new RuntimeException(
                "_dependingScopes not empty");
        }
        buildReferents();
    }

    public void disp() {
        System.out.println("=== State ===");
        int lower = getLower();
        int upper = getUpper();
        for (int i = lower; i < upper; i++) {
            Seed seed = getSeed(i);
            System.out.println(
                String.format(
                    " - %4d %s",
                    i, seed.toString()));
            seed.deps().disp();
            seed.refs().disp();
        }
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

    private boolean shouldBindResult(Seed seed) {
        Boolean b = seed.shouldBind();
        int refCount = seed.refs().count();
        if (b == null) {
            if (SeedFunction.Begin == seed.getSeedFunction()) {
                return false;
            } else {
                switch (seed.getMode()) {
                case Pure: return 2 <= refCount;
                case Ordered: return 1 <= refCount;
                case SideEffectful: return true;
                case Statement: return true;
                }
                return true;
            }
        } else {
            return b.booleanValue();
        }
    }

    private void bind(Seed seed) {
        if (!SeedUtils.hasCompilationResult(seed)) {
            throw new RuntimeException(
                "Cannot bind a seed before it has a result (seed "
                + seed.toString() + ")");
        }

        Object result = seed.getCompilationResult();
        Binding b = _localBindings.addBinding(seed);
        b.isStatement = seed.getMode() == Mode.Statement;
        seed.setCompilationResult(
            _settings
            .platformFunctions
            .renderLocalVarName(b.varName));
    }

    private void maybeBind(Seed seed) {
        if (shouldBindResult(seed)) {
            bind(seed);
        }
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
                            "No compilation result set for seed "
                            + seed.toString());
                    }
                    maybeBind(seed);
                    wasCalled.step();
                    Object result = seed.getCompilationResult();
                    if (result instanceof Seed) {
                        throw new RuntimeException(
                            "The result of '" + seed 
                            + "' is a seed'");
                    }

                    if (seed.getSeedFunction() == SeedFunction.End) {
                        return result;
                    }
                    return generateCodeFrom(
                        result,
                        index+1);
                }
            };

        _currentSeed = seed; // Hacky
        Object result = seed.compile(this, wrapCallback(
                innerCallback));
        _currentSeed = null;
        if (wasCalled.get() == 0) {
            throw new RuntimeException(
                "Callback never called when compiling seed "
                + seed.toString());
        }

        if (seed.getSeedFunction() == SeedFunction.Begin) {
            Object endSeed0 = seed.getData();
            if (!(endSeed0 instanceof Seed)) {
                throw new RuntimeException(
                    "The begin seed does not have a valid end seed");
            }
            Seed endSeed = (Seed)endSeed0;
            endSeed.setCompilationResult(result);
            maybeBind(endSeed);
            return generateCodeFrom(
                result,
                endSeed.getId());
        } else {
            return result;
        }
    }

    // Just there for backward compatibility
    public void setCompilationResult(Object o) {
        if (_currentSeed == null) {
            throw new RuntimeException("No seed being compiled");
        }
        _currentSeed.setCompilationResult(o);
    }

    // Just there for backward compatibility
    public Object getCompilationResult() {
        if (_currentSeed == null) {
            throw new RuntimeException("No seed being compiled");
        }
        return _currentSeed;
    }

    public Object generateCode() {
        return generateCodeFrom(null, getLower());
    }

    public LocalVar declareLocalVar() {
        return _lvars.declare();
    }

    public LocalVar[] declareLocalVars(int n) {
        LocalVar[] dst = new LocalVar[n];
        for (int i = 0; i < n; i++) {
            dst[i] = declareLocalVar();
        }
        return dst;
    }

    public LocalStruct allocateLocalStruct(
        Object key, Object tpSig, LocalVar[] vars) {
        if (_localStructs.containsKey(key)) {
            throw new RuntimeException(
                "Struct with key " + key.toString()  
                + " already declared");
        }
        LocalStruct ls = new LocalStruct(tpSig, vars);
        _localStructs.put(
            key, ls);
        return ls;
    }

    public LocalStruct getLocalStruct(Object key) {
        return _localStructs.get(key);
    }

    public int generateSymbolIndex() {
        return _symbolCounter++;
    }

    public void addTopCode(CodeItem code) {
        _topCode.add(code);
    }

    public Seed getLastSeed() {
        return getSeed(_upperSeeds.size()-1);
    }

    public CodeMap getTopCode() {
        return _topCode;
    }
    
    public void setFlag(clojure.lang.Keyword flag) {
        _flags.add(flag);
    }

    public boolean hasFlag(clojure.lang.Keyword flag) {
        return _flags.contains(flag);
    }

    public int getTypeIndex(Object x) {
        return _typeIndexMap.get(x);
    }
}
