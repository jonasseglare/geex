package geex;

import java.util.ArrayList;
import java.util.Stack;
import java.util.HashMap;
import java.util.HashSet;
import geex.ISeed;
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
import clojure.lang.IFn;
import clojure.lang.PersistentHashMap;

public class State {

    private ArrayList<ISeed> _lowerSeeds = new ArrayList<ISeed>();
    private ArrayList<ISeed> _upperSeeds = new ArrayList<ISeed>();
    private LocalBindings _localBindings = new LocalBindings();
    private StateSettings _settings = null;
    private ISeed _currentSeed = null;
    private LocalVars _lvars = new LocalVars();
    private HashMap<Object, LocalStruct> _localStructs 
        = new HashMap<Object, LocalStruct>();
    private int _symbolCounter = 0;
    private CodeMap _topCode 
        = new CodeMap();
    private HashSet<Keyword> _flags = new HashSet<Keyword>();
    private DataIndex _typeIndexMap = new DataIndex();
    private ISeed _localVarSection = null;
    private HashMap<Object, Object> _varMap 
        = new HashMap<Object, Object>();
    private long _gensymCounter = 0;
    private IFn _seedCall;
    private Stack<ArrayList<ISeed>> _scopes 
        = new Stack<ArrayList<ISeed>>();
    private ISeed _lastOrdered = null;
    

    public void openScope() {
        _scopes.push(new ArrayList<ISeed>());
    }

    public ISeed closeScope() {
        ArrayList<ISeed> lastScope = _scopes.pop();
        Mode maxMode = Mode.Pure;
        boolean hasValue = false;
        Object type = null;
        int n = lastScope.size();
        Object[] deps = new Object[2*n];
        for (int i = 0; i < n; i++) {
            ISeed seed = lastScope.get(i);
            int at = 2*i;
            deps[at + 0] = i;
            deps[at + 1] = seed;
            maxMode = SeedUtils.max(
                maxMode, seed.getMode());
        }
        if (!lastScope.isEmpty()) {
            ISeed last = lastScope.get(lastScope.size()-1);
            hasValue = last.hasValue();
            type = last.getType();
        }
        SeedParameters params = new SeedParameters();
        params.type = type;
        params.hasValue = hasValue;
        params.mode = maxMode;
        params.description = "Closed scope";
        params.compiler = _settings.closeScope;
        params.rawDeps = PersistentHashMap.create(deps);
        
        ISeed seed = new DynamicSeed(params);
        addSeed(seed, false);
        return seed;
    }
    
    public State(StateSettings s) {
        if (s == null) {
            throw new RuntimeException("No settings provided");
        }
        s.check();
        _settings = s;
        openScope();
    }

    public LocalVars getLocalVars() {
        return _lvars;
    }

    public LocalBindings localBindings() {
        return _localBindings;
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

    public void addSeed(ISeed x, boolean reverse) {
        if (SeedUtils.isRegistered(x)) {
            throw new RuntimeException(
                "Cannot add seed with id "
                + x.getId() + " because it is already registered");
        }
        x.setId(reverse? nextLowerIndex() : nextUpperIndex());
        (reverse? _lowerSeeds : _upperSeeds).add(x);
        x.setForwardedFunction(_settings.forwardedFunction);
    }

    public ISeed getSeed(int index) {
        if (0 <= index) {
            return _upperSeeds.get(index);
        }
        return _lowerSeeds.get(-index-1);
    }

    public int getSeedCount() {
        return _upperSeeds.size() + _lowerSeeds.size();
    }

    public boolean isEmpty() {
        return getSeedCount() == 0;
    }

  
    /*build-referents
  build-ids-to-visit
  check-referent-visibility
  check-scope-stacks*/

    private void buildReferents() {
        int lower = getLower();
        int upper = getUpper();
        for (int i = lower; i < upper; i++) {
            ISeed seed = getSeed(i);
            int id = seed.getId();
            seed.deps().addReferentsFromId(id);
        }
    }

    public void finalizeState() {
        closeScope();
        if (!_scopes.empty()) {
            throw new RuntimeException("Scopes is not empty");
        }
        buildReferents();
    }

    public void disp() {
        System.out.println("=== State ===");
        int lower = getLower();
        int upper = getUpper();
        for (int i = lower; i < upper; i++) {
            ISeed seed = getSeed(i);
            System.out.println(
                String.format(
                    " - %4d %s",
                    i, seed.toString()));
            seed.deps().disp();
            seed.refs().disp();
        }
    }

    private ISeed advanceToNextSeed(int index) {
        while (index < getUpper()) {
            ISeed seed = getSeed(index);
            if (seed != null) {
                return seed;
            }
        }        
        return null;
    }

    /*private boolean shouldBindResult(ISeed seed) {
        Boolean b = seed.shouldBind();
        int refCount = seed.refs().count();
        if (b == null) {
            switch (seed.getMode()) {
            case Pure: return 2 <= refCount;
            case Ordered: return 1 <= refCount;
            case SideEffectful: return true;
            }
            return true;
        } else {
            return b.booleanValue();
        }
        }*/

    /*private void maybeBind(ISeed seed) {
        if (shouldBindResult(seed)) {
            bind(seed);
        }
        }*/

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
        if (isEmpty()) {
            throw new RuntimeException(
                "Cannot generate code, because empty");
        }
        for (int i = getLower(); i < getUpper(); i++) {
            ISeed seed = getSeed(i);
            System.out.println("Generate code for " 
                + seed.toString());
            seed.setCompilationResult(seed.compile(this));
        }
        ISeed last = getSeed(getUpper()-1);
        return last.getCompilationResult();
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

    public ISeed getLastSeed() {
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

    public ISeed getLocalVarSection() {
        return _localVarSection;
    }

    public void setLocalVarSection(ISeed vs) {
        _localVarSection = vs;
    }

    public HashMap<Object, Object> getVarMap() {
        return _varMap;
    }
}
