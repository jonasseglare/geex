package geex;

import java.util.ArrayList;
import java.util.Stack;
import java.util.HashMap;
import java.util.HashSet;
import geex.ISeed;
import geex.SeedUtils;
import geex.Counter;
import java.lang.RuntimeException;
import geex.LocalVars;
import geex.LocalStruct;
import clojure.lang.Keyword;
import geex.DataIndex;
import clojure.lang.IFn;
import clojure.lang.PersistentHashMap;

public class State {

    private ArrayList<ISeed> _upperSeeds = new ArrayList<ISeed>();
    private StateSettings _settings = null;
    private ISeed _currentSeed = null;
    private LocalVars _lvars = new LocalVars();
    private HashMap<Object, LocalStruct> _localStructs 
        = new HashMap<Object, LocalStruct>();
    private int _symbolCounter = 0;
    private HashSet<Keyword> _flags = new HashSet<Keyword>();
    private DataIndex _typeIndexMap = new DataIndex();
    private HashMap<Object, Object> _varMap 
        = new HashMap<Object, Object>();
    private long _gensymCounter = 0;
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
        for (int i = 0; i < n; i++) {
            ISeed seed = lastScope.get(i);
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
        
        ISeed seed = new DynamicSeed(params);
        for (int i = 0; i < n; i++) {
            seed.deps().addDep(i, lastScope.get(i));
        }
        addSeed(seed);
        return seed;
    }
    
    public State(StateSettings s) {
        if (s == null) {
            throw new RuntimeException("No settings provided");
        }
        s.check();
        _settings = s;
        openScope();
        openScope(); // When this scope is finalized, the resulting seed needs a scope where to go.
    }

    public LocalVars getLocalVars() {
        return _lvars;
    }

    public Object getPlatform() {
        return _settings.platform;
    }

    public int getLower() {
        return 0;
    }

    public int getUpper() {
        return _upperSeeds.size();
    }

    int nextUpperIndex() {
        return _upperSeeds.size();
    }

    void checkNonEmptyScopes() {
        if (_scopes.isEmpty()) {
            throw new RuntimeException("Empty scopes");
        }
    }


    public void addSeed(ISeed x) {
        if (SeedUtils.isRegistered(x)) {
            throw new RuntimeException(
                "Cannot add seed with id "
                + x.getId() + " because it is already registered");
        }
        x.setId(nextUpperIndex());
        _upperSeeds.add(x);
        x.setForwardedFunction(_settings.forwardedFunction);
        checkNonEmptyScopes();
        ArrayList<ISeed> currentScope = _scopes.peek();
        currentScope.add(x);
    }

    public ISeed getSeed(int index) {
        if (0 <= index && index < _upperSeeds.size()) {
            return _upperSeeds.get(index);
        } else {
            throw new RuntimeException(
                "Seed index out of bounds");
        }
    }

    public int getSeedCount() {
        return _upperSeeds.size();
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
        for (int i = 0; i < upper; i++) {
            ISeed seed = getSeed(i);
            int id = seed.getId();
            seed.deps().addReferentsFromId(id);
        }
    }

    public void finalizeState() {
        closeScope();
        if (_scopes.size() != 1) {
            throw new RuntimeException(
                "After closing the scope, there should be exactly one element on the stack.");
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





    private boolean shouldList(ISeed seed) {
        Boolean b = seed.shouldBind();
        int refCount = seed.refs().count() - 1/*scope*/;
        if (b == null) {
            switch (seed.getMode()) {
            case Pure: return 2 <= refCount;
            case Ordered: return 1 <= refCount;
            case SideEffectful: return true;

                /* In general, don't list code. For instance, 
                 we don't want to list the branches of an
                 if-statement in code. But when generating
                 methods, we might want to list the code.
                */
            case Code: return false;
            }
            return true;
        } else {
            return b.booleanValue();
        }
    }
    /*
      
      Examples:
      * A conditional branch in Java:
      - hasValue = false
      - Mode = Pure (inserted where it is used)
      
      * A Java void method call
      - hasValue = false
      - Mode = SideEffectful

      * A call to recur
      - hasValue = true
      - Mode = Pure (it is the value returned).

     */

    // By bind, we mean "producing a statement in order", 
    // possibly with a symbol bound to it.
    private void compileSeed(ISeed seed) {
        Object result = seed.compile(this);
        if (_settings.checkCompilationResult != null) {
            _settings.checkCompilationResult.invoke(
                seed, result);
        }

        SeedState state = seed.getState();
        state.setCompilationResult(result);

        if (shouldList(seed)) {
            state.list();
        }
        if (seed.hasValue() && state.isListed() 
            && seed.getMode() != Mode.Code) {
            state.bind(_settings.generateSeedSymbol.invoke(seed));
        }
    }

    private static Keyword keywordDispTrace = Keyword.intern(
        "disp-trace");

    private static Keyword keywordDispCompilationResults 
        = Keyword.intern(
            "disp-compilation-results");

    public Object generateCode() {
        if (isEmpty()) {
            throw new RuntimeException(
                "Cannot generate code, because empty");
        }
        boolean dispTrace = hasFlag(keywordDispTrace);
        boolean dispCompResults = hasFlag(
            keywordDispCompilationResults);
        for (int i = getLower(); i < getUpper(); i++) {
            ISeed seed = getSeed(i);
            if (dispTrace) {
                System.out.println("Generate code for " 
                    + seed.toString());
            }
            compileSeed(seed);
            if (dispCompResults) {
                System.out.println("  Compilation result: "
                    + seed.getState().getValue().toString());
            }
        }
        ISeed last = getSeed(getUpper()-1);
        return last.getState().getValue();
    }

    public LocalVar declareLocalVar() {
        LocalVar lvar = _lvars.declare();
        if (scopedLocalVars == null) {
            throw new RuntimeException(
                "No local variable scope");
        }
        scopedLocalVars.add(lvar);
        return lvar;
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


    public ISeed getLastSeed() {
        return getSeed(_upperSeeds.size()-1);
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


    public ArrayList<LocalVar> scopedLocalVars;

    public HashMap<Object, Object> getVarMap() {
        return _varMap;
    }
}
