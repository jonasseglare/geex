package geex;

import geex.Optional;

public class SeedState {

    /*

      Three possible compilation states:
       - Not listed, the code is inserted as is.
          - Pure expressions
          - Code expressions
       - Listed, but not bound.
          - Side-effectful *statements*
       - Listed, and bound to a variable:
          - Pure expressions shared by many
          - Side-effectful expressions


       Rules:
          Always set compilation result
          if explicit listing true or false, then let that decide
          otherwise:
            SideEffectful or (Many referents && not code) => list
          list && hasValue => bind
          
     */


    /*

      Setters
      
     */
    public void setCompilationResult(Object v) {
        _value = Optional.of(v);
    }

    public void list() {
        _listed = true;
    }

    public void bind(Object k) {
        _key = Optional.of(k);
    }



    /*

      Getters

     */
    public boolean hasCompilationResult() {
        return _value.hasValue();
    }

    void checkHasCompilationResult() {
        if (!hasCompilationResult()) {
            throw new RuntimeException("No compilation result");
        }
    }

    public Object getCompilationResult() {
        checkHasCompilationResult();
        return _key.hasValue()? _key.get() : _value.get();
    }

    public Object getKey() {
        return _key.get();
    }

    public Object getValue() {
        return _value.get();
    }

    public boolean isListed() {
        return _listed;
    }

    private boolean _listed = false;
    private Optional<Object> _key = Optional.empty();
    private Optional<Object> _value = Optional.empty();
};
