package geex;


// Can also have null as a value
public class Optional<T> {
    private boolean _hasValue = false;
    public T _value = null;

    public Optional(T x) {
        _hasValue = true;
        _value = x;
    }

    public Optional() {}

    public boolean hasValue() {
        return _hasValue;
    }

    public boolean isPresent() {
        return _hasValue;
    }
    
    public T get() {
        if (!_hasValue) {
            throw new RuntimeException("No value present");
        }
        return _value;
    }

    public static<T> Optional<T> of(T value) {
        return new Optional<T>(value);
    }

    public static<T> Optional<T> empty() {
        return new Optional<T>();
    }
}
