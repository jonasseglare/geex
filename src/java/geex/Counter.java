package geex;

public class Counter {
    private int _value = 0;
    
    public int generate() {
        return _value++;
    }

    public void step() {
        _value++;
    }

    public int get() {
        return _value;
    }
}
