package geex;

public class Binding {
    public Object varName;
    public Object type;
    public Object value;

    Binding(String varName_, Object type_, Object value_) {
        varName = varName_;
        type = type_;
        value = value_;
    }
}
