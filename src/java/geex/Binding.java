package geex;

public class Binding {
    public String varName;
    public Object type;
    public Object value;
    public boolean isStatement = false;

    Binding(String varName_, Object type_, Object value_) {
        varName = varName_;
        type = type_;
        value = value_;
    }
}
