package geex;

public class StateSettings {
    public PlatformFunctions platformFunctions = null;
    public Object platform = null;

    void check() {
        if (platformFunctions == null) {
            throw new RuntimeException("No platform functions");
        }
        if (platform == null) {
            throw new RuntimeException("No platform specified");
        }
    }
}
