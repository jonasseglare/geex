package geex;

public class StateSettings {
    public PlatformFunctions platformFunctions = null;

    void check() {
        if (platformFunctions == null) {
            throw new RuntimeException("No platform functions");
        }
    }
}
