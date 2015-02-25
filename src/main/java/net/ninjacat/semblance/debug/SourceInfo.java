package net.ninjacat.semblance.debug;

/**
 * Representation of source code information.
 * <br/>
 * Used for error reporting and debugging purposes
 */
public class SourceInfo {
    public static final SourceInfo UNKNOWN = new SourceInfo(0, 0);

    private final int line;
    private final int position;

    public SourceInfo(int line, int position) {
        this.line = line;
        this.position = position;
    }

    public int getLine() {
        return line;
    }

    public int getPosition() {
        return position;
    }

    @Override
    public String toString() {
        if (UNKNOWN == this) {
            return "[unknown]";
        } else {
            return String.format("[%d, %d]", line, position);
        }
    }
}
