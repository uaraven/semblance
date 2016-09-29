package net.ninjacat.semblance.debug;

import java.io.Serializable;

/**
 * Representation of source code information.
 * <br/>
 * Used for error reporting and debugging purposes
 */
public class SourceInfo implements Serializable {
    public static final SourceInfo UNKNOWN = new SourceInfo(0, 0);
    private static final long serialVersionUID = -5509266557132831611L;
    private final int line;
    private final int position;

    /**
     * Creates new instance.
     *
     * @param line     Line number.
     * @param position Position in line.
     */
    public SourceInfo(final int line, final int position) {
        this.line = line;
        this.position = position;
    }

    /**
     * @return Line number.
     */
    public int getLine() {
        return line;
    }

    /**
     * @return Position in line
     */
    public int getPosition() {
        return position;
    }

    @Override
    public String toString() {
        if (UNKNOWN.equals(this)) {
            return "[unknown]";
        } else {
            return String.format("[%d, %d]", line, position);
        }
    }

    @SuppressWarnings("all")
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        SourceInfo that = (SourceInfo) o;

        if (line != that.line) return false;
        if (position != that.position) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = line;
        result = 31 * result + position;
        return result;
    }
}
