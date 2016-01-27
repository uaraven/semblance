package net.ninjacat.semblance.data;

/**
 * Value types supported by Semblance
 */
public enum SemblanceType {
    INTEGER,
    FLOATIG_POINT,
    STRING,
    SYMBOL,
    LIST,
    VECTOR,
    FUNCTION,
    MACRO,
    RETURN, // internal wrapper around return value
    MAP,
    BREAK,
    RECURSIVE,
    OPAQUE,
    SPECIAL; // this are internal types, they will never appear in programs

    /**
     * @return {@code true} if this type should break block execution
     */
    public boolean isBreak() {
        return this == BREAK || this == RECURSIVE;
    }

}
