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
    MAP, SPECIAL // this is internal type, it will never appear in programs
}
