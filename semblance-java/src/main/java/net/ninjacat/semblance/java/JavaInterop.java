package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.collections.LispValue;

/**
 * Utilities for Java-Semblance interoperability
 */
public final class JavaInterop {

    private JavaInterop() {
    }

    /**
     * Converts java object into Semblance {@link LispValue}
     *
     * @param javaObject Object to convert
     * @return LispValue with wrapped java object
     */
    public static LispValue toLisp(final Object javaObject) {
        return new JavaWrapperValue(javaObject);
    }
}
