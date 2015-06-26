package net.ninjacat.semblance.java;

/**
 * This interface should be implemented by Semblance values which can be converted to Java objects.
 */
public interface JavaConvertible {
    /**
     * @return Representation of semblance value as java value.
     */
    Object asJavaObject();
}
