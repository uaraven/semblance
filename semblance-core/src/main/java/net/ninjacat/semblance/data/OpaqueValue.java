package net.ninjacat.semblance.data;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.java.JavaConvertible;

import javax.annotation.Nonnull;

/**
 * Value that has no semblance interpretation. Can be used to store data
 */
public class OpaqueValue<T> implements LispValue, JavaConvertible {

    private final T value;

    public OpaqueValue(@Nonnull final T value) {
        this.value = value;
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.OPAQUE;
    }

    @Override
    public String repr() {
        return value.toString();
    }

    /**
     * @return internal value
     */
    public T getValue() {
        return value;
    }

    @Override
    public int compareTo(@Nonnull final LispValue other) {
        if (other.getType() == getType()) {
            return value.equals(((OpaqueValue) other).getValue()) ? 0 : 1;
        }
        return 0;
    }

    @Override
    public Object asJavaObject() {
        return value;
    }
}
