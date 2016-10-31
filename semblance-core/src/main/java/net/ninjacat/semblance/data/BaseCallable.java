package net.ninjacat.semblance.data;

import net.ninjacat.semblance.data.collections.LispValue;

import javax.annotation.Nonnull;

/**
 * Base implementation for a {@link LispCallable}.
 * <p>
 * Implements {@link LispCallable#getType()}, {@link LispCallable#repr()} and {@link LispCallable#compareTo(Object)}.
 */
public abstract class BaseCallable implements LispCallable {
    private static final long serialVersionUID = 1320843159277874259L;

    @Override
    public SemblanceType getType() {
        return SemblanceType.FUNCTION;
    }

    @Override
    public String repr() {
        return "callable";
    }

    @Override
    public int compareTo(@Nonnull final LispValue o) {
        return o == this ? 1 : 0;
    }
}
