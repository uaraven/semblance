package net.ninjacat.semblance.data.special;

import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;

import javax.annotation.Nonnull;

/**
 * Wrapper around parameters for recursive call
 */
public class RecursiveCallValue implements LispValue, WrappedValue {

    private static final long serialVersionUID = 739594838026914699L;

    private final LispCollection parameters;

    /**
     * Creates new instance of recursive parameters wrapper
     *
     * @param parameters list of parameters
     */
    public RecursiveCallValue(final LispCollection parameters) {
        this.parameters = parameters;
    }

    /**
     * @return List of parameters in recursive call
     */
    @Override
    public LispCollection getValue() {
        return parameters;
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.RECURSIVE;
    }

    @Override
    public String repr() {
        return "(recur " + parameters.repr() + ")";
    }

    @Override
    public int compareTo(@Nonnull final LispValue other) {
        return equals(other) ? 0 : 1;
    }

    @Override
    public boolean equals(final Object other) {
        if (this == other) {
            return true;
        }
        if (other == null || getClass() != other.getClass()) {
            return false;
        }

        final RecursiveCallValue otherCall = (RecursiveCallValue) other;

        return parameters.equals(otherCall.parameters);

    }

    @Override
    public int hashCode() {
        return parameters.hashCode();
    }
}
