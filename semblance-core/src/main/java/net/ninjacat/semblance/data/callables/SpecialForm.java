package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.collections.LispValue;

import javax.annotation.Nonnull;

/**
 * Abstract Special Form.
 */
@SuppressWarnings("ComparableImplementedButEqualsNotOverridden")
public abstract class SpecialForm extends ParametrizedCallable {

    private static final long serialVersionUID = -714733247319284122L;

    /**
     * Creates new instance of the special form.
     *
     * @param definition Definition of special form.
     */
    protected SpecialForm(final String... definition) {
        super(definition);
    }

    @Override
    public int compareTo(@Nonnull final LispValue other) {
        //noinspection ObjectEquality
        if (other == this) {
            return 0;
        } else {
            throw new ClassCastException(String.format("%s is not compatible with %s", getType(), other.getType()));
        }
    }
}
