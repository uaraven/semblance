package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.SList;

/**
 * Abstract Special Form.
 */
public abstract class SpecialForm extends ParametrizableCallable {

    /**
     * Creates new instance of the special form.
     *
     * @param definition Definition of special form.
     */
    protected SpecialForm(final String... definition) {
        super(definition);
    }

    /**
     * Creates a new special form.
     *
     * @param definition Special form definition. Used for documentation purposes.
     */
    protected SpecialForm(final SList definition) {
        super(definition);
    }

}
