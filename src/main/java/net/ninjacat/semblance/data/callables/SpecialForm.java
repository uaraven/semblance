package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.SList;

/**
 * Created on 28/02/15.
 */
public abstract class SpecialForm extends ParametrizableCallable {

    /**
     * Creates a new special form.
     *
     * @param definition Special form definition. Used for documentation purposes.
     */
    protected SpecialForm(final SList definition) {
        super(definition);
    }

}
