package net.ninjacat.semblance.data.callables;

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

}
