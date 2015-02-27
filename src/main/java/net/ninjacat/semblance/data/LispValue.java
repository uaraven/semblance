package net.ninjacat.semblance.data;

import java.io.Serializable;

/**
 * Basic program element which can be evaluated
 * <br>
 * All values are divided into atoms and lists
 */
public interface LispValue extends Serializable {
    /**
     * @return {@link net.ninjacat.semblance.data.SemblanceType} type of the value
     */
    SemblanceType getType();

    /**
     * Evaluates the value.
     * <br>
     * If value is an atom then it will return itself. If value is S-expression it will be evaluated as
     * function/macro/special form call and result of the call will be returned
     *
     * @return evaluated value
     */
    LispValue evaluate();

    /**
     * @return value itself, analogue of {@code this}
     */
    LispValue self();

    /**
     * @return syntactically correct string representation of the value
     */
    String repr();
}
