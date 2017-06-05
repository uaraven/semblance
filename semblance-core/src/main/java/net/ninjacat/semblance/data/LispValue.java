package net.ninjacat.semblance.data;

import java.io.Serializable;

/**
 * Basic program element which can be evaluated
 * <br>
 * All values are divided into atoms and lists
 */
public interface LispValue extends Serializable, Comparable<LispValue> {
    /**
     * @return {@link SemblanceType} type of the value
     */
    SemblanceType getType();

    /**
     * @return syntactically correct string representation of the value
     */
    String repr();

    /**
     * @return print-friendly representation of the value
     */
    String printIt();

}
