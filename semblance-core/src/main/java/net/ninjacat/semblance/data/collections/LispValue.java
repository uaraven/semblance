package net.ninjacat.semblance.data.collections;

import net.ninjacat.semblance.data.SemblanceType;

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

}
