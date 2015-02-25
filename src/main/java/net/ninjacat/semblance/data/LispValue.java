package net.ninjacat.semblance.data;

/**
 * Basic program element which can be evaluated
 * <br>
 * All values are divided into atoms and lists
 */
public interface LispValue {
    LispValue evaluate();

    LispValue self();

    String repr();
}
